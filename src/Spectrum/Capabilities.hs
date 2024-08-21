{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Spectrum.Capabilities where

import Text.Regex.PCRE
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv, getEnv)
import System.Info (os)
import System.Process (readProcess)

-- | Represents the level of color support
data ColorLevel = Unsupported
                | Basic
                | EightBit
                | TrueColor
  deriving (Show, Eq)

-- | Information about color support for stdout and stderr
data ColorLevelInfo = ColorLevelInfo
  { stdoutLevel :: ColorLevel
  , stderrLevel :: ColorLevel
  }
  deriving (Show)

-- | Represents a numeric version
data NumericVersion = NumericVersion
  { major :: Int
  , minor :: Int
  , patch :: Int
  }

-- | Environment provider interface
class EnvProvider e where
  getEnvOpt :: e -> String -> IO (Maybe String)
  getEnv :: e -> String -> IO String

-- | OS information provider interface
class OSInfoProvider o where
  isWindows :: o -> IO Bool
  osVersion :: o -> IO (Maybe String)

-- | Capabilities configuration
data Config = forall e o. (EnvProvider e, OSInfoProvider o) => Config
  { envProvider :: e
  , osInfoProvider :: o
  }

-- | Parse a version string into a NumericVersion
parseNumericVersion :: String -> Maybe NumericVersion
parseNumericVersion s =
  case matchRegex (mkRegex "(?P<major>\\d+)\\.(?P<minor>\\d+)\\.(?P<patch>\\d+)") s of
    Just [major, minor, patch] ->
      Just $ NumericVersion { major = read major, minor = read minor, patch = read patch }
    _ -> Nothing

-- | Get the forced color level from environment
envForceLevel :: EnvProvider e => e -> IO (Maybe ColorLevel)
envForceLevel e = do
  forceColor <- getEnvOpt e "FORCE_COLOR"
  return $ case forceColor of
    Just "true" -> Just Basic
    Just "false" -> Just Unsupported
    Just s -> case reads s of
      [(0, "")] -> Just Unsupported
      [(1, "")] -> Just Basic
      [(2, "")] -> Just EightBit
      [(3, "")] -> Just TrueColor
      _ -> Nothing
    Nothing -> Nothing

-- | Check if an environment variable exists
inEnv :: EnvProvider e => e -> String -> IO Bool
inEnv e name = isJust <$> getEnvOpt e name

-- | Check if an environment variable matches a specific value
hasEnvMatching :: EnvProvider e => e -> String -> String -> IO Bool
hasEnvMatching e name value = do
  envVal <- getEnvOpt e name
  return $ envVal == Just value

-- | Get the color level for Windows
windowsLevel :: OSInfoProvider o => o -> IO ColorLevel
windowsLevel o = do
  osVersion <- osVersion o
  return $ case osVersion >>= parseNumericVersion of
    Just v | v.major == 10 && v.minor == 0 && v.patch >= 14931 -> TrueColor
           | v.major == 10 && v.minor == 0 && v.patch >= 10586 -> EightBit
           | v.major == 10 && v.minor > 0 -> TrueColor
           | v.major > 10 -> TrueColor
    _ -> Basic

-- | Get the color level for TeamCity
teamcityLevel :: EnvProvider e => e -> IO ColorLevel
teamcityLevel e = do
  teamcityVersion <- getEnvOpt e "TEAMCITY_VERSION"
  return $ case teamcityVersion of
    Just s ->
      if matchRegex (mkRegex "^(9\\.(0*[1-9]\\d*)\\.|\\d{2,}\\.)") s == Just []
      then Basic
      else Unsupported
    Nothing -> Unsupported

-- | Check if the terminal program is recognized
isRecognisedTermProgram :: EnvProvider e => e -> IO Bool
isRecognisedTermProgram e = do
  termProgram <- getEnvOpt e "TERM_PROGRAM"
  return $ termProgram `elem` ["iTerm.app", "Apple_Terminal"]

-- | Get the color level for iTerm
itermLevel :: EnvProvider e => e -> IO ColorLevel
itermLevel e = do
  termProgramVersion <- getEnvOpt e "TERM_PROGRAM_VERSION"
  return $ case termProgramVersion >>= parseNumericVersion of
    Just v | v.major >= 3 -> TrueColor
    _ -> EightBit

-- | Get the color level based on the terminal program
termProgramLevel :: EnvProvider e => e -> IO ColorLevel
termProgramLevel e = do
  termProgram <- getEnvOpt e "TERM_PROGRAM"
  case termProgram of
    Just "iTerm.app" -> itermLevel e
    Just "Apple_Terminal" -> return EightBit
    _ -> return Unsupported

-- | Check if the terminal supports 256 colors
termIs256Color :: String -> Bool
termIs256Color term = isJust $ matchRegex (mkRegex "(?i)-256(color)?$") term

-- | Check if the terminal supports 16 colors
termIs16Color :: String -> Bool
termIs16Color term = isJust $ matchRegex (mkRegex "(?i)^screen|^xterm|^vt100|^vt220|^rxvt|color|ansi|cygwin|linux") term

-- | List of CI environment variables to check
ciEnvVars :: [String]
ciEnvVars = ["TRAVIS", "CIRCLECI", "APPVEYOR", "GITLAB_CI", "GITHUB_ACTIONS", "BUILDKITE", "DRONE"]

-- | Check if running in a CI environment
isInCI :: EnvProvider e => e -> IO Bool
isInCI e = or <$> sequence (map (inEnv e) ciEnvVars ++ [hasEnvMatching e "CI_NAME" "codeship"])

-- | Get the supported color level
supportedColorLevel :: Config -> Bool -> IO ColorLevel
supportedColorLevel (Config env os) isTTY = do
  forceLevel <- envForceLevel env
  let minLevel = fromMaybe Unsupported forceLevel

  if not isTTY && isNothing forceLevel
  then return Unsupported
  else do
    dumbTerm <- hasEnvMatching env "TERM" "dumb"
    ciEnv <- isInCI env
    teamcityEnv <- inEnv env "TEAMCITY_VERSION"
    tfBuildEnv <- inEnv env "TF_BUILD"
    agentNameEnv <- inEnv env "AGENT_NAME"
    colortermTruecolor <- hasEnvMatching env "COLORTERM" "truecolor"
    recognisedTermProgram <- isRecognisedTermProgram env
    colortermEnv <- inEnv env "COLORTERM"
    isWin <- isWindows os

    if dumbTerm then return minLevel
    else if isWin then windowsLevel os
    else if ciEnv then return Basic
    else if teamcityEnv then teamcityLevel env
    else if tfBuildEnv && agentNameEnv then return Basic
    else if colortermTruecolor then return TrueColor
    else if recognisedTermProgram then termProgramLevel env
    else do
      term <- getEnvOpt env "TERM"
      case term of
        Just t ->
          if termIs256Color t then return EightBit
          else if termIs16Color t || colortermEnv then return Basic
          else return minLevel
        Nothing -> return minLevel

-- | Get the supported color levels for stdout and stderr
supportedColorLevels :: Config -> IO ColorLevelInfo
supportedColorLevels config = do
  stdoutLevel <- supportedColorLevel config True -- Assuming stdout is TTY, adjust if needed
  stderrLevel <- supportedColorLevel config True -- Assuming stderr is TTY, adjust if needed
  return ColorLevelInfo {..}

-- | System environment provider
newtype SysEnv = SysEnv ()

instance EnvProvider SysEnv where
  getEnvOpt _ = lookupEnv
  getEnv _ = getEnv

-- | System OS information provider
newtype SysOSInfo = SysOSInfo ()

instance OSInfoProvider SysOSInfo where
  isWindows _ = return $ os == "mingw32"
  osVersion _ = case os of
    "mingw32" -> Just <$> readProcess "cmd" ["/c", "ver"] ""
    "darwin" -> Just <$> readProcess "sw_vers" ["-productVersion"] ""
    "linux" -> Just <$> readProcess "uname" ["-r"] ""
    _ -> return Nothing

-- | Default system configuration
defaultConfig :: Config
defaultConfig = Config (SysEnv ()) (SysOSInfo ())

-- Utility functions for testing

-- | Create an environment provider from a Map
envProviderOfMap :: Map.Map String String -> Config
envProviderOfMap envMap = Config (MapEnv envMap) (SysOSInfo ())

newtype MapEnv = MapEnv (Map.Map String String)

instance EnvProvider MapEnv where
  getEnvOpt (MapEnv m) k = return $ Map.lookup k m
  getEnv (MapEnv m) k = maybe (fail $ "Environment variable not found: " ++ k) return $ Map.lookup k m

-- | Create an OS info provider with custom values
osInfoProviderOfValues :: Bool -> Maybe String -> Config
osInfoProviderOfValues isWin osVer = Config (SysEnv ()) (CustomOSInfo isWin osVer)

data CustomOSInfo = CustomOSInfo Bool (Maybe String)

instance OSInfoProvider CustomOSInfo where
  isWindows (CustomOSInfo isWin _) = return isWin
  osVersion (CustomOSInfo _ osVer) = return osVer