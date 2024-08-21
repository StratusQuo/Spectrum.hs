{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Spectrum.Capabilities (
  ColorLevel(..),
  ColorLevelInfo(..),
  NumericVersion(..),
  Config(..),
  parseNumericVersion,
  envForceLevel,
  inEnv,
  hasEnvMatching,
  windowsLevel,
  teamcityLevel,
  isRecognisedTermProgram,
  itermLevel,
  termProgramLevel,
  termIs256Color,
  termIs16Color,
  isInCI,
  supportedColorLevel,
  supportedColorLevels,
  defaultConfig,
  envProviderOfMap,
  osInfoProviderOfValues
) where

import Text.Regex.TDFA ((=~~), (=~))
import Data.Maybe (isJust, fromMaybe, isNothing)
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv)
--import qualified System.Environment as SystemEnv
import System.Info (os)
import System.Process (readProcess)
import Text.Read (readMaybe)  -- Import readMaybe from Text.Read

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
  deriving (Show, Eq, Ord)

-- | Environment provider interface
class EnvProvider e where
  getEnvOpt :: e -> String -> IO (Maybe String)

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
parseNumericVersion s = do
  (_, _, _, captures) <- s =~~ "(?P<major>\\d+)\\.(?P<minor>\\d+)\\.(?P<patch>\\d+)" :: Maybe (String, String, String, [String])
  major <- readMaybe (head captures) 
  minor <- readMaybe (captures !! 1) -- Or (head $ tail captures)
  patch <- readMaybe (captures !! 2) -- Or (head $ tail $ tail captures)
  return $ NumericVersion { major, minor, patch }

-- | Get the forced color level from environment
envForceLevel :: EnvProvider e => e -> IO (Maybe ColorLevel)
envForceLevel e = do
  forceColor <- getEnvOpt e "FORCE_COLOR"
  return $ case forceColor of
    Just "true" -> Just Basic
    Just "false" -> Just Unsupported
    Just s -> case (reads s :: [(Int, String)]) of
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
  osWVersion <- osVersion o
  return $ case osWVersion >>= parseNumericVersion of
    Just v | major v == 10 && minor v == 0 && patch v >= 14931 -> TrueColor
           | major v == 10 && minor v == 0 && patch v >= 10586 -> EightBit
           | major v == 10 && minor v > 0 -> TrueColor
           | major v > 10 -> TrueColor
    _ -> Basic

-- | Get the color level for TeamCity
teamcityLevel :: EnvProvider e => e -> IO ColorLevel
teamcityLevel e = do
  teamcityVersion <- getEnvOpt e "TEAMCITY_VERSION"
  return $ case teamcityVersion of
    Just s ->
      if s =~ "^(9\\.(0*[1-9]\\d*)\\.|\\d{2,}\\.)$" :: Bool
      then Basic
      else Unsupported
    Nothing -> Unsupported

-- | Check if the terminal program is recognized
isRecognisedTermProgram :: EnvProvider e => e -> IO Bool
isRecognisedTermProgram e = do
  termProgram <- getEnvOpt e "TERM_PROGRAM"
  return $ termProgram `elem` [Just "iTerm.app", Just "Apple_Terminal"]

-- | Get the color level for iTerm
itermLevel :: EnvProvider e => e -> IO ColorLevel
itermLevel e = do
  termProgramVersion <- getEnvOpt e "TERM_PROGRAM_VERSION"
  return $ case termProgramVersion >>= parseNumericVersion of
    Just v | major v >= 3 -> TrueColor
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
termIs256Color term = term =~ "(?i)-256(color)?$" :: Bool

-- | Check if the terminal supports 16 colors
termIs16Color :: String -> Bool
termIs16Color term = term =~ "(?i)^screen|^xterm|^vt100|^vt220|^rxvt|color|ansi|cygwin|linux" :: Bool

-- | List of CI environment variables to check
ciEnvVars :: [String]
ciEnvVars = ["TRAVIS", "CIRCLECI", "APPVEYOR", "GITLAB_CI", "GITHUB_ACTIONS", "BUILDKITE", "DRONE"]

-- | Check if running in a CI environment
isInCI :: EnvProvider e => e -> IO Bool
isInCI e = or <$> sequence (map (inEnv e) ciEnvVars ++ [hasEnvMatching e "CI_NAME" "codeship"])

-- | Get the supported color level
supportedColorLevel :: Config -> Bool -> IO ColorLevel
supportedColorLevel (Config env osInfo) isTTY = do
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
    isWin <- isWindows osInfo

    if dumbTerm then return minLevel
    else if isWin then windowsLevel osInfo
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
  --getEnv _ = SystemEnv.getEnv

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
  -- getEnv (MapEnv m) k = maybe (fail $ "Environment variable not found: " ++ k) return $ Map.lookup k m

-- | Create an OS info provider with custom values
osInfoProviderOfValues :: Bool -> Maybe String -> Config
osInfoProviderOfValues isWin osVer = Config (SysEnv ()) (CustomOSInfo isWin osVer)

data CustomOSInfo = CustomOSInfo Bool (Maybe String)

instance OSInfoProvider CustomOSInfo where
  isWindows (CustomOSInfo isWin _) = return isWin
  osVersion (CustomOSInfo _ osVer) = return osVer