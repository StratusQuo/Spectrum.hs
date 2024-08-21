module Spectrum.Capabilities where

import Text.Regex.PCRE
import Data.Maybe
import System.Environment
import System.Info

data ColorLevel = Unsupported
                | Basic
                | EightBit
                | TrueColor
  deriving (Show, Eq)

data ColorLevelInfo = ColorLevelInfo
  { stdoutLevel :: ColorLevel
  , stderrLevel :: ColorLevel
  }
  deriving (Show)

data NumericVersion = NumericVersion
  { major :: Int
  , minor :: Int
  , patch :: Int
  }

parseNumericVersion :: String -> NumericVersion
parseNumericVersion s =
  case matchRegex (mkRegex "(?P<major>\\d+)\\.(?P<minor>\\d+)\\.(?P<patch>\\d+)") s of
    Just [major, minor, patch] ->
      NumericVersion { major = read major, minor = read minor, patch = read patch }
    _ -> error "Invalid version string"

envForceLevel :: IO (Maybe ColorLevel)
envForceLevel = do
  forceColor <- lookupEnv "FORCE_COLOR"
  return $ case forceColor of
    Just "true" -> Just Basic
    Just "false" -> Just Unsupported
    Just s -> case reads s of
      [(0, _)] -> Just Unsupported
      [(1, _)] -> Just Basic
      [(2, _)] -> Just EightBit
      [(3, _)] -> Just TrueColor
      _ -> Nothing
    Nothing -> Nothing

inEnv :: String -> IO Bool
inEnv name = isJust <$> lookupEnv name

hasEnvMatching :: String -> String -> IO Bool
hasEnvMatching name value = do
  envVal <- lookupEnv name
  return $ envVal == Just value

windowsLevel :: IO ColorLevel
windowsLevel = do
  osVersion <- osVersionString
  return $ case osVersion of
    Just s ->
      let v = parseNumericVersion s
      in if v.major == 10 && v.minor == 0 && v.patch >= 14931
         then TrueColor
         else if v.major == 10 && v.minor == 0 && v.patch >= 10586
         then EightBit
         else if (v.major == 10 && v.minor > 0) || (v.major > 10)
         then TrueColor
         else Basic
    Nothing -> Basic

teamcityLevel :: IO ColorLevel
teamcityLevel = do
  teamcityVersion <- lookupEnv "TEAMCITY_VERSION"
  return $ case teamcityVersion of
    Just s ->
      if matchRegex (mkRegex "^(9\\.(0*[1-9]\\d*)\\.|\\d{2,}\\.)") s == Just []
      then Basic
      else Unsupported
    Nothing -> Unsupported

isRecognisedTermProgram :: IO Bool
isRecognisedTermProgram = do
  termProgram <- lookupEnv "TERM_PROGRAM"
  return $ case termProgram of
    Just "iTerm.app" -> True
    Just "Apple_Terminal" -> True
    _ -> False

itermLevel :: IO ColorLevel
itermLevel = do
  termProgramVersion <- lookupEnv "TERM_PROGRAM_VERSION"
  return $ case termProgramVersion of
    Just s ->
      let v = parseNumericVersion s
      in if v.major >= 3
         then TrueColor
         else EightBit
    Nothing -> EightBit

termProgramLevel :: IO ColorLevel
termProgramLevel = do
  termProgram <- lookupEnv "TERM_PROGRAM"
  case termProgram of
    Just "iTerm.app" -> itermLevel
    Just "Apple_Terminal" -> return EightBit
    _ -> return Unsupported

termIs256Color :: String -> Bool
termIs256Color term = matchRegex (mkRegex "(?i)-256(color)?$") term == Just []

termIs16Color :: String -> Bool
termIs16Color term = matchRegex (mkRegex "(?i)^screen|^xterm|^vt100|^vt220|^rxvt|color|ansi|cygwin|linux") term == Just []

supportedColorLevel :: Bool -> IO ColorLevel
supportedColorLevel isTTY = do
  forceLevel <- envForceLevel
  let minLevel = fromMaybe Unsupported forceLevel

  if not isTTY && isNothing forceLevel
  then return Unsupported
  else do
    dumbTerm <- hasEnvMatching "TERM" "dumb"
    ciEnv <- inEnv "CI"
    teamcityEnv <- inEnv "TEAMCITY_VERSION"
    tfBuildEnv <- inEnv "TF_BUILD"
    agentNameEnv <- inEnv "AGENT_NAME"
    colortermTruecolor <- hasEnvMatching "COLORTERM" "truecolor"
    recognisedTermProgram <- isRecognisedTermProgram
    colortermEnv <- inEnv "COLORTERM"

    if dumbTerm
    then return minLevel
    else if isWindows
    then windowsLevel
    else if ciEnv
    then do
      travis <- inEnv "TRAVIS"
      circleci <- inEnv "CIRCLECI"
      appveyor <- inEnv "APPVEYOR"
      gitlabCI <- inEnv "GITLAB_CI"
      githubActions <- inEnv "GITHUB_ACTIONS"
      buildkite <- inEnv "BUILDKITE"
      drone <- inEnv "DRONE"
      codeship <- hasEnvMatching "CI_NAME" "codeship"
      if travis || circleci || appveyor || gitlabCI || githubActions || buildkite || drone || codeship
      then return Basic
      else return minLevel
    else if teamcityEnv
    then teamcityLevel
    else if tfBuildEnv && agentNameEnv
    then return Basic
    else if colortermTruecolor
    then return TrueColor
    else if recognisedTermProgram
    then termProgramLevel
    else do
    term <- lookupEnv "TERM"
        case term of
        Just t ->
            if termIs256Color t
            then return EightBit
            else if termIs16Color t || colortermEnv
            then return Basic
            else return minLevel
        Nothing -> return minLevel

supportedColorLevels :: IO ColorLevelInfo
supportedColorLevels = do
  stdoutLevel <- supportedColorLevel $ isTerminalDevice stdout
  stderrLevel <- supportedColorLevel $ isTerminalDevice stderr
  return $ ColorLevelInfo { stdoutLevel, stderrLevel }