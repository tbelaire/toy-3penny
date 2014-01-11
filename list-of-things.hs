
import Control.Monad (void)

import Reactive.Threepenny

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = do
  startGUI defaultConfig setup

listItem :: String -> UI Element
listItem s = mkElement "li" # set text s

timesClicked :: Element -> UI (Behavior Int)
timesClicked elem = accumB 0 ((+1) <$ UI.click elem)

stringsGiven :: Element -> t -> UI (Behavior [String])
stringsGiven button input = accumB [] ((++ [" has clicked"]) <$ UI.click button)


submitEvents :: Element -> Element -> UI (Event String)
submitEvents button input = do val <- get value input
                               return $ val <$ (UI.click button)

setup :: Window -> UI ()
setup rootWindow = void $ do
  userNameInput <- UI.input # set (attr "placeholder") "User name"
  loginButton <- UI.button #+ [ string "Clickity!" ]
  outputClicks <- UI.p
  nameSpew <- UI.p
  currNameElem <- UI.p
  nameList <- UI.ul
  getBody rootWindow #+
    map element [ userNameInput, loginButton, outputClicks , nameSpew, currNameElem ]


  clicks <- timesClicked loginButton
  element outputClicks # sink text (show <$> clicks)

  names <- stringsGiven loginButton userNameInput
  element nameSpew # sink text (unlines <$> names)

  nameE <- submitEvents loginButton userNameInput
  currName <- stepper "Ash" nameE
  element currNameElem # sink text currName
