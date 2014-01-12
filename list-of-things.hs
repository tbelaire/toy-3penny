import Data.List (intersperse)
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

submitEvents :: Element -> Element -> UI (Event String)
submitEvents button input = do currVal <- stepper "" $ UI.valueChange input
                               return $ currVal <@ (UI.click button)

setup :: Window -> UI ()
setup rootWindow = void $ do
  userNameInput <- UI.input # set (attr "placeholder") "User name"
  loginButton <- UI.button #+ [ string "Clickity!" ]
  outputClicks <- UI.p
  currNameElem <- UI.p
  nameListElem <- UI.p
  getBody rootWindow #+
    map element [ userNameInput, loginButton, outputClicks,
    nameListElem, currNameElem ]


  clicks <- timesClicked loginButton
  element outputClicks # sink text (show <$> clicks)

  nameE <- submitEvents loginButton userNameInput
  currNameB <- stepper "" nameE
  element currNameElem # sink text currNameB
 
  -- names :: Behavior [String]
  namesB <- accumB [] ((\name lst-> lst ++ [name]) <$> nameE)
  element nameListElem # sink text (concat  <$> namesB)

  return ()
