
import Control.Monad (void)

import Reactive.Threepenny

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = do
  startGUI defaultConfig setup

listItem :: String -> UI Element
listItem s = mkElement "li" # set text s

timesClicked elem = accumB (0::Int) ((+1) <$ UI.click elem)

stringsGiven button input = accumB [] ((++ ["Clicked"]) <$ UI.click button)


setup :: Window -> UI ()
setup rootWindow = void $ do
  userNameInput <- UI.input # set (attr "placeholder") "User name"
  loginButton <- UI.button #+ [ string "Clickity!" ]
  outputClicks <- UI.p
  nameList <- UI.ul
  getBody rootWindow #+
    map element [ userNameInput, loginButton, outputClicks] --, nameList ]


  clicks <- timesClicked loginButton
  -- names <- stringsGiven loginButton userNameInput
  -- sink :: ReadWriteAttr x i o -> Behavior i -> UI x -> UI x
  element outputClicks # sink text (show <$> clicks)
  -- nameList <- (fmap (listItem) <$> names)
  -- element nameList # sink children nameList

