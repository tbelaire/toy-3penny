
import Control.Monad (void)

import Reactive.Threepenny

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = do
  startGUI defaultConfig setup

timesClicked :: Element -> UI (Behavior Int)
timesClicked elem = accumB (0::Int) ((+1) <$ UI.click elem)

setup :: Window -> UI ()
setup rootWindow = void $ do
  button <- UI.button #+ [ string "Clickity!" ]
  outputClicks <- UI.p
  getBody rootWindow #+
    map element [ button, outputClicks]


  clicks <- timesClicked button
  element outputClicks # sink text (show <$> clicks)

