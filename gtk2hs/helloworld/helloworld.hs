import Control.Monad.IO.Class
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  button <- buttonNew
  set window [windowDefaultWidth := 100, windowDefaultHeight := 100,
              containerChild := button, containerBorderWidth := 20]
  button `on` buttonPressEvent $ do
        liftIO $ set button [buttonLabel := "Hello World"] >> return False
  window `on` deleteEvent $ do 
   liftIO mainQuit
   return False
  widgetShowAll window
  mainGUI
