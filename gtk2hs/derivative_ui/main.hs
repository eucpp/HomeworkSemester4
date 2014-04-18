-- GUI приложение для вычисление производной полинома

import Control.Monad.IO.Class 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Derivative

updateDeriv :: Entry -> Entry -> IO ()
updateDeriv expr_edit deriv_edit  = 
    entryGetText expr_edit >>=
    \str -> entrySetText deriv_edit str
    

main = do
    initGUI
    Just xml    <- xmlNew "window_ui.glade"
    window      <- xmlGetWidget xml castToWindow "mainwindow"
    expr_edit   <- xmlGetWidget xml castToEntry "expr"
    deriv_edit  <- xmlGetWidget xml castToEntry "deriv"
    button      <- xmlGetWidget xml castToButton "calc_button"
    button `on` buttonActivated $ updateDeriv expr_edit deriv_edit
    window `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll window
    mainGUI
    
    
