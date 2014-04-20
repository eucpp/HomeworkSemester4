-- GUI приложение для вычисление производной полинома

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.IO.Class 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

updateDeriv :: Entry -> Entry -> IO ()
updateDeriv expr_edit deriv_edit  = 
    entryGetText expr_edit >>=
    \str -> entrySetText deriv_edit str
    
format_word :: String -> String
format_word []  = []
format_word str = (\(x:xs) -> (toUpper x):xs) . map toLower $ str

format :: String -> String
format str = case (elemIndex ' ' str) of
    Just ind    -> (\(str1, str2) -> (format $ take ind str1) ++ " "  ++ (format str2))  $ splitAt (ind + 1) str
    Nothing     -> format_word str

main = do
    initGUI
    Just xml    <- xmlNew "window_ui.glade"
    window      <- xmlGetWidget xml castToWindow "mainwindow"
    input       <- xmlGetWidget xml castToEntry "input"
    output      <- xmlGetWidget xml castToEntry "output"
    button      <- xmlGetWidget xml castToButton "go_button"
    button `on` buttonActivated $ entryGetText input >>= 
                                  \str -> entrySetText output $ format str
    window `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll window
    mainGUI
    
    
