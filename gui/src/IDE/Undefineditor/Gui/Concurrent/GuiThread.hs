module IDE.Undefineditor.Gui.Concurrent.GuiThread where

import Control.Exception (BlockedIndefinitelyOnMVar(BlockedIndefinitelyOnMVar), try)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Graphics.UI.Gtk (postGUIAsync) -- , postGUISync)

-- nasty hack... why isn't this function in the api?
-- also, I'm very concerned that the BlockedIndefinitelyOnMVar exception
-- will not be raised if we are on a different lightweight thread from
-- the gui thread but still on the same operating system thread.
isInGuiThread :: IO Bool
isInGuiThread = do
  putStrLn "creating empty mvar"
  mvar <- newEmptyMVar
  putStrLn "telling gui thread to fill the mvar"
  postGUIAsync (putMVar mvar ())
  putStrLn "trying to read the mvar"
  e <- try (readMVar mvar)
  putStrLn $ "finished trying: result: " ++ show e
  case e of
    Left BlockedIndefinitelyOnMVar -> return True
    Right () -> return False

inGuiThread :: IO a -> IO a
inGuiThread action = -- do
  action {- -- hacky solution until I get this figured out
  stay <- isInGuiThread
  case stay of
    True -> action
    False -> postGUISync action
    -}
