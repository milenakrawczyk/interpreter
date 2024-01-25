module ErrorsMiLatte where

import Control.Exception
import Control.Monad.State
import TypesMiLatte

throwError :: Show b => String -> b -> ProgStateT (a)
throwError msg obj = liftIO $ throwIO $ ErrorCall $ "Error: " ++ msg ++ " (" ++ (show obj) ++ ")"

throwErrorNoObj :: String -> ProgStateT (a)
throwErrorNoObj = liftIO . throwIO . ErrorCall

vthrowError :: Show b => String -> b -> ValidStateT (a)
vthrowError msg obj = liftIO $ throwIO $ ErrorCall $ "Error: " ++ msg ++ " (" ++ (show obj) ++ ")"

vthrowErrorNoObj :: String -> ValidStateT (a)
vthrowErrorNoObj = liftIO . throwIO . ErrorCall
