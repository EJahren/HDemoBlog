module Constants where
import Data.Map as M
import Happstack.Server

myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000 

domain :: String
domain = "http://localhost:8000"

accounts = M.fromList [("Dude","ILoveMyPasswordLongtime")]
