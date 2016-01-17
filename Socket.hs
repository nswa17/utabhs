module Socket where

import Network.Socket

localhost = "127.0.0.1"

sock = socket AF_INET Stream defaultProtocol

test :: Socket -> IO ()
test soc = do
  (soc, socAddr) <- accept soc
  connect soc socAddr
  test soc
