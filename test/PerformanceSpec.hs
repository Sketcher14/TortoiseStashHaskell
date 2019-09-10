module PerformanceSpec
  ( runTests
  ) where

import Criterion.Main

import AES128.Encryption
import AES128.Decryption
import AES128.Utils
import GUI.Global

sourcePath :: String
sourcePath = "test/source/"

password :: String
password = "HelloWorld"

runTests :: [Benchmark]
runTests = [ bgroup
             "encryption"
               [ bench "file 1mb" $
                   nfIO (readEncryptWrite (sourcePath ++ "1mb.txt") (sourcePath ++ "1mb.txt." ++ extension) password)
               , bench "file 2mb" $
                   nfIO (readEncryptWrite (sourcePath ++ "2mb.txt") (sourcePath ++ "2mb.txt." ++ extension) password) 
               , bench "file 4mb" $
                   nfIO (readEncryptWrite (sourcePath ++ "4mb.txt") (sourcePath ++ "4mb.txt." ++ extension) password) 
               , bench "file 6mb" $
                   nfIO (readEncryptWrite (sourcePath ++ "6mb.txt") (sourcePath ++ "6mb.txt." ++ extension) password) 
               , bench "file 8mb" $
                   nfIO (readEncryptWrite (sourcePath ++ "8mb.txt") (sourcePath ++ "8mb.txt." ++ extension) password) 
               ]
           ]