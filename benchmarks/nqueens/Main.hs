import NqueensDPH
import Timing
import System.Environment

main :: IO ()
main
  = do args <- getArgs
       case args of
         [n] -> run (read n)
         _   -> putStr $ "usage: $0 <size>"

run n = do (result, tme) <- time $ let result = (nqPA n) in result `seq` return result
           putStr $ prettyTime tme

