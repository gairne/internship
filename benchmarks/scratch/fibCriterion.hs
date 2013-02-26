import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = defaultMain
           [
             bgroup "fib"
               [ bench "f10" $ whnf fib 10
               , bench "f20" $ whnf fib 20
               , bench "f30" $ whnf fib 30
               , bench "f31" $ whnf fib 31
             ]
           ]

