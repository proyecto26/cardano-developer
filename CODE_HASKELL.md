# Code Basics
A list of basics topics working with `Haskell`.

Commands:
- `ghci` - run the package
- `:l` - load file
- `:r` - reload package
- `:q` - close interpreter

<details>
  <summary><b>Hello world</b></summary>

  - helloWorld.hs:
  ```hs
  module HelloWorld where

  -- Monad
  main :: IO ()
  main = putStrLn "Hello World"
  ```
  
  - Run program:
  ```sh
  ghci helloWorld.hs
  main
  ```
</details>


<details>
  <summary><b>Sum a range of numbers</b></summary>

  - myLib.hs:
  ```hs
  module MyLib (sumNumbers) where

  -- Monad
  sumNumbers :: IO ()
  sumNumbers = print (sum [1..10])
  ```
  
  - Run program:
  ```sh
  ghci myLib.hs
  sumNumbers
  ```
</details>

<details>
  <summary><b>Functions</b></summary>

  ```hs
  hello name = "Hello, " ++ name
  hello "Juan"
  ```
</details>

<details>
  <summary><b>Types</b></summary>

  ```hs
  -- 2 params and return data
  f :: Int -> Int -> Int
  f x y = x*y+x+y
  f 2 3 -- 11
  ```
</details>

<details>
  <summary><b>Lists</b></summary>

  ```hs
  list = ["A", "B", "C"]
  head list -- "A"
  tail list -- ["B", "C"]
  ```
</details>

<details>
  <summary><b>Anonymous functions</b></summary>

  ```hs
  -- a function without a name (Lambda abstraction)
  f = \x y -> x*y+x+y
  f 2 3 -- 11
  ```
</details>

<details>
  <summary><b>Higher-order functions</b></summary>

  ```hs
  [2*x | x <- [0..10]]
  -- [0, 2, 4, etc]
              
  map :: (elm -> res) -> [elm] -> [res]
  map (\x -> x*2+1) [1..10]
  
  -- Free point style (Event delegation)
  mul2 = \x -> x * 2
  map mul2 [1..5]
  ```
</details>
  
<details>
  <summary><b>I/O</b></summary>

  - io.hs:
  ```hs
  greeting() = do
    name <- getLine
    eventName <- getLine
    putStrLn ("Hola" ++ name)
    putStrLn ("Espero " ++ eventName ++ " te guste")
  ```
  
  - Run program:
  ```sh
  stack ghci
  :l io.hs
  greeting()
  ```
</details>
  
<details>
  <summary><b>Recursion</b></summary>

  - recursion.hs:
  ```hs
  -- State machines?
  myFilter :: (a -> Bool) -> [a] -> [a]
  myFilter fn [] = []
  myFilter fn (head:tail)
    | fn head = head : myFilter fn tail
    | otherwise = myFilter fn tail
  ```
  
  - Run program:
  ```sh
  stack ghci
  :l recursion.hs
  myFilter (\x -> x > 3) [1, 2, 3, 4] # greater than 3
  myFilter (>3) [1, 2, 3, 4]
  ```
</details>

<details>
  <summary><b>Loops</b></summary>

  - loops.hs:
  ```hs
  -- map
  f x = x*(x+1)
  lst = map f [1..10]
  
  -- using prefix functions
  div = (/)
  
  -- reduce
  resultLeft = foldl div 1 [1..10]
  
  -- reduceRight
  resultRight = foldr div 1 [1..10]
  
  main = do
    print lst
    print resultLeft
    print resultRight
  ```
  
  - Run program:
  ```sh
  stack runhaskell loops.hs
  ```
</details>

<details>
  <summary><b>Currying</b></summary>

  ```hs
  currying :: Int -> Int -> Int -> Int
  currying x y z = x*y+z
  currying 2 3 4 -- 10
  
  -- manual example (not necessary in Haskell, currying works automatically)
  currying'   = \x y z -> x*y+z
  currying''  = \x -> (\y z -> x*y+z)
  currying''' = \x -> (\y -> (\z -> x*y+z))
  ```
</details>

## Credits
- [JS vs Haskell una comparación impura](https://speakerdeck.com/jessecogollo/js-vs-haskell-una-comparacion-impura) by [@jessecogollo](https://github.com/jessecogollo)
- [Haskell](https://www.haskell.org/)
- [Speller example](https://gist.github.com/jessecogollo/244a9bdb94b99d05e49fa16303cc9ad8)
- [Hanged example](https://gist.github.com/jessecogollo/2e8d38de900d5631786f8d7963e9ac70)
- [Functional Programming in Haskell](https://www.futurelearn.com/courses/functional-programming-haskell) - Supercharge Your Coding.
