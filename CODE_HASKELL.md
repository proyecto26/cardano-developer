# Code Basics
A list of basics topics working with `Haskell`.

Commands:
- `ghci` - run the package
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
