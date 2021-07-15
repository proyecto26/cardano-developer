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
