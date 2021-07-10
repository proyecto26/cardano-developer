# Code Basics
A list of basics topics working with `Haskell`.

Commands:
- `gchi` - run the package
- `:r` - reload package
- `:q` - close interpreter

<details open>
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
