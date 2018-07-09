```haskell
myvar = "value"

-- Within a function body we use let and in to define variables
add5 :: Int -> Int
add5 val = 
    let  
        five = 5
    in 
        five + val

-- Within a do block we use let 
greet :: IO ()
greet = do
    let helloWorld = "Hello world!"
    putStrLn helloWorld
```
