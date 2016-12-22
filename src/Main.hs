import Parse

main = do putStrLn "Expr: "
          x <- getLine
          (print . show . parse) x
