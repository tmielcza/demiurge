import Parse(parse)

main = do putStrLn "Expr: "
          x <- getLine
          (print . show . parse) x
