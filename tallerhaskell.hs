

sucesor :: Int -> Int
sucesor n = n + 1

predecesor :: Int -> Int
predecesor n = n - 1


add :: Int -> Int -> Int
add a 0 = a                         
add a b = add (sucesor a) (b - 1)   


multiplicar :: Int -> Int -> Int
multiplicar _ 0 = 0
multiplicar a b = add a (multiplicar a (b - 1))


restar :: Int -> Int -> Int
restar a 0 = a
restar a b = restar (predecesor a) (b - 1)

dividir :: Int -> Int -> Int
dividir a b
    | b == 0    = error "no se puedee dividir por cero :(("
    | a < b     = 0
    | otherwise = 1 + dividir (restar a b) b


addReal :: Float -> Float -> Float
addReal x y = x + y


main :: IO ()
main = do
    putStrLn "=== PRUEBAS DE OPERACIONES ==="
    putStrLn $ "Sucesor de 5: " ++ show (sucesor 5)
    putStrLn $ "Predecesor de 5: " ++ show (predecesor 5)
    putStrLn $ "Suma 3 + 4: " ++ show (add 3 4)
    putStrLn $ "Multiplicar 3 * 4: " ++ show (multiplicar 3 4)
    putStrLn $ "Restar 10 - 3: " ++ show (restar 10 3)
    putStrLn $ "Dividir 12 / 3: " ++ show (dividir 12 3)
    putStrLn $ "Suma real 3.5 + 4.2: " ++ show (addReal 3.5 4.2)
