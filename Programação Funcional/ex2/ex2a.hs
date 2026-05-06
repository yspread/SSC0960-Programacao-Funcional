somaDivisores :: Int -> Int
somaDivisores n = foldl (+) 0 [x | x <- [1..(n-1)], mod n x == 0] --soma de todos os divisores próprios de n

ehDefeituoso :: Int -> Bool --verifica se um número é defeituoso
ehDefeituoso n = somaDivisores n < n

ehPerfeito :: Int -> Bool --verifica se um número é perfeito
ehPerfeito n = somaDivisores n == n

ehAbundante :: Int -> Bool --verifica se um número é abundante
ehAbundante n = somaDivisores n > n

main = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    let lista = [x..y] --lista que comporta os valores entre 
    -- conto quantos defeituosos, perfeitos e abundantes tem na lista
    let nDefeituosos = length (filter ehDefeituoso lista)
    let nPerfeitos   = length (filter ehPerfeito lista)
    let nAbundantes  = length (filter ehAbundante lista)
    print nDefeituosos
    print nPerfeitos
    print nAbundantes