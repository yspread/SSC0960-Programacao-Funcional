isPrime :: Int -> Bool
isPrime n   --função para verificar se um número é primo
    | n < 2 = False -- se n for menor que 2, n com certeza não é primo
    |otherwise = null [x | x <- [2..(n - 1)], mod n x == 0] --verificação se n é divisivel por algum número entre 2 e n - 1
    -- caso não haja nenhum divisor entre 2 e n - 1, a lista será nula e a função retornará true
maxDist :: [Int] -> Int
maxDist primos  = maximum (zipWith (-) (tail primos) primos)
{- essa função retorna a maior distância entre 2 primos
pra isso, subtraio paralelamente a lista de primos de sua tail
isso permite que eu pegue a diferença 2 a 2 e consiga todas as diferenças-}

main = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    let lista = [x..y] -- lista que vai de x a y
    let primos = filter isPrime [x..y] -- lista que comporta todos os primos de x a y
    if length primos < 2 --se a lista de primos tiver tamanho 0 ou 1, a distância será 0
        then 
            print 0
        else do 
            let max = maxDist primos
            print max