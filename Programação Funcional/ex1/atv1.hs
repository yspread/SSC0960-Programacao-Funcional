calcP :: Double -> Double -> Double -> Double
calcP a b c = (a + b + c) /2
calcHeron :: Double -> Double -> Double -> Double -> Double
calcHeron p a b c = sqrt(p * (p - a) * (p - b) * (p - c))

main = do
    ladoum <- readLn :: IO Double
    ladodois <- readLn :: IO Double
    ladotres <- readLn :: IO Double
    if ladoum + ladodois >= ladotres && ladoum + ladotres >= ladodois && ladodois + ladotres >= ladoum && ladoum >= 0 && ladodois >= 0 && ladotres >= 0
        then do
            let p = calcP ladoum ladodois ladotres
            let a = calcHeron p ladoum ladodois ladotres
            print a
        else
            putStrLn "-"