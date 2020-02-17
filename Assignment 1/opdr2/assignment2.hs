--
-- ((-b) +/- sqrt(b^2 - 4ac)) / 2a
--
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c = if d < 0 then [0, 0] else [x1, x2]
    where
        x1 = ((-b) + sqrt(d)) / (2 * a)
        x2 = ((-b) - sqrt(d)) / (2 * a)
        d = b*b - 4*a*c

nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c | d < 0 = [0, 0] 
                 | d >= 0 = [x1, x2]
                where
                    x1 = ((-b) + sqrt(d)) / (2 * a)
                    x2 = ((-b) - sqrt(d)) / (2 * a)
                    d = b*b - 4*a*c

main = do
    print(nulpuntena 1 4 4) -- Result is [-2.0, -2.0]
    print(nulpuntenb 1 4 4) -- Result is [-2.0, -2.0]

