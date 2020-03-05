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

worpena::[(Int,Int,Int)]
worpena = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],((x+y+z) `mod` 5) == 0]

worpenb:: Int -> [(Int,Int,Int)]
worpenb n = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],((x+y+z) `mod` n) == 0]

main = do
    print(nulpuntena 1 4 4) -- Result is [-2.0, -2.0]
    print(nulpuntenb 1 4 4) -- Result is [-2.0, -2.0]
    print(worpena)
    print(worpenb 7)
