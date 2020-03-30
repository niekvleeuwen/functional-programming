{-|
Module : Opdracht1
Omschrijving : Opdracht 1
Inleverpoging : Eerste kans
Student naam : Niek van Leeuwen,Jordy van Essen
Nummer : 0967267,0968981
Klas : TI2C
-}

module Opdracht1
    where

-- faculteit met gebruik van pattern matching
faca :: Int -> Int 
faca 0 = 1 
faca n = n * faca(n - 1) 

-- faculteit met gebruik van guards
facb :: Int -> Int
facb n | n < 0 = error "Faculteit niet mogelijk met min getallen"
       | n == 0 = 1 
       | n /= 0 = n * facb(n-1) 

--
-- ((-b) +/- sqrt(b^2 - 4ac)) / 2a
--
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c = if d < 0 then [0, 0] else [x1, x2]
    where
        x1 = ((-b) - sqrt(d)) / (2 * a)
        x2 = ((-b) + sqrt(d)) / (2 * a)
        d = b*b - 4*a*c

nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c | d < 0 = [0, 0] 
                 | d >= 0 = [x1, x2]
                where
                    x1 = ((-b) - sqrt(d)) / (2 * a)
                    x2 = ((-b) + sqrt(d)) / (2 * a)
                    d = b*b - 4*a*c

-- return alle worpen waarvan de ogen een veelvoud is van vijf
worpena::[(Int,Int,Int)]
worpena = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],((x+y+z) `mod` 5) == 0]

-- return alle worpen waarvan de ogen een veelvoud is van n
worpenb:: Int -> [(Int,Int,Int)]
worpenb n = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],((x+y+z) `mod` n) == 0]

main = do 
   putStrLn "De faculteit van 10 is:" 
   print(faca 10)
   putStrLn "De faculteit van 20 is:" 
   print(facb 20)
   print(nulpuntena 1 (-6) (-7)) -- Result is [-1.0, 7.0]
   print(nulpuntenb 1 (-6) (-7)) -- Result is [-1.0, 7.0]
   print(worpena)
   print(worpenb 7)
