{-|
Module : Opdracht2
Omschrijving : Opdracht 2
Inleverpoging : Eerste kans
Student naam : Niek van Leeuwen,Jordy van Essen
Nummer : 0967267,0968981
Klas : TI2C
-}

module Opdracht2
    where

import Data.Char

-- 1A Bereken de grootste gemenen deler van twee natuurlijke getallen
euclid::Integer -> Integer -> Integer 
euclid x y
        | y == 0 = x
        | otherwise = euclid y (x `mod` y)

-- 1B e * d = 1(mod m)
egcd::Integer->Integer->(Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =  
    let (g, s, t) = egcd (b `mod` a) a
    in (g, t - (b `div` a) * s, s)

-- maak de uitkomst, als deze negatief is, positief door de modulus er bij op te tellen
calcD::Integer->Integer->Integer
calcD e m = if d < 0 then d+m else d
        where (_,d,_) = egcd e m
   
-- 2

-- bereken een getal dat relatief priem is met m'
calcE::Integer->Integer->Integer
calcE m' e
        | e > m' = calcE m' (m'-1)
        | (euclid e m') == 1 = e
        | otherwise = calcE m' (e-1)

p = 31
q = 37
m = p*q
m' = (p-1)*(q-1)
e = calcE m' m'-10
d = calcD e m'

-- 3
rsa::(Integer, Integer) -> Integer -> Integer
rsa (key, m) x = mod (x^key) m       

-- 4
rsaencrypt::(Integer, Integer) -> Char -> Integer
rsaencrypt (key, m) x = rsa (key, m) (toInteger(ord x))

rsadecrypt::(Integer, Integer) -> Integer -> Char
rsadecrypt (key, m) x = chr(fromIntegral(rsa (key, m)  x))

{-
    Opdracht 5

    Bob geeft zijn public key aan Alice. Alice versleutelt haar bericht met haar private key. Daarna versleutelt ze het bericht met bob zijn public key. Nu wordt het bericht verstuurd naar Bob. Hij kan nu het bericht met zijn private key ontsleutelen en daarna de volgende laag ontsleutelen met de public key van Alice. Zo kunnen Alice en Bob dus veilig communiceren met elkaar.
-}

main = do 
   putStrLn "Bereken de grootste gemenen deler van e en m'"
   print(euclid e m') 
   putStrLn "Bereken e·d= 1(mod m):"
   print(calcD e m')
   putStrLn "Versleutel de letter 'K':"
   let key = rsaencrypt (e,m) 'K'
   print(key)
   putStrLn "Verkijg de letter met de versleutelde tekst:"
   print(rsadecrypt (e,m) key)
