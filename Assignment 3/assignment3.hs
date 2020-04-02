{-|
Module : Opdracht3
Omschrijving : Opdracht 3
Inleverpoging : Eerste kans
Student naam : Niek van Leeuwen,Jordy van Essen
Nummer : 0967267,0968981
Klas : TI2C
-}

module Opdracht3
    where

import Data.List
import Data.Char
 
voorbeeldfunctie::Double->Double
voorbeeldfunctie x = x^2 + 2*x

-- 1A Differentieer functie f op x=p met precisie p
differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = ((f (x+p)) - (f (x))) / p
 
-- 1B Integreer functie f op interval a,b met precisie p
integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p
    | a < b  = ((f(a+(p/2))*p) + integreer f (a+p) b p)
    | a >= b = (f(a+(p/2))*p)

-- 2 Return alle elementen die meer dan een keer voorkomen
dubbelen::(Eq a) => [a]->[a]
dubbelen s = nub $ s \\ nub s

-- 3 Return een lijst met kansen op alle uitkomsten van het gegeven pokerspel 
calculatechances::[Integer]
calculatechances = [a,b,c,d,e,f,g,h] where
    list = poker stenen
    a = count 7 list
    b = count 6 list
    c = count 5 list
    d = count 4 list
    e = count 3 list
    f = count 2 list
    g = count 1 list
    h = count 0 list 

poker::[[Integer]]->[Integer]
poker [a] = [uitkomsten (convert a)]
poker (x:xs)  = [uitkomsten (convert x)] ++ (poker xs)
   
uitkomsten::[[Integer]]->Integer
uitkomsten list
    | (list!!0)==[0,0,0,0,0,5] = 7 -- Poker
    | (list!!0)==[0,0,0,0,1,4] = 6 -- Four of a kind
    | (list!!0)==[0,0,0,1,1,3] = 5 -- Three of a kind
    | (list!!0)==[0,0,0,0,2,3] = 4 -- Full house
    | (list!!0)==[0,0,0,1,2,2] = 3 -- Two pair
    | (list!!0)==[0,0,1,1,1,2] = 2 -- One pair
    | (list!!0)==[0,1,1,1,1,1] = (straight (list!!1)) -- Straight
    | otherwise                = 0 -- Bust
 
-- Deze functie checkt of een gegeven list een straight bevat
straight::[Integer]->Integer
straight list
    | list==[1,2,3,4,5] = 1
    | list==[2,3,4,5,6] = 1
    | otherwise         = 0

-- De lijst met lijsten van iedere worp bouwen:
s = [1..6]
stenen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
 
-- Onderstaande functie retourneert het aantal voorkomens van c in een lijst:
count::Integer->[Integer]->Integer
count c [] = 0
count c (x:xs)
    | c==x= 1 + (count c xs)
    | otherwise = count c xs

-- Onderstaande functie converteert een lijst in een aantal tuples met voorkomens
convert::[Integer]->[[Integer]]
convert list = [sort [a,b,c,d,e,f],list] where
    a = count 1 list
    b = count 2 list
    c = count 3 list
    d = count 4 list
    e = count 5 list
    f = count 6 list

main = do 
    putStrLn "Differentier x^2 + 2x op x = 0:" 
    print(differentieer voorbeeldfunctie (1/100000) 0)
    putStrLn "Integreer x^2 + 2x op interval(0,5):" 
    print(integreer voorbeeldfunctie 0 5 (1/100000))
    putStrLn "Geef alle elementen die meer dan een keer voorkomen:"
    print(dubbelen "aabbbcdeeeef")
    putStrLn "De kansen van alle uitkomsten van het gegeven pokerspel:"
    print(calculatechances)
