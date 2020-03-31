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
 
func::Double->Double
func x = x^2 + 2*x
 
differentieer::(Double->Double)->Double->Double->Double
differentieer f x p = ((f (x+p)) - (f (x))) / p
 
integral::(Double->Double)->Double->Double->Double->Double
integral f a b p
    | a < b  = ((f(a+(p/2))*p) + integral f (a+p) b p)
    | a >= b = (f(a+(p/2))*p)
   
dubbelen::(Ord a) => [a]->[a]
dubbelen s = nub $ s \\ nub s
 
s = [1..6]
stenen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
 
count::Integer->[Integer]->Integer
count c [] = 0
count c (x:xs)
    | c==x= 1 + (count c xs)
    | otherwise = count c xs
   
convert::[Integer]->[[Integer]]
convert list = [sort [a,b,c,d,e,f],list] where
    a = count 1 list
    b = count 2 list
    c = count 3 list
    d = count 4 list
    e = count 5 list
    f = count 6 list
   
-- Gebruik deze functie voor de kans van mogelijke uitkomst.
chance::[Integer]
chance = [a,b,c,d,e,f,g,h] where
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
poker [[]]    = error "List is empty!"
poker [a] = [hands (convert a)]
poker (x:xs)  = [hands (convert x)] ++ (poker xs)
   
   
hands::[[Integer]]->Integer
hands list
    | (list!!0)==[0,0,0,0,0,5] = 7
    | (list!!0)==[0,0,0,0,1,4] = 6
    | (list!!0)==[0,0,0,1,1,3] = 5
    | (list!!0)==[0,0,0,0,2,3] = 4
    | (list!!0)==[0,0,0,1,2,2] = 3
    | (list!!0)==[0,0,1,1,1,2] = 2
    | (list!!0)==[0,1,1,1,1,1] = (straight (list!!1))
    | otherwise                = 0
 
straight::[Integer]->Integer
straight list
    | list==[1,2,3,4,5] = 1
    | list==[2,3,4,5,6] = 1
    | otherwise         = 0