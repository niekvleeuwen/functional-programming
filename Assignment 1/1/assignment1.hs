faca :: Int -> Int 
faca 0 = 1 
faca n = n * faca(n - 1) 

facb :: Int -> Int
facb n | n == 0 = 1 
       | n /= 0 = n * facb(n-1) 

main = do 
   putStrLn "De faculteit van 10 is:" 
   print(faca 10)
   putStrLn "De faculteit van 20 is:" 
   print(facb 20)