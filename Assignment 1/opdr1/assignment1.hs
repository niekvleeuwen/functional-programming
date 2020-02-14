faculteit_a :: Int -> Int 
faculteit_a 0 = 1 
faculteit_a n = n * faculteit_a(n - 1) 

faculteit_b :: Integer -> Integer 
faculteit_b n | n == 0 = 1 
              | n /= 0 = n * faculteit_b(n-1) 

main = do 
   putStrLn "De faculteit van 10 is:" 
   print(faculteit_a 10)
   putStrLn "De faculteit van 20 is:" 
   print(faculteit_b 20)