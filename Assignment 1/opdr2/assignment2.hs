nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c = if d < 0 then error "0" else [x, y]
            where
                x = e + sqrt d / (2 * a)
                y = e - sqrt d / (2 * a)
                d = b * b - 4 * a * c
                e = - b / (2 * a)


main = do 
   print(nulpuntena 1 4 4)
