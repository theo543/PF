fs l = map (+) l
apply l = map (\(x, f) -> f x) (zip l (fs l))

main = print (apply [1,2,3])
