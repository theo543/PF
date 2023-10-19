fs = map (+)
apply l = zipWith ($) (fs l) l

main = print (apply [1,2,3])
