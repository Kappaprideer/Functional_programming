f1 = \x -> (x-2)
f2 = \x y -> sqrt(x^2 + y^2)
f3 = \x y z -> sqrt(x^2 + y^2 + z^2)
f4 = \x -> 2*x
f5 = \x -> x*2
f6 = \x -> 2^x
f10 = \x -> (x^2)
f11 = \x -> (2/x)
f12 = \x -> (x/3)
f13 = \x -> (4-x)

f14 = \x -> sqrt(x)
f15 = \x -> sqrt(x^2)

f7 = \x -> (x `mod` 2 == 0)
f8 = \x -> (2*sqrt(x)^3 * (sqrt(x)+1))
f9 = \x -> (if x==1 then 3 else 0)
