roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
    let d = sqrt (b * b - 4 * a * c)
        e = 2 * a
    in ( (-b - d) / e, (-b + d) / e )
    
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x,y) =
    let a = sqrt(x*x + y*y)
    in (x/a,y/a)
    
unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x,y,z) =
    let a = sqrt(x*x + y*y + z*z)
    in (x/a,y/a,z/a)
    
    
