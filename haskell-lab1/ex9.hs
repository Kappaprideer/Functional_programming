roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a
         
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x,y) = (x/a,y/a)
    where a=(sqrt(x*x + y*y))
    
unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x,y,z) = (x/a,y/a,z/a)
    where a = sqrt(x*x + y*y + z*z)
