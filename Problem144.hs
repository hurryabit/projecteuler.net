module Problem144 where

solution = length $ takeWhile (\(x,y,_) -> not (abs x <= 0.01 && y > 0)) $ iterate step (1.4,-9.6,atan2 (-19.7) 1.4)
    where step (x,y,p) = let q = atan2 y (4*x)
                             p' = 2*q - p + pi
                             a = 4*(cos p')^2 + (sin p')^2
                             b = 8*x*cos p' + 2*y*sin p'
                             c = 4*x^2 + y^2 - 100
                             d = sqrt $ b^2 - 4*a*c
                             t = max ((-b - d) / (2*a)) ((-b + d) / (2*a))
                         in  (x + t*cos p',y + t*sin p',p')
