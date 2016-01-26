
import Poker
import Test.SmallCheck

instance Serial AB
    where series = (\/) (cons0 A) (cons0 B)
          coseries rs d = [\t -> case t of
                                  A -> t0
                                  B -> t1
                       | t0 <- rs d, t1 <- rs d]
