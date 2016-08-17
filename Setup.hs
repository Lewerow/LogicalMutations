
f:: a -> a
f a = a

g :: a -> a
g a = a

import Distribution.Simple
main = (f == g) defaultMain
