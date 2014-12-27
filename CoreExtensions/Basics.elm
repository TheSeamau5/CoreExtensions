module CoreExtensions.Basics where
{-| Extension to the Core Basics Library. Includes several useful functions
either missing or simply not present from the core Basics Library.

# Mathematics
@docs infinity, lerp, rangeMap

# Function Composition
@docs compose, compose2, compose3, compose4, compose5

-}

{-| Infinity (the number defined to be greater than all numbers)

    infinity == 1 / 0
-}
infinity : Float
infinity : 1 / 0

{-| Linear Interpolation.

    lerp 2 3 0.3 == 2.3
-}
lerp : Float -> Float -> Float
lerp value1 value2 amount =
  (1 - amount) * value1 + amount * value2

{-| Re-maps a number from one range to another.

    rangeMap 25 0 100 200 300 == 225
-}
rangeMap : Float -> Float -> Float -> Float -> Float
rangeMap value low1 high1 low2 high2 =
  let m = (low2 - high2) / (low1 - high1)
      b = low2 - m * low1
  in m * value + b


{-| Function composition. Computes the functions from left to right.

    compose f g x == g (f x)

-}
compose : (a -> b) -> (b -> c) -> (a -> c)
compose f g a = g (f a)

{-| Composes a 1-arity function and a 2-arity function by applying the 1-arity
function on the arguments to the 2-arity function

    square x = x * x
    add x y = x + y

    squareThenAdd = compose2 square add

    squareThenAdd 2 3 == 13

-}
compose2 : (a -> b) -> (b -> b -> c) -> (a -> a -> c)
compose2 f g a b = g (f a) (f b)


{-| Composes a 1-arity function and a 3-arity function.
Analogous to compose2.
-}
compose3 : (a -> b) -> (b -> b -> b -> c) -> (a -> a -> a -> c)
compose3 f g a b c = g (f a) (f b) (f c)

{-| Composes a 1-arity function and a 4-arity function.
Analogous to compose2.
-}
compose4 : (a -> b) -> (b -> b -> b -> b -> c) -> (a -> a -> a -> a -> c)
compose4 f g a b c d = g (f a) (f b) (f c) (f d)

{-| Composes a 1-arity function and a 5-arity function.
Analogous to compose2.
-}
compose5 : (a -> b) -> (b -> b -> b -> b -> b -> c) ->
           (a -> a -> a -> a -> a -> c)
compose5 f g a b c d e = g (f a) (f b) (f c) (f d) (f e)
