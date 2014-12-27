module CoreExtensions.Array where

{-| Extension to the Core Array Library. Includes several useful functions
either missing or simply not present from the core Array Library. The goal is
to make arrays as easy to use as lists.

# Basics
@docs head, tail, isEmpty, member, indexOf, reverse

# Create arrays
@docs initialize2, initialize3

# Putting arrays together
@docs intersperse, concat

# Map ALL the things!
@docs map2, map3, map4, map5

# Zip ALL the things!
@docs zip, zip3, zip4, zip5

# Unzip ALL the things!
@docs unzip, unzip3, unzip4, unzip5

# Special maps
@docs concatMap

# Special folds
@docs foldlSafe, foldrSafe, sum, product, minimum, maximum

# Sort
@docs sort, sortBy, sortWith
-}

import CoreExtensions.Basics (..)
import Array (..)

head : Array a -> Maybe a
head = get 0

tail : Array a -> Array a
tail array = slice 1 (length array) array

isEmpty : Array a -> Bool
isEmpty array = (length array) == 0

member : a -> Array a -> Bool
member element array =
  let iterate index =
    let arrayElement = get index array
    in
      case arrayElement of
        Nothing -> False
        Just x ->
          if x == element
          then True
          else iterate (index + 1)

  in iterate 0

{-| Get the index of an element in an array.
Returns the first index if the element is duplicated in the array.

    indexOf 2 (fromList [3,4,5,2,1,2]) == Just 3

    indexOf 1 (fromList [2,3,4]) == Nothing

-}
indexOf : a -> Array a -> Maybe Int
indexOf element array =
  let iterate index =
    let arrayElement = get index array
    in
    case arrayElement of
      Nothing -> Nothing
      Just x ->
        if x == element
          then Just index
          else iterate (index + 1)

  in iterate 0

{-| Reverse an array

    reverse (fromList [1,2,3]) == fromList [3,2,1]
-}
reverse : Array a -> Array a
reverse array =
  let maxIterations = length array

      iterate index =
        let arrayElement = get index array
        in
          case arrayElement of
            Nothing -> empty
            Just x -> push x (iterate (index + 1))

  in iterate 0

{-| Initialize a 2 dimensional array.

`initialize n m f` creates an `n x m` array with the element at index `(i,j)`
initialized to the result of `(f i j)`

    initialize2 2 3 (,) ==
      fromList [fromList [(0,0), (0,1), (0,2)],
                fromList [(1,0), (1,1), (1,2)]]
-}
initialize2 : Int -> Int -> (Int -> Int -> a) -> Array (Array a)
initialize2 x y constructor =
  initialize y (\xIndex ->
    initialize x (\yIndex ->
        constructor xIndex yIndex
      )
    )

initialize3 : Int -> Int -> Int -> (Int -> Int -> Int -> a) ->
              Array (Array (Array a))
initialize3 x y z constructor =
  initialize z (\xIndex ->
    initialize y (\yIndex ->
      initialize x (\zIndex ->
          constructor xIndex yIndex zIndex
        )
      )
    )


{-| Places the given value between all members of the given array.

    intersperse "on" (fromList ["turtles", "turtles", "turtles"]) ==
      fromList ["turtles", "on", "turtles", "on", "turtles"]
-}
intersperse : a -> Array a -> Array a
intersperse value array =
  let isEven x = x % 2 == 0
      maxIterations = 2 * (length array) - 1

      iterate index =
        if index < 0 then empty
        else
          if isEven index then
            case get (index // 2) array of
              Nothing -> empty
              Just x  -> push x (iterate (index - 1))
          else
            push value (iterate (index - 1))

  in iterate (maxIterations - 1)



{-| Concatenate a bunch of arrays into a single array:

    concat (fromList [(fromList [1,2]), (fromList [3,4])]) ==
      fromList [1,2,3,4]
-}
concat : Array (Array a) -> Array a
concat arrayOfArrays =
  let iterate index =
    let arrayAtIndex = get index arrayOfArrays
    in
      case arrayAtIndex of
        Just xs -> append xs (iterate (index + 1))
        Nothing -> empty
  in iterate 0


{-| Map a given function onto an array and flatten the result arrays.

    concatMap f array == concat (map f array)
-}
concatMap : (a -> Array b) -> Array a -> Array b
concatMap f array = concat <| map f array


{-| Combine two arrays, combining them with the given function. If one
array is longer, the extra elements are dropped.

    map2 (+) (fromList [1,2,3]) (fromList [1,2,3,4]) ==
      fromList [2,4,6]

    map2 (,) (fromList [1,2,3]) (fromList ['a', 'b']) ==
      fromList [(1,'a'), (2, 'b')]

    pairs : Array a -> Array b -> Array (a,b)
    pairs lefts rights =
      map2 (,) lefts rights

-}
map2 : (a -> b -> result) -> Array a -> Array b -> Array result
map2 f array1 array2 =
  let maxIterations = min (length array1) (length array2)
      iterate index =
        let array1Element = get index array1
            array2Element = get index array2
        in
          case (array1Element, array2Element) of

            (Just a, Just b) ->
              push (f a b) (iterate (index - 1))

            _ -> empty

  in iterate (maxIterations - 1)




map3 : (a -> b -> c -> result) -> Array a -> Array b -> Array c -> Array result
map3 f array1 array2 array3 =
  let maxIterations = min (length array1) (min (length array2) (length array3))

      iterate index =
        let array1Element = get index array1
            array2Element = get index array2
            array3Element = get index array3
        in
          case (array1Element, array2Element, array3Element) of

            (Just a, Just b, Just c) ->
              push (f a b c) (iterate (index - 1))

            _ -> empty

  in iterate (maxIterations - 1)




map4 : (a -> b -> c -> d -> result) ->
       Array a -> Array b -> Array c -> Array d -> Array result
map4 f array1 array2 array3 array4 =
  let maxIterations = min (length array1) <|
                      min (length array2) <|
                      min (length array3) (length array4)

      iterate index =
        let array1Element = get index array1
            array2Element = get index array2
            array3Element = get index array3
            array4Element = get index array4
        in
          case (array1Element, array2Element, array3Element, array4Element) of

            (Just a, Just b, Just c, Just d) ->
              push (f a b c d) (iterate (index - 1))

            _ -> empty

  in iterate (maxIterations - 1)


map5 : (a -> b -> c -> d -> e -> result) ->
       Array a -> Array b -> Array c -> Array d -> Array e -> Array result
map5 f array1 array2 array3 array4 array5 =
  let maxIterations = min (length array1) <|
                      min (length array2) <|
                      min (length array3) <|
                      min (length array4) (length array5)

      iterate index =
        let array1Element = get index array1
            array2Element = get index array2
            array3Element = get index array3
            array4Element = get index array4
            array5Element = get index array5
        in
          case (array1Element,
                array2Element,
                array3Element,
                array4Element,
                array5Element) of

              (Just a, Just b, Just c, Just d, Just e) ->
                push (f a b c d e) (iterate (index - 1))

              _ -> empty

  in iterate (maxIterations - 1)


{-| `zip` takes two arrays and returns an array of corresponding pairs.

    zip (fromList [1,2,3]) (fromList [2,3,4]) ==
      fromList [(1,2), (2,3), (3,4)]
-}
zip : Array a -> Array b -> Array (a,b)
zip = map2 (,)


{-| `zip3` takes three arrays and returns an array of triples.
Analogous to `zip`.
-}
zip3 : Array a -> Array b -> Array c -> Array (a,b,c)
zip3 = map3 (,,)


{-| `zip4` takes four arrays and returns an array of quadruples.
Analogous to `zip`.
-}
zip4 : Array a -> Array b -> Array c -> Array d -> Array (a,b,c,d)
zip4 = map4 (,,,)


{-| `zip5` takes five arrays and returns an array of quintuples.
Analogous to `zip`.
-}
zip5 : Array a -> Array b -> Array c -> Array d -> Array e -> Array (a,b,c,d,e)
zip5 = map5 (,,,,)


{-|`unzip` transforms an array of pairs into an array of first
components and an array of second components.
-}
unzip : Array (a,b) -> (Array a, Array b)
unzip array =
  (map fst array, map snd array)


{-|`unzip3` takes an array of triples and returns three arrays.
Analogous to `unzip`.
-}
unzip3 : Array (a,b,c) -> (Array a, Array b, Array c)
unzip3 array =
  let first  (x,y,z) = x
      second (x,y,z) = y
      third  (x,y,z) = z
  in
    (map first array, map second array, map third array)


{-|`unzip4` takes an array of quadruples and returns four arrays.
Analogous to `unzip`.
-}
unzip4 : Array (a,b,c,d) -> (Array a, Array b, Array c, Array d)
unzip4 array =
  let first  (a,b,c,d) = a
      second (a,b,c,d) = b
      third  (a,b,c,d) = c
      fourth (a,b,c,d) = d
  in
    (map first array, map second array, map third array, map fourth array)


{-|`unzip5` takes an array of quintuples and returns five arrays.
Analogous to `unzip`.
-}
unzip5 : Array (a,b,c,d,e) -> (Array a, Array b, Array c, Array d, Array e)
unzip5 array =
  let first  (a,b,c,d,e) = a
      second (a,b,c,d,e) = b
      third  (a,b,c,d,e) = c
      fourth (a,b,c,d,e) = d
      fifth  (a,b,c,d,e) = e
  in
    (map first  array,
     map second array,
     map third  array,
     map fourth array,
     map fifth  array)


foldlSafe : (a -> a -> a) -> Array a -> Maybe a
foldlSafe reducer array =
  case head array of
    Nothing -> Nothing
    Just x  -> Just <| foldl reducer x (tail array)


foldrSafe : (a -> a -> a) -> Array a -> Maybe a
foldrSafe reducer array =
  case head array of
    Nothing -> Nothing
    Just x  -> Just <| foldr reducer x (tail array)


sum : Array number -> number
sum = foldl (+) 0

product : Array number -> number
product = foldl (*) 1


maximum : Array comparable -> Maybe comparable
maximum = foldlSafe max

minimum : Array comparable -> Maybe comparable
minimum = foldlSafe min


{-| Sort values from lowest to highest

    sort (fromList [3,1,5]) == fromList [1,3,5]
-}
sort : Array comparable -> Array comparable
sort = sortWith compare


{-| Sort values by a derived property.

    alice = { name="Alice", height=1.62 }
    bob   = { name="Bob"  , height=1.85 }
    chuck = { name="Chuck", height=1.76 }

    sortBy .name   (fromList [chuck,alice,bob]) == fromList [alice,bob,chuck]
    sortBy .height (fromList [chuck,alice,bob]) == fromList [alice,chuck,bob]

    sortBy String.length (fromList ["mouse","cat"]) == fromList ["cat","mouse"]
-}
sortBy : (a -> comparable) -> Array a -> Array a
sortBy accessor = sortWith (compose2 accessor compare)


{-| Sort values with a custom comparison function.

    sortWith flippedComparison (fromList [1..5]) == fromList [5,4,3,2,1]

    flippedComparison a b =
      case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`
-}
sortWith : (a -> a -> Order) -> Array a -> Array a
sortWith comparisonFunction array =
  case head array of
    Nothing -> empty
    Just first ->
      let lesserFilter x =
            (\l -> comparisonFunction l x == LT ||
                   comparisonFunction l x == EQ)
          greaterFilter x =
            (\l -> comparisonFunction l x == GT)
          last = tail array
          lesser = sortWith comparisonFunction (lesserFilter first last)
          equal = fromList [first]
          greater = sortWith comparisonFunction (greaterFilter first last)
      in lesser `append` equal `append` greater
