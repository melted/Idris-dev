||| Types for interfacing with C.
||| This file should be kept free from IO.
module CFFI.Types

import Data.Vect

%access public export
%default partial

||| An universe of C types.
data CType = I8 | I16 | I32 | I64 | FLOAT | DOUBLE | PTR

||| Composites of C types
data Composite = T CType | ARRAY Int Composite | STRUCT (List Composite) | UNION (List Composite)
                | PACKEDSTRUCT (List Composite)

implicit mkComposite : CType -> Composite
mkComposite x = T x

Show CType where
    show I8 = "I8"
    show I16 = "I16"
    show I32 = "I32"
    show I64 = "I64"
    show FLOAT = "FLOAT"
    show DOUBLE = "DOUBLE"
    show PTR = "PTR"

Show Composite where
    show (T ct) = show ct
    show (ARRAY n t) = "ARRAY " ++ show n ++ " " ++ show t
    show (STRUCT xs) = "STRUCT " ++ show xs
    show (UNION xs) = "UNION " ++ show xs
    show (PACKEDSTRUCT xs) = "PACKEDSTRUCT " ++ show xs

translate : CType -> Type
translate I8 = Bits8
translate I16 = Bits16
translate I32 = Bits32
translate I64 = Bits64
translate FLOAT = Double
translate DOUBLE = Double
translate PTR = Ptr

mutual
    sizeOfCT : CType -> Int
    sizeOfCT I8 = 1
    sizeOfCT I16 = 2
    sizeOfCT I32 = 4
    sizeOfCT I64 = 8
    sizeOfCT FLOAT = 4
    sizeOfCT DOUBLE = 8
    sizeOfCT PTR = prim__sizeofPtr

    sizeOf : Composite -> Int
    sizeOf (T ct) = sizeOfCT ct
    sizeOf (ARRAY n t) = n * sizeOf t
    sizeOf (STRUCT xs) = sizeOfStruct xs
    sizeOf (UNION xs) = foldl (\acc, x =>max acc $ sizeOf x) 0 xs
    sizeOf (PACKEDSTRUCT xs) = foldl (\acc, x => acc + sizeOf x) 0 xs

    alignOfCT : CType -> Int
    alignOfCT I8 = 1
    alignOfCT I16 = 2
    alignOfCT I32 = 4
    alignOfCT I64 = prim__sizeofPtr
    alignOfCT FLOAT = 4
    alignOfCT DOUBLE = prim__sizeofPtr
    alignOfCT PTR = prim__sizeofPtr

    alignOf : Composite -> Int
    alignOf (T t) = alignOfCT t
    alignOf (ARRAY n t) = alignOf t
    alignOf (STRUCT xs) = foldl (\acc, x => max acc $ alignOf x) 0 xs
    alignOf (UNION xs) = foldl (\acc, x => max acc $ alignOf x) 0 xs
    alignOf (PACKEDSTRUCT xs) = 1

    pad : Int -> Int -> Int
    pad pos align = let c = pos `mod` align in if c == 0 then 0 else align - c

    nextOffset : Composite -> Int -> Int
    nextOffset ty pos = pos + pad pos (alignOf ty)

    offsetsStruct : List Composite -> List Int
    offsetsStruct xs = offsets' xs 0
        where
            offsets' : List Composite -> Int -> List Int
            offsets' [] _ = []
            offsets' (x::xs) pos = (nextOffset x pos)::(offsets' xs (nextOffset x pos + sizeOf x))

    sizeOfStruct : List Composite -> Int
    sizeOfStruct xs = sizeStruct' xs 0 1
        where
            sizeStruct' : List Composite -> Int -> Int -> Int
            sizeStruct' [] pos maxAlign = pos + (pad pos maxAlign)
            sizeStruct' (x::xs) pos maxAlign = sizeStruct' xs (nextOffset x pos + sizeOf x)
                                                        (max maxAlign (alignOf x))

fields : Composite -> Nat
fields (STRUCT xs) = length xs
fields (PACKEDSTRUCT xs) = length xs
fields (UNION xs) = length xs
fields (ARRAY n _) = toNat n
fields (T _) = 1

offsetsPacked : List Composite -> List Int
offsetsPacked xs = offsets' xs [] 0
    where
        offsets' : List Composite -> List Int -> Int -> List Int
        offsets' [] acc _ = reverse acc
        offsets' (x::xs) acc pos = offsets' xs (pos::acc) (sizeOf x)

-- TODO: Use index and fix upp proofs.
offset : Composite -> Nat -> Int
offset (STRUCT xs) i = fromMaybe 0 $ index' i (offsetsStruct xs)
offset (PACKEDSTRUCT xs) i = fromMaybe 0 $ index' i (offsetsPacked xs)
offset (ARRAY _ t) i = sizeOf t * toIntNat i
offset (T _) _ = 0

offsets : Composite -> List Int
offsets (STRUCT xs) = offsetsStruct xs
offsets (PACKEDSTRUCT xs) = offsetsPacked xs
offsets (UNION xs) = replicate (length xs) 0
offsets (ARRAY n t) = [ x*sizeOf t | x <- [0..n-1]]
offsets (T _) = [0]

-- TODO: handle out of bounds with proofs
-- Also choose a better name.
select : Composite -> Nat -> Composite
select (STRUCT xs@(y::_)) i = fromMaybe y (index' i xs)
select (PACKEDSTRUCT xs@(y::_)) i = fromMaybe y (index' i xs)
select (UNION xs@(y::_)) i = fromMaybe y (index' i xs)
select (ARRAY n t) _ = t
select t _ = t
