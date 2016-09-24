module Definition
  ( LispVal(..)
  , SchemeNumber(..)
  ) where

import Control.Arrow
import Data.Ratio
import Data.Complex


------- Type Definitions -------

data LispVal = LAtom       String
             | LList       [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber     SchemeNumber
             | LString     String
             | LBool       Bool
             | LChar       Char
  deriving (Eq, Read)

data SchemeNumber = SInt      Integer
                  | SDouble   Double
                  | SRational Rational
                  | SComplex  (Complex Double)
  deriving (Eq, Read)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (LString contents)      = "\"" ++ contents ++ "\""
showVal (LChar c)               = [c]
showVal (LAtom name)            = name
showVal (LNumber contents)      = show contents
showVal (LBool bool)            = showLBool bool
  where
    showLBool :: Bool -> String
    showLBool True  = "#t"
    showLBool False = "#f"

showVal (LList contents)        = "(" ++ unwordsList contents ++ ")"
showVal (LDottedList head tail) = "(" ++ unwordsList head ++ " . "
                                      ++ showVal tail     ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show SchemeNumber where
  show (SInt a)      = show a
  show (SDouble a)   = show a
  show (SRational a) = show (numerator a) ++ " / " ++ show (denominator a)
  show (SComplex a)  = show (realPart a)  ++ " + " ++ show (imagPart a) ++ "i"


instance Num SchemeNumber where
  (+)         = addSNum
  (*)         = mulSNum
  abs         = absSNum
  signum      = signSNum
  fromInteger = SInt
  negate      = negSNum

addSNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
addSNum (SInt a) (SInt b)      = SInt      $ a + b
addSNum (SInt a) (SDouble b)   = SDouble   $ fromIntegral a + b
addSNum (SInt a) (SRational b) = SRational $ fromIntegral a + b
addSNum (SInt a) (SComplex b)  = SComplex  $ fromIntegral a + b

addSNum (SDouble a) (SInt b)      = SDouble   $ a + fromIntegral b
addSNum (SDouble a) (SDouble b)   = SDouble   $ a + b
addSNum (SDouble a) (SRational b) = SRational $ fromIntegral (truncate a) + b
addSNum (SDouble a) (SComplex b)  = SComplex  $ (a :+ 0) + b

addSNum (SRational a) (SInt b)      = SRational $ a + fromIntegral b
addSNum (SRational a) (SDouble b)   = SRational $ a + fromIntegral (truncate b)
addSNum (SRational a) (SRational b) = SRational $ a + b
addSNum (SRational a) (SComplex b)  = SComplex  $ calcRatioComplex (+) a b

addSNum (SComplex a) (SInt b)      = SComplex $ a + fromIntegral b
addSNum (SComplex a) (SDouble b)   = SComplex $ a + fromIntegral (truncate b)
addSNum (SComplex a) (SRational b) = SComplex $ calcRatioComplex (+) b a
addSNum (SComplex a) (SComplex b)  = SComplex $ a + b

mulSNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
mulSNum (SInt a) (SInt b)      = SInt      $ a * b
mulSNum (SInt a) (SDouble b)   = SDouble   $ fromIntegral a * b
mulSNum (SInt a) (SRational b) = SRational $ fromIntegral a * b
mulSNum (SInt a) (SComplex b)  = SComplex  $ fromIntegral a * b

mulSNum (SDouble a) (SInt b)      = SDouble   $ a * fromIntegral b
mulSNum (SDouble a) (SDouble b)   = SDouble   $ a * b
mulSNum (SDouble a) (SRational b) = SRational $ fromIntegral (truncate a) * b
mulSNum (SDouble a) (SComplex b)  = SComplex  $ (a :+ 0) * b

mulSNum (SRational a) (SInt b)      = SRational $ a * fromIntegral b
mulSNum (SRational a) (SDouble b)   = SRational $ a * fromIntegral (truncate b)
mulSNum (SRational a) (SRational b) = SRational $ a * b
mulSNum (SRational a) (SComplex b)  = SComplex  $ calcRatioComplex (*) a b

mulSNum (SComplex a) (SInt b)      = SComplex $ a * fromIntegral b
mulSNum (SComplex a) (SDouble b)   = SComplex $ a * fromIntegral (truncate b)
mulSNum (SComplex a) (SRational b) = SComplex $ calcRatioComplex (*) b a
mulSNum (SComplex a) (SComplex b)  = SComplex $ a * b

calcRatioComplex :: Num a
                 => (a -> a -> a)
                 -> Rational
                 -> Complex Double
                 -> Complex Double
calcRatioComplex op ratio complex =
  let fracDbl = rationalToDouble ratio
      rPart   = realPart complex
      iPart   = imagPart complex
  in  rPart * fracDbl :+ iPart * fracDbl

rationalToDouble :: Rational -> Double
rationalToDouble = fromRational

signSNum :: SchemeNumber -> SchemeNumber
signSNum (SComplex a)  = SInt . getSign $ realPart a
signSNum (SInt a)      = SInt $ getSign a
signSNum (SDouble a)   = SInt $ getSign a
signSNum (SRational a) = SInt $ getSign a

getSign :: (Num a, Ord a) => a -> Integer
getSign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

absSNum :: SchemeNumber -> SchemeNumber
absSNum (SInt a)      = SInt $ abs a
absSNum (SDouble a)   = SDouble $ abs a
absSNum (SRational a) = SRational $ abs a
absSNum (SComplex a)  = SComplex $ abs a

negSNum :: SchemeNumber -> SchemeNumber
negSNum (SInt a)      = SInt $ negate a
negSNum (SDouble a)   = SDouble $ negate a
negSNum (SRational a) = SRational $ negate a
negSNum (SComplex a)  = SComplex $ negate a

instance Enum SchemeNumber where
  toEnum = SInt . fromIntegral

  fromEnum (SInt a)      = fromIntegral a
  fromEnum (SDouble a)   = truncate a
  fromEnum (SRational a) = truncate a
  fromEnum (SComplex a)  = truncate $ realPart a

instance Real SchemeNumber where
  toRational (SInt a)      = toRational a
  toRational (SDouble a)   = toRational a
  toRational (SRational a) = a
  toRational (SComplex a)  = toRational (realPart a)

instance Ord SchemeNumber where
  compare = cmpSNum

cmpSNum :: SchemeNumber -> SchemeNumber -> Ordering
cmpSNum (SInt a) (SInt b)      = a              `compare` b
cmpSNum (SInt a) (SDouble b)   = fromIntegral a `compare` b
cmpSNum (SInt a) (SRational b) = toRational a   `compare` b
cmpSNum (SInt a) (SComplex b)  = fromIntegral a `compare` realPart b

cmpSNum (SDouble a) (SInt b)      = a            `compare` fromIntegral b
cmpSNum (SDouble a) (SDouble b)   = a            `compare` b
cmpSNum (SDouble a) (SRational b) = toRational a `compare` b
cmpSNum (SDouble a) (SComplex b)  = a            `compare` realPart b

cmpSNum (SRational a) (SInt b)      = a `compare` fromIntegral b
cmpSNum (SRational a) (SDouble b)   = a `compare` toRational b
cmpSNum (SRational a) (SRational b) = a `compare` b
cmpSNum (SRational a) (SComplex b)  = a `compare` toRational (realPart b)

cmpSNum (SComplex a) (SInt b)      = realPart a `compare` fromIntegral b
cmpSNum (SComplex a) (SDouble b)   = realPart a `compare` b
cmpSNum (SComplex a) (SRational b) = toRational (realPart a) `compare` b
cmpSNum (SComplex a) (SComplex b)  = realPart a `compare` realPart b

instance Integral SchemeNumber where
  quotRem = quotRemSNum

  toInteger (SInt a)      = a
  toInteger (SDouble a)   = truncate a
  toInteger (SRational a) = truncate a
  toInteger (SComplex a)  = truncate $ realPart a

quotRemSNum :: SchemeNumber -> SchemeNumber -> (SchemeNumber, SchemeNumber)
quotRemSNum = curry
  $ (toInteger *** toInteger)
  >>> uncurry quotRem
  >>> (SInt *** SInt)

instance Fractional SchemeNumber where
  fromRational = SRational
  (/)          = fracDivSNum

fracDivSNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
fracDivSNum (SInt a) (SInt b)      = SDouble   $ fromIntegral a / fromIntegral b
fracDivSNum (SInt a) (SDouble b)   = SDouble   $ fromIntegral a / b
fracDivSNum (SInt a) (SRational b) = SRational $ fromIntegral a / b
fracDivSNum (SInt a) (SComplex b)  = SComplex  $ fromIntegral a / b

fracDivSNum (SDouble a) (SInt b)      = SDouble   $ a / fromIntegral b
fracDivSNum (SDouble a) (SDouble b)   = SDouble   $ a / b
fracDivSNum (SDouble a) (SRational b) = SRational $ toRational a / b
fracDivSNum (SDouble a) (SComplex b)  = SComplex  $ (a :+ 0) / b

fracDivSNum (SRational a) (SInt b)      = SRational $ a / fromIntegral b
fracDivSNum (SRational a) (SDouble b)   = SRational $ a / toRational b
fracDivSNum (SRational a) (SRational b) = SRational $ a / b
fracDivSNum (SRational a) (SComplex b)  = SComplex  $ fromRational a / b

fracDivSNum (SComplex a) (SInt b)      = SComplex $ a / fromIntegral b
fracDivSNum (SComplex a) (SDouble b)   = SComplex $ a / (b :+ 0)
fracDivSNum (SComplex a) (SRational b) = SComplex $ a / fromRational b
fracDivSNum (SComplex a) (SComplex b)  = SComplex $ a / b
