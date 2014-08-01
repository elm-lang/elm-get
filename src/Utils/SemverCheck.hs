{-# LANGUAGE OverloadedStrings #-}

module Utils.SemverCheck where

import Data.Aeson (Value(..), Object)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM

data VersionError
  = NotSuccessor Int Int
  | NotZero Int
  | SameVersions
  deriving (Show)

type IndexPos = Int

-- | Check two version numbers to see whether one of them follows another
--   If that's the case, return an index in version number when increment
--   takes place. If that's not the case, return an error with position
--   where that error happened
immediateNext :: [Int] -> [Int] -> Either (IndexPos, VersionError) IndexPos
immediateNext prev next = check 0 $ zip (prev ++ repeat 0) next
  where check i ls =
          case ls of
            [] -> Left (i, SameVersions)
            (x, y) : rest
              | x == y -> check (i + 1) rest
              | x + 1 == y -> checkZeros i (i + 1) rest
              | otherwise -> Left (i, NotSuccessor x y)

        checkZeros result pos ls =
          case ls of
            [] -> Right result
            (_, 0) : rest -> checkZeros result (pos + 1) rest
            (_, y) : _ -> Left (pos, NotZero y)

showIndexPos :: IndexPos -> String
showIndexPos x =
  case x of
    0 -> "major position"
    1 -> "minor position"
    2 -> "patch position"
    _ -> "position " ++ show x

data Compatibility
  = Incompatible
  | Compatible
  | Same
  deriving (Eq, Ord)

data NonCorrespondence
  = DifferentRenaming String String String
  | DifferentTypes
  | DifferentRecordExtensions
  | ParsingError

data VariableType
  = Comparable
  | Appendable
  | NumberT -- there is a constructor Number in Data.Aeson, thus "T" suffix
  | Regular
  deriving (Eq, Show)

variableType :: String -> VariableType
variableType str
  | "comparable" `isPrefixOf` str = Comparable
  | "appendable" `isPrefixOf` str = Appendable
  | "number" `isPrefixOf` str = NumberT
  | otherwise = Regular

isCompatible :: VariableType -> VariableType -> Compatibility
isCompatible ty1 ty2 =
  case (ty1, ty2) of
    (Regular, Regular) -> Same
    (_, Regular) -> Compatible
    (x, y) | x == y -> Same
           | otherwise -> Incompatible
