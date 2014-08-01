{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.SemverCheck where

import Control.Monad (when, foldM)
import Control.Monad.Error (ErrorT, noMsg, strMsg, Error, throwError)
import Control.Monad.Trans (lift)
import Data.Aeson hiding (Number)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map

data VersionError
  = NotSuccessor Int Int
  | NotZero Int
  | SameVersions
  | TooLongVersion
  deriving (Show)

data IndexPos
  = Major
  | Minor
  | Patch
  deriving (Show)

showIndexPos :: IndexPos -> String
showIndexPos x = map toLower (show x) ++ " position"

buildIndexPos :: Int -> Either (Int, VersionError) IndexPos
buildIndexPos x =
  case x of
    0 -> Right Major
    1 -> Right Minor
    2 -> Right Patch
    _ -> Left (x, TooLongVersion)

-- | Check two version numbers to see whether one of them follows another
--   If that's the case, return an index in version number when increment
--   takes place. If that's not the case, return an error with position
--   where that error happened
immediateNext :: [Int] -> [Int] -> Either (Int, VersionError) IndexPos
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
            [] -> buildIndexPos result
            (_, 0) : rest -> checkZeros result (pos + 1) rest
            (_, y) : _ -> Left (pos, NotZero y)

data Compatibility
  = Incompatible
  | Compatible
  | Same
  deriving (Eq, Ord)

type Var = String

data NonCorrespondence
  = DifferentRenaming Var Var Var
  | DifferentTypes
  | DifferentRecordExtensions
  | ParsingError
  | StringError String

instance Error NonCorrespondence where
  noMsg = StringError "Unknown error"
  strMsg = StringError

data VariableType
  = Comparable
  | Appendable
  | Number
  | Regular
  deriving (Eq, Show)

isSpecial :: String -> String -> Bool
isSpecial prefix str =
  let n = length prefix
  in (prefix `isPrefixOf` str) && (all (== '\'') $ drop n str)

variableType :: String -> VariableType
variableType str
  | isSpecial "comparable" str = Comparable
  | isSpecial "appendable" str = Appendable
  | isSpecial "number" str = Number
  | otherwise = Regular

isCompatible :: VariableType -> VariableType -> Compatibility
isCompatible ty1 ty2 =
  case (ty1, ty2) of
    (Regular, Regular) -> Same
    (_, Regular) -> Compatible
    (x, y) | x == y -> Same
           | otherwise -> Incompatible

type VarRenaming = Map Var Var
type RenameContext = ErrorT NonCorrespondence Parser

buildRenaming :: VarRenaming -> (Value, Value) -> RenameContext VarRenaming
buildRenaming env (v1, v2) =
  case (v1, v2) of
    (Object o1, Object o2) ->
      let extract :: FromJSON a => Text -> RenameContext (a, a)
          extract tag =
            do v1 <- lift (o1 .: tag)
               v2 <- lift (o2 .: tag)
               return (v1, v2)
          assert cond = when (not cond) $ throwError DifferentTypes
      in
      do (tag1 :: String, tag2) <- extract "tag"
         when (tag1 /= tag2) $ throwError DifferentTypes
         case tag1 of
           "var" ->
             do (var1, var2) <- extract "name"
                case Map.lookup var1 env of
                  Just rvar ->
                    case (rvar == var2) of
                      True -> return env
                      False -> throwError $ DifferentRenaming var1 var2 rvar
                  Nothing ->
                    return (Map.insert var1 var2 env)

           "function" ->
             do (ts1, ts2) <- extract "args"
                (r1, r2) <- extract "result"

                assert (length ts1 == length ts2)
                env1 <- foldM buildRenaming env (zip ts1 ts2)
                buildRenaming env1 (r1, r2)

           "adt" ->
             do (n1 :: String, n2) <- extract "name"
                assert (n1 == n2)

                (a1, a2) <- extract "args"
                assert (length a1 == length a2)
                foldM buildRenaming env (zip a1 a2)

           "alias" ->
             do (n1 :: String, n2) <- extract "alias"
                assert (n1 == n2)
                return env

           "record" ->
             do (ext1, ext2) <- extract "extensions"
                env1 <-
                  case (ext1, ext2) of
                    (Nothing, Nothing) -> return env
                    (Just v1, Just v2) -> buildRenaming env (v1, v2)
                    _ -> throwError DifferentTypes

                (fs1 :: [(String, Value)], fs2) <- extract "field"
                assert (length fs1 == length fs2)
                let map2 = Map.fromList fs2
                    f e (name, val) =
                      case Map.lookup name map2 of
                        Just val2 -> buildRenaming e (val, val2)
                        Nothing -> throwError DifferentTypes
                foldM f env1 fs1

           _ -> fail $ "Unknown tag " ++ tag1

    _ -> fail "Trying to parse types from non-objects"
