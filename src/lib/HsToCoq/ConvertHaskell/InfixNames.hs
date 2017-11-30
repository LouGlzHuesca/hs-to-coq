{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module HsToCoq.ConvertHaskell.InfixNames (
  identIsVariable,
  infixToPrefix, toPrefix, toLocalPrefix,
  infixToCoq, toCoqName,
  identIsOp, identToOp,
  canonicalName,
  splitModule -- a bit out of place here. oh well.
  ) where

import Control.Lens

import Control.Applicative
import Data.Semigroup (Semigroup(..))
import Data.Maybe
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding ((<|>), many)

import Encoding (zEncodeString, zDecodeString)

import GHC.Stack

--------------------------------------------------------------------------------

-- Lets keep this module self-contained (but use the same type synonyms)
type Op = T.Text
type Ident = T.Text
type ModuleIdent = T.Text
type AccessIdent = T.Text

identIsVariable_ :: Text -> Bool
identIsVariable_ = T.uncons <&> \case
  Just (h,t) -> (isAlpha h || h == '_') && T.all (\c -> isAlphaNum c || c == '_' || c == '\'') t
  Nothing    -> False

identIsVariable :: Text -> Bool
identIsVariable = all identIsVariable_ . T.splitOn "."

-- An operator's user-facing name in Coq (a notation)
infixToPrefix :: Op -> Ident
infixToPrefix = ("_" <>) . (<> "_")

toPrefix :: Ident -> Ident
toPrefix x | identIsVariable x = x
           | otherwise         = infixToCoq x

toLocalPrefix :: Ident -> Ident
toLocalPrefix x | identIsVariable x = x
                | otherwise         = "l" <> infixToCoq x


-- An operator's defined name in Coq (hidden by a notation)
infixToCoq_ :: Op -> Ident
infixToCoq_ name = "op_" <> T.pack (zEncodeString $ T.unpack name) <> "__"

-- This is code smell: Why do we return an unstructured Ident, and not a QualId?
infixToCoq :: HasCallStack => Op -> Ident
infixToCoq op = case splitModule op of
    Just (m,op) -> m <> "." <> infixToCoq_ op
    Nothing     -> infixToCoq_ op

splitModule :: Ident -> Maybe (ModuleIdent, AccessIdent)
splitModule = either (const Nothing) Just . parse qualid "" where
  qualid = do
    let modFrag = T.cons <$> upper <*> (T.pack <$> many (alphaNum <|> char '\''))
    mod <- T.intercalate "." <$> many1 (try (modFrag <* char '.'))
    base <- T.pack <$> some anyChar -- since we're assuming we get a valid name
    pure $ (mod, base)

toCoqName :: Op -> Ident
toCoqName x | identIsVariable x = x
            | otherwise         = infixToCoq x

canonicalName :: Op -> Ident
canonicalName x
  | identIsVariable x = x
  | otherwise         = infixToCoq . fromMaybe x $ T.stripPrefix "_" =<< T.stripSuffix "_" x

identIsOp :: Ident -> Bool
identIsOp t = "op_" `T.isPrefixOf` t && "__" `T.isSuffixOf` t
    -- the next clause is a work-around as long as the dict accessors are named
    -- op_...____ – these do not have notations
    && not ("____" `T.isSuffixOf` t)

identToOp :: Ident -> Maybe Op
identToOp t
   | identIsOp t = Just $ T.pack (zDecodeString (T.unpack (T.drop 3 (T.dropEnd 2 t))))
   | otherwise   = Nothing
