{-# LANGUAGE OverloadedStrings #-}
module Basic (
  ConversionRule(..),
  ArgsType(..),
  ReturnType(..),
  D,
  argTypeToName,
  argsToNamesWithIndex,
  returnTypeUniv,
  returnTypesToNames,
  concatComma,
  functionCall,
  letExp,
  rustDoc,
  rustDocShort,
  cDoc,
  cDocShort
) where
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), concatWith, comma, (<+>), parens, semi)


type D = Doc Text
data ArgsType = TheoremArg
    | TermArg
    | StringArg
    | ConversionArg
  deriving (Show, Eq, Ord)
data ReturnType = TheoremReturn
    | TermReturn
    | String
    | Type
    -- | Void
    | Bool
    | Conversion
  deriving (Show, Eq, Ord)
returnTypeUniv :: [ReturnType]
returnTypeUniv = [TheoremReturn, TermReturn, String, Type, Bool]
data ConversionRule = ConversionRule {
  name :: Text,
  nameOCaml :: Maybe Text,
  doc :: [D], -- each element is a line of documentation
  docShort :: Maybe [D], -- short documentation
  args :: [ArgsType],
  returnType :: [ReturnType]
}
argTypeToName :: ArgsType -> D
argTypeToName x = case x of
    TheoremArg -> "th"
    TermArg -> "tm"
    StringArg -> "s"
    ConversionArg -> "conv"
argsToNamesWithIndex :: [ArgsType] -> [D]
argsToNamesWithIndex al = argsToNamesWithIndex1 (zip al [(1 :: Int)..])
          where argsToNamesWithIndex1 = map (\(x, i) ->
                  argTypeToName x <>  pretty i)
returnTypeToName :: ReturnType -> D
returnTypeToName x = case x of
    TheoremReturn -> "th"
    TermReturn -> "tm"
    String -> "s"
    Type -> "ty"
    -- Void -> "()"
    Bool -> "result"
    Conversion -> "conv"

returnTypesToNames :: [ReturnType] -> [D]
returnTypesToNames rts =
    let names = map returnTypeToName rts in
      zipWith (\x y -> x <> pretty y) names [(1 :: Int)..]

concatComma :: [D] -> D
concatComma = concatWith (\x y -> x <> comma <+> y)

functionCall :: D -> [D] -> D
functionCall name args = name <> parens (concatComma args)

letExp :: D -> D -> D
letExp name value = "let" <+> name <+> "=" <+> value <> semi

rustDoc :: ConversionRule -> [D]
rustDoc cr = map ("///" <+>) (doc cr)

rustDocShort :: ConversionRule -> [D]
rustDocShort cr = case docShort cr of
    Just ds -> map ("///" <+>) ds
    Nothing -> rustDoc cr

cDoc :: ConversionRule -> [D]
cDoc cr = "/**" : map (" * " <>) (doc cr) ++ [" */"]

cDocShort :: ConversionRule -> [D]
cDocShort cr = case docShort cr of
    Just ds -> "/**" : map (" * " <>) ds ++ [" */"]
    Nothing -> cDoc cr