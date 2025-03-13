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
import qualified Data.Text as T 
import Prettyprinter (Doc, Pretty (pretty), concatWith, comma, (<+>), parens, semi)
import Data.Yaml (FromJSON(..), (.:), (.:?), Parser, Value)
import Data.Aeson.Types (explicitParseField, explicitParseFieldMaybe)
import qualified Data.Yaml as Y



type D = Doc Text

-- newtype MyD = MyD { unMyD :: D }

-- instance FromJSON MyD where
--     parseJSON v = do
--       text :: Text <- parseJSON v
--       return $ MyD (pretty text) 
data ArgsType = TheoremArg
    | TermArg
    | StringArg
    | ConversionArg
  deriving (Show, Eq, Ord)

instance FromJSON ArgsType where
    parseJSON (Y.String v) = do
        case v of
            "Theorem" -> return TheoremArg
            "Term" -> return TermArg
            "String" -> return StringArg
            "Conversion" -> return ConversionArg
            _ -> fail "Invalid ArgsType"
    parseJSON _ = fail "Invalid ArgsType"
data ReturnType = TheoremReturn
    | TermReturn
    | String
    | Type
    -- | Void
    | Bool
    | Conversion
  deriving (Show, Eq, Ord)

instance FromJSON ReturnType where
    parseJSON (Y.String v) = do
        case v of
            "Theorem" -> return TheoremReturn
            "Term" -> return TermReturn
            "String" -> return String
            "Type" -> return Type
            -- "Void" -> return Void
            "Bool" -> return Bool
            "Conversion" -> return Conversion
            _ -> fail "Invalid ReturnType"
    parseJSON _ = fail "Invalid ReturnType"

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

parseDs :: Value -> Parser [D]
parseDs (Y.String v) = return $ map pretty $ T.lines v
parseDs _ = fail "Invalid D"

-- doc :: ConversionRule -> [D]
-- doc a = map unMyD (docInternal a)

-- docShort :: ConversionRule -> Maybe [D]
-- docShort a = fmap (map unMyD) (docShortInternal a)
instance FromJSON ConversionRule where
    parseJSON (Y.Object v) = ConversionRule
        <$> v .: "name"
        <*> v .:? "nameOCaml"
        <*> explicitParseField parseDs v "doc"
        <*> explicitParseFieldMaybe parseDs v "docShort"
        <*> v .: "args"
        <*> v .: "returnType"
    parseJSON _ = fail "Invalid ConversionRule"

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