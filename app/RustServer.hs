{-# LANGUAGE OverloadedStrings #-}

module RustServer where
import Data.Text (Text)
import Data.Map (Map, insert, lookup, fromList)
import Prettyprinter qualified as PP
import Prettyprinter (Doc, (<+>), indent, semi, braces, parens, Pretty(..), vsep, group)
import Basic
import Control.Monad.State (State, get, put)
import Control.Monad ()
import Control.Monad.State.Lazy (runState)
import Data.Maybe (isJust)

bracesIndent :: D -> D -> D
bracesIndent pre d =
    vsep [
        pre <+> "{",
        indent 4 d,
        "}"
    ]
typeToTypeKey :: ReturnType -> D
typeToTypeKey t = case t of
    TheoremReturn -> "TheoremKey"
    TermReturn -> "TermKey"
    String -> "String"
    Type -> "TypeKey"
    -- Void -> "()"
    Bool -> "bool"
    Conversion -> "ConversionKey"
argToTypeKey :: ArgsType -> D
argToTypeKey t = case t of
    TheoremArg -> "TheoremKey"
    TermArg -> "TermKey"
    StringArg -> "String"
    ConversionArg -> "ConversionKey"
argToContainerName :: ArgsType -> Maybe D
argToContainerName t = case t of
    TheoremArg -> Just "theorems"
    TermArg -> Just "terms"
    _ -> Nothing
returnToContainerName :: ReturnType -> Maybe D
returnToContainerName t = case t of
    TheoremReturn -> Just "theorems"
    TermReturn -> Just "terms"
    Type -> Just "types"
    _ -> Nothing
argsToNameTypeKey :: [ArgsType] -> [(D, D)]
argsToNameTypeKey al = zip (argsToNamesWithIndex al) (map argToTypeKey al)

argsListR :: ConversionRule -> Doc Text
argsListR cr =
    let if_mut = if any (isJust . returnToContainerName) (returnType cr) then "mut " else "" in
    if_mut <> "self, _ctx: Context, " <>
        concatComma (map (\(nameD, typeD) ->
                nameD <> PP.colon <+> typeD
            )
            (argsToNameTypeKey $ args cr))


returnTypeR :: ConversionRule -> Doc Text
returnTypeR cr =
    let returns = returnType cr in
    let returnsList = map typeToTypeKey returns in
    case returns of
        [] -> "Result<()>"
        [_] -> "Result<" <> concatComma returnsList <> ">"
        _ -> "Result<" <> parens (concatComma returnsList) <> ">"

loadDynFunctionR :: ConversionRule -> D
loadDynFunctionR cr = 
    let maybeAs = case nameOCaml cr of
            Just n -> n <> " as " 
            Nothing -> mempty in
    group ("load_dyn_function!" <> parens (pretty (maybeAs <> name cr)) <> semi)

type Context = (Map ArgsType (D, Bool), Int)
defaultContext :: (Map ArgsType (D, Bool), Int)
defaultContext = (Data.Map.fromList [
    (TheoremArg, (argTypeToName TheoremArg, False)),
    (TermArg, (argTypeToName TermArg, False)),
    (StringArg, (argTypeToName StringArg, False))
    ], 0)
prepareArgsMR :: ConversionRule -> D
prepareArgsMR cr =
    let get_name_update n = do
            (ctx, index) <- get
            res <- case Data.Map.lookup n ctx of
                Just (t, _) -> return t
                Nothing -> undefined
            put (insert n (res, True) ctx, index)
            return res in
    -- let typeToContainerName t = case t of
    --         TheoremArg -> "theorems"
    --         TermArg -> "terms"
    --         StringArg -> undefined in
    -- let get_index_update = do
    --         (ctx, index) <- get
    --         put (ctx, index + 1)
    --         return index in
    let get_sentencesM :: State Context [D] = mapM (\(nameD, typeK) -> do
            _ <- get_name_update typeK
            let type_container = argToContainerName typeK
            -- index <- get_index_update
            let error_message :: D = "\"invalid" <+> argTypeToName typeK <+> "argument in" <+> pretty (name cr) <> "\""
            case type_container of
                Nothing -> return ""
                Just container ->
                    return $ group ("let" <+> nameD <+> "=" <+> container <> ".get" <> parens nameD <> ".ok_or" <> parens error_message <> "?;")
            -- index <- get_index_update
            -- return $ name <> PP.colon <+> "args[" <> pretty index <> "]"
            ) (zip (argsToNamesWithIndex $ args cr) (args cr)) in
    let run_result = runState get_sentencesM defaultContext in
    let contextResult = fst $ snd run_result in
    let prepareContainer = filterJust $ map (
            \x -> case Data.Map.lookup x contextResult of
                Just (_, True) ->
                    do
                        container <- argToContainerName x
                        return $ group ("let" <+> container <+> "=" <+> "self." <> container <> "();")
                _ -> Nothing
            ) [TheoremArg, TermArg] in
    vsep $ prepareContainer ++ fst run_result

ifDestruct :: ConversionRule -> Bool
ifDestruct cr = length (returnType cr) > 1

dynCallR :: ConversionRule -> D
dynCallR cr =
    let argsNameWithType = zip (argsToNamesWithIndex $ args cr) (args cr) in
    let argsCall = concatComma $ map (\(nameD, typeK) ->
            case typeK of
                StringArg -> "&" <> nameD
                _ -> nameD
            ) argsNameWithType in
    let prefix = if ifDestruct cr then "let token = " else mempty in
    let suffix = if ifDestruct cr then semi else mempty in
    group (prefix <> "unsafe" <+> braces ("self.dyn_call" <> parens (pretty (name cr)<> "," <+> "args!" <> parens argsCall) <> "?") <> suffix)

destructR :: ConversionRule -> Maybe D
destructR cr =
    if not $ ifDestruct cr then Nothing else
        Just $ group ("unsafe { self.destruct::" <> "<" <> pretty (length $ returnType cr) <> ", _>::destruct(token)? }")

callingReturnR :: ConversionRule -> D
callingReturnR cr =
    let returnsList = returnTypesToNames $ returnType cr in
    case returnsList of
        [] -> "_"
        [h] -> h
        _ -> parens $ concatComma returnsList

returnSentenceR :: ConversionRule -> D
returnSentenceR cr =
    let returnsListWithType = zip (returnTypesToNames $ returnType cr) (returnType cr) in
    let returnList1 = map (\(name, ty) -> case ty of
                TheoremReturn -> "theorems.insert" <> parens name
                TermReturn -> "terms.insert" <> parens name
                String -> "unsafe { " <> name <> ".get_str()? }"
                Type -> "types.insert" <> parens name
                Conversion -> "conversions.insert" <> parens name
                Bool -> "unsafe { " <> name <> ".get_bool()? }"
            ) returnsListWithType in
    let res = case returnList1 of
            [] -> "Ok(())"
            [h] -> "Ok" <> parens h
            _ -> "Ok" <> parens (concatComma returnList1) in
    group res

filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust (Nothing:xs) = filterJust xs
filterJust ((Just x):xs) = x : filterJust xs

containerUpdates :: ConversionRule -> [D]
containerUpdates cr =
    filterJust $ map (\t ->
                if elem t $ returnType cr then
                    case returnToContainerName t of
                        Just container -> Just $ group ("let mut" <+> container <+> "=" <+> "self." <> container <> "_mut();")
                        Nothing -> Nothing
                else Nothing
                ) returnTypeUniv


signatureR :: ConversionRule -> D
signatureR cr =
    "async fn" <+> pretty (name cr) <> parens (argsListR cr) <+> "->" <+> returnTypeR cr

functionRustServer :: ConversionRule -> D
functionRustServer cr =
    let signature = signatureR cr in
    let load_dyn_function = loadDynFunctionR cr in
    let prepare_args = prepareArgsMR cr in
    let dyn_call = dynCallR cr in
    let destructs = case destructR cr of
            Just d -> [d]
            Nothing -> []
    in
    vsep $ rustDocShort cr ++ [
        
        bracesIndent signature (vsep $ [
                    load_dyn_function,
                    bracesIndent ("let " <> callingReturnR cr <+> "= ") (vsep ([
                                    prepare_args,
                                    dyn_call
                                ] ++ destructs)) <> semi
                ]
                ++ containerUpdates cr
                ++ [returnSentenceR cr]
            )
    ]

declarationRustInterface :: ConversionRule -> D
declarationRustInterface cr =
    let argList = concatComma (map (\(nameD, typeD) ->
                    nameD <> PP.colon <+> typeD
                )
                (argsToNameTypeKey $ args cr)) in
    let decl = ("async fn" :: D) <+> pretty (name cr) <> parens argList <+> "->" <+> returnTypeR cr <> ";" in
    vsep $ rustDocShort cr ++ [decl]
