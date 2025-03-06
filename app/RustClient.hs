{-# LANGUAGE OverloadedStrings #-}

module RustClient where
import Basic
import Prettyprinter (concatWith, (<+>), parens, pretty, vsep, group)
import RustServer (bracesIndent)

argTypeToRustArgType :: ArgsType -> D
argTypeToRustArgType t = case t of
    TheoremArg -> "&Theorem"
    TermArg -> "&Term"
    ConversionArg -> "&Conversion"
    StringArg -> "String"

returnTypeToRustReturnType :: ReturnType -> D
returnTypeToRustReturnType t = case t of
    TheoremReturn -> "Theorem"
    TermReturn -> "Term"
    Conversion -> "Conversion"
    String -> "String"
    Type -> "Type"
    Bool -> "bool"

arsList :: ConversionRule -> D
arsList cr =
    let argsWithType = zip (argsToNamesWithIndex $ args cr) (map argTypeToRustArgType $ args cr) in
    "&self," <+> concatWith (\x y -> x <> ", " <> y) (map (\(nameD, typeD) -> nameD <> ":" <+> typeD) argsWithType)

returnTypeR :: ConversionRule -> D
returnTypeR cr =
    let returns = returnType cr in
    let returnsList = map returnTypeToRustReturnType returns in
    case returnsList of
        [] -> "Result<()>"
        [h] -> "Result<" <> h <> ">"
        _ -> "Result<" <> parens (concatWith (\x y -> x <> ", " <> y) returnsList) <> ">"

returnTypesToNamesList :: ConversionRule -> D
returnTypesToNamesList cr = concatWith (\x y -> x <> ", " <> y) $ returnTypesToNames $ returnType cr

executeTransformation :: D -> ArgsType -> D
executeTransformation name ty =
    let suffix = case ty of
            TheoremArg -> ".key"
            TermArg -> ".key"
            ConversionArg -> ".key"
            StringArg -> mempty
    in name <> suffix

returnTransformation :: D -> ReturnType -> D
returnTransformation name ty = case ty of
    TheoremReturn -> "Theorem::new" <> parens (name <> ", self.clone()")
    TermReturn -> "Term::new" <> parens (name <> ", self.clone()")
    Type -> "Type::new" <> parens (name <> ", self.clone()")
    Conversion -> "Conversion::new" <> parens (name <> ", self.clone()")
    String -> name
    Bool -> name

executeLet :: ConversionRule -> D
executeLet cr =
    let vars = returnTypesToNamesList cr in
    -- let (prefix, suffix) = case length $ returnType cr of
    --         0 -> (mempty :: D, "?" :: D)
    --         1 -> ("let" <+> vars <+> "=", "??")
    --         _ -> ("let" <+> parens vars <+> "=", "??") in
    let (prefix, suffix) = case returnType cr of
            [] -> (mempty :: D, "?;" :: D)
            [_] -> ("let" <+> vars <+> "= ", "??;")
            _ -> ("let" <+> parens vars <+> "= ", "??;") in
    let argKeys = concatComma $ "context::current()" : zipWith executeTransformation
            (argsToNamesWithIndex $ args cr) (args cr) in
    group (prefix <> "self.execute" <> parens (
            "self.interface()." <> pretty (name cr) <> parens argKeys
    ) <> suffix)
returnExp :: ConversionRule -> D
returnExp cr =
    let returns = case returnType cr of 
            [] -> "()"
            _ -> concatComma $ zipWith returnTransformation (returnTypesToNames $ returnType cr) (returnType cr) in
    group (("Ok" :: D) <> parens returns)
functionRustClient :: ConversionRule -> D
functionRustClient cr = 
    let decl = ("pub fn " :: D) <> pretty (name cr) <> parens (arsList cr) <+> "->" <+> returnTypeR cr in
    vsep $ rustDocShort cr ++ [
        bracesIndent decl $ vsep [
            executeLet cr,
            returnExp cr
        ]
    ]
