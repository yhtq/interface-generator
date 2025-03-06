{-# LANGUAGE OverloadedStrings #-}

module ProofKernelInterface where
import Basic
import Prettyprinter ((<+>), parens, pretty, vsep, group, comma, semi, braces, space, colon)
import RustServer (bracesIndent)

argTypeToPtrRust :: ArgsType -> D
argTypeToPtrRust TheoremArg = "*const Gc<Theorem>"
argTypeToPtrRust TermArg = "*const Gc<Term>"
argTypeToPtrRust ConversionArg = "*const Gc<Conversion>"
argTypeToPtrRust StringArg = "c_char"

returnTypeToCRust :: ReturnType -> D
returnTypeToCRust TheoremReturn = "*const Gc<Theorem>"
returnTypeToCRust TermReturn = "*const Gc<Term>"
returnTypeToCRust Conversion = "*const Gc<Conversion>"
returnTypeToCRust String = "*const c_char"
returnTypeToCRust Type = "*const Gc<Type>"
returnTypeToCRust Bool = "bool"

ensures :: D -> D
ensures name = "ensures!" <> parens (concatComma ["!" <> name <> ".is_null()",
                                                 "\"" <> "`" <> name <> "` is null\"",
                                                 "std::ptr::null()"]) <> semi

clientPrepare :: D
clientPrepare = "let client = ensures_ok!(get_client(), std::ptr::null());"

argPrepare :: D -> ArgsType -> D
argPrepare name StringArg = vsep $ map group
                                ["let" <+> name <+> "= unsafe" <+> braces (" CStr::from_ptr(" <> name <> ").to_str() ") <> semi,
                                 "let" <+> name <+> "=" <+> "ensures_ok!" <> parens (name <> comma <+> "std::ptr::null()") <> ".to_string()" <> semi
                                 ]
argPrepare name _ = "let" <+> name <+> "= unsafe" <+> braces (" &*" <> name <> space) <> ".as_ref();"

rpcCall :: ConversionRule -> D
rpcCall cr =
    let returnName = case returnTypesToNames $ returnType cr of
            [] -> "c_int"
            [x] -> x
            _ -> undefined in   -- don' t know how to handle multi return types yet 
    -- let calling = functionCall "ensures_ok!" [
    --         functionCall (pretty $ "client." <> name cr) (argsToNamesWithIndex (args cr)),
    --         "std::ptr::null()"
    --         ] in
    let calling = functionCall (pretty $ "client." <> name cr) (argsToNamesWithIndex (args cr)) in
    case returnType cr of
        [] -> vsep $ map group [
                functionCall "ensures_ok!" [calling, "-1"] <> semi,
                "0"
            ]
        [Bool] -> group $ functionCall "ensures_ok!" [calling, "false"]
        [String] -> vsep $ map group [
                letExp returnName $ functionCall "ensures_ok!" [calling, "std::ptr::null()"],
                letExp "result" (functionCall "CString::new" [returnName] <> ".unwrap()"),
                returnName <> ".into_gc()"
            ]
        _ -> vsep $ map group [
                letExp returnName $ functionCall "ensures_ok!" [calling, "std::ptr::null()"],
                returnName <> ".into_gc()"
            ]
functionCInterface :: ConversionRule -> D
functionCInterface cr =
    let argList = zipWith (\nameD typeD -> nameD <> colon <+> typeD) (argsToNamesWithIndex $ args cr) (map argTypeToPtrRust $ args cr) in
    let returns = case returnType cr of
            [] -> "c_int"
            [x] -> returnTypeToCRust x
            _ -> undefined in   -- don' t know how to handle multi return types yet 
    let decl = "pub unsafe extern \"C\" fn" <+> pretty (name cr) <> parens (concatComma argList) <+> "->" <+> returns in
    vsep $
        map ("///" <+>) (doc cr) ++
        [
            "#[no_mangle]",
            bracesIndent decl $ vsep $ [
                "clear_last_error();"
            ]
            ++ zipWith argPrepare (argsToNamesWithIndex $ args cr) (args cr)
            ++ [
                clientPrepare,
                rpcCall cr
            ]
        ]