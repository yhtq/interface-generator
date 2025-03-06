{-# LANGUAGE OverloadedStrings #-}

module ProofKernelCHeader where
import Basic
import Prettyprinter (parens, pretty, vsep, semi)

returnTypesC :: ConversionRule -> D
returnTypesC cr = case returnType cr of
    [] -> "void "   -- we keep the space here to be consistent `const char *a`
    [Bool] -> "bool "
    [String] -> "const char *"
    [Type] -> "const struct Gc_Type *"
    [TheoremReturn] -> "const struct Gc_Theorem *"
    [TermReturn] -> "const struct Gc_Term *"
    [Conversion] -> "const struct Gc_Conversion *"
    _ -> undefined

argTypeToC :: ArgsType -> D
argTypeToC TheoremArg = "const struct Gc_Theorem *"
argTypeToC TermArg = "const struct Gc_Term *"
argTypeToC ConversionArg = "const struct Gc_Conversion *"
argTypeToC StringArg = "const char *"

-- docToCDoc :: [D] -> D
-- docToCDoc [] = mempty
-- docToCDoc ds = vsep $ "/**" : map (" * " <>) ds ++ [" */"]

proofKernelCHeader :: ConversionRule -> D
proofKernelCHeader cr =
    let argList = concatComma $ zipWith (flip (<>)) (argsToNamesWithIndex $ args cr) (map argTypeToC $ args cr) in
    vsep $ cDoc cr ++ [
        
        returnTypesC cr <> pretty (name cr) <> parens argList <> semi
    ]

