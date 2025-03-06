{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
import Prelude hiding (readFile, writeFile, lines, unlines)
import Basic
import RustServer
import RustClient
import ProofKernelInterface (functionCInterface)
import ProofKernelCHeader (proofKernelCHeader)
import System.FilePath
import Data.Text (Text, lines, unlines)
import qualified Data.Text as DT
import Data.Text.IO (readFile, writeFile)
import Text.Regex
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter (indent, layoutPretty, defaultLayoutOptions)
import Text.RawString.QQ
import Prettyprinter (pretty)


xStarPath :: FilePath
xStarPath :: FilePath = "/home/yhtq/学习/x-star"
-- rpcClientPath = "/home/yhtq/学习/x-star/lcf_server/hol-rpc/src/interface_client.rs"
rpcClientPath :: FilePath
rpcClientPath = xStarPath </> "lcf_server/hol-rpc/src/interface_client.rs"
rpcServerPath :: FilePath
rpcServerPath = xStarPath </> "lcf_server/hol-rpc/src/interface_server.rs"
rpcInterfacePath :: FilePath
rpcInterfacePath = xStarPath </> "lcf_server/hol-rpc/src/lib.rs"
proofKernelInterfacePath :: FilePath
proofKernelInterfacePath = xStarPath </> "lcf_server/proof-kernel/src/lib.rs"
proofKernelCHeaderPath :: FilePath
proofKernelCHeaderPath = xStarPath </> "lcf_server/includes/proof-kernel.h"

countingLeadingSpace :: Text -> Int
countingLeadingSpace t = DT.length $ DT.takeWhile (== ' ') t
guidanceString :: Regex
guidanceString = mkRegex "The followings are auto generated"
printToLoc :: D -> [Text] -> [Text]
printToLoc _ [] = []
-- if line x contains guidanceString, then insert content d after x
printToLoc d (x:xs) = case matchRegex guidanceString (show x) of
    Nothing -> x : printToLoc d xs
    Just _ -> let indentCount = countingLeadingSpace x in
        x : renderStrict (layoutPretty defaultLayoutOptions (indent indentCount d)) : xs

printerIO :: FilePath -> D -> IO ()
printerIO path d = do
    content <- readFile path
    let contentLines = lines content
    let newContent = unlines $ printToLoc d contentLines
    writeFile path newContent


main :: IO ()
main = do
    let int_ring = ConversionRule {
        name = "int_ring",
        nameOCaml = Just "INT_RING",
        doc = ["Ring decision procedure instantiated to integers. "],
        docShort = Nothing,
        args = [TermArg],
        returnType = [TheoremReturn]
    }
    let num_exp_conv = ConversionRule {
        name = "num_exp_conv",
        nameOCaml = Just "NUM_EXP_CONV",
        docShort = Just ["Proves what the exponential of two natural number numerals is."],
        doc = map pretty $ lines [r|
SYNOPSIS
    Proves what the exponential of two natural number numerals is.

DESCRIPTION
    If n and m are numerals (e.g. 0, 1, 2, 3,...), then NUM_EXP_CONV `n EXP m` returns the theorem:

    |- n EXP m = s

    where s is the numeral that denotes the natural number denoted by n raised to the power of the one denoted by m.

FAILURE CONDITIONS
    NUM_EXP_CONV tm fails if tm is not of the form `n EXP m`, where n and m are numerals.

EXAMPLE

    # NUM_EXP_CONV `2 EXP 64`;;
    val it : thm = |- 2 EXP 64 = 18446744073709551616

    # NUM_EXP_CONV `1 EXP 99`;;
    val it : thm = |- 1 EXP 99 = 1

    # NUM_EXP_CONV `0 EXP 0`;;
    val it : thm = |- 0 EXP 0 = 1

    # NUM_EXP_CONV `0 EXP 10000`;;
    val it : thm = |- 0 EXP 10000 = 0
        |] ,
        args = [TermArg],
        returnType = [TheoremReturn]
    }
    let 
    let printRule rule = do
            printerIO rpcClientPath $ functionRustClient rule
            printerIO rpcServerPath $ functionRustServer rule
            printerIO proofKernelInterfacePath $ functionCInterface rule
            printerIO proofKernelCHeaderPath $ proofKernelCHeader rule
            printerIO rpcInterfacePath $ declarationRustInterface rule 
    printRule int_ring
    printRule num_exp_conv
    -- print $ countingLeadingSpace "    // The followings are auto generated"

    -- showRule rule1
    -- showRule rule2
    -- showRule rule3
    -- showRule rule4
