{-# LANGUAGE OverloadedStrings #-}

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
        doc = ["a test function", "see hol-light tutorial for more details"],
        args = [TermArg],
        returnType = [TheoremReturn]
    }
    let printRule rule = do
            printerIO rpcClientPath $ functionRustClient rule
            printerIO rpcServerPath $ functionRustServer rule
            printerIO proofKernelInterfacePath $ functionCInterface rule
            printerIO proofKernelCHeaderPath $ proofKernelCHeader rule
            printerIO rpcInterfacePath $ declarationRustInterface rule 
    printRule int_ring
    -- print $ countingLeadingSpace "    // The followings are auto generated"

    -- showRule rule1
    -- showRule rule2
    -- showRule rule3
    -- showRule rule4
