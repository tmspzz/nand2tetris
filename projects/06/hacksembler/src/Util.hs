{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( isAInstruction
  , isCInstruction
  , isLabel
  , aInstructionToBinary
  , predefinedSymbolTable
  ) where

import RIO 
import qualified RIO.Text as T
import qualified RIO.Partial as RIO'
import qualified RIO.List.Partial as L'
import qualified RIO.List as L
import qualified RIO.HashMap as HM


isAInstruction :: T.Text -> Bool
isAInstruction t = "@" `T.isPrefixOf` t

isLabel :: T.Text -> Bool
isLabel t = "(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t

isCInstruction :: T.Text -> Bool
isCInstruction t = (not . isAInstruction $ t) && (not . isLabel $ t)

aInstructionToBinary :: T.Text -> T.Text
aInstructionToBinary = T.justifyRight 16 '0' . T.reverse . toBinaryText . T.drop 1

toBinary :: Int -> [Int]
toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

toBinaryText :: T.Text -> T.Text
toBinaryText = toString . toBinary . RIO'.read . T.unpack
  where 
    toString = L.foldl' (\text digit -> T.cons (L'.head . show $ digit) text ) "" 

predefinedSymbolTable :: HM.HashMap T.Text T.Text
predefinedSymbolTable = HM.fromList 
  [ ("R0", "0")
  , ("R1", "1")
  , ("R2", "2")
  , ("R3", "3")
  , ("R4", "4")
  , ("R5", "5")
  , ("R6", "6")
  , ("R7", "7")
  , ("R8", "8")
  , ("R9", "9")
  , ("R10", "10")
  , ("R11", "11")
  , ("R12", "12")
  , ("R13", "13")
  , ("R14", "14")
  , ("R15", "15")
  , ("SCREEN", "16384")
  , ("KBD", "24576")
  , ("SP", "0")
  , ("LCL", "1")
  , ("ARG", "2")
  , ("THIS", "3")
  , ("THAT", "4")
  ]

