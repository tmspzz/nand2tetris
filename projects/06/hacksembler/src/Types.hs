{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import RIO
import RIO.Process
import qualified RIO.Text as T


-- | Command line arguments
data Options = Options
  { fileName :: String
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

data CInstruction = CInstruction 
  { destination :: !DEST
  , computation :: !CMP
  , jump :: !JMP
  } deriving (Show, Eq)

data JMP = NullJMP | JGT | JEQ | JGE | JLT | JNE | JLE | JMP deriving (Show, Eq)

jmpFromText :: Maybe T.Text -> JMP
jmpFromText Nothing = NullJMP
jmpFromText (Just "JGT") = JGT
jmpFromText (Just "JEQ") = JEQ
jmpFromText (Just "JGE") = JGE
jmpFromText (Just "JLT") = JLT
jmpFromText (Just "JNE") = JNE
jmpFromText (Just "JLE") = JLE
jmpFromText (Just "JMP") = JMP
jmpFromText (Just jmp) = error $ T.unpack jmp ++ " is not a valid JUMP" 

jmpToBinary :: JMP -> T.Text
jmpToBinary NullJMP = "000"
jmpToBinary JGT = "001"
jmpToBinary JEQ = "010"
jmpToBinary JGE = "011"
jmpToBinary JLT = "100"
jmpToBinary JNE = "101"
jmpToBinary JLE = "110"
jmpToBinary JMP = "111"

data DEST = NullDEST | M | D | MD | A | AM | AD | AMD deriving (Show, Eq)

destFromText :: Maybe T.Text -> DEST
destFromText Nothing = NullDEST
destFromText (Just "M") = M
destFromText (Just "D") = D
destFromText (Just "MD") = MD
destFromText (Just "A") = A
destFromText (Just "AM") = AM
destFromText (Just "AD") = AD
destFromText (Just "AMD") = AMD
destFromText (Just dest) = error $ T.unpack dest ++ " is not a valid DESTINATION register" 

destToBinary :: DEST -> T.Text
destToBinary NullDEST = "000"
destToBinary M = "001"
destToBinary D = "010"
destToBinary MD = "011"
destToBinary A = "100"
destToBinary AM = "101"
destToBinary AD = "110"
destToBinary AMD = "111"

data CMP = C0 | C1 | CMinus1 | CD | CA | CM | CNotD | CNotA | CNotM | CMinusD | CMinusA | CMinusM | CDPlus1 | CAPlus1 | CMPlus1 | CDMinus1 | CAMinus1 | CMMinus1 | CDPlusA | CDPlusM | CDMinusA | CDMinusM | CAMinusD | CMMinusD | CDAndA | CDAndM | CDOrA | CDOrM deriving (Show, Eq)

cmpFromText :: T.Text -> CMP
cmpFromText "0" = C0
cmpFromText "1" = C1
cmpFromText "-1" = CMinus1
cmpFromText "D" = CD
cmpFromText "A" = CA
cmpFromText "M" = CM
cmpFromText "!D" = CNotD
cmpFromText "!A" = CNotA
cmpFromText "!M" = CNotM
cmpFromText "-D" = CMinusD
cmpFromText "-A" = CMinusA
cmpFromText "-M" = CMinusM
cmpFromText "D+1" = CDPlus1
cmpFromText "A+1" = CAPlus1
cmpFromText "M+1" = CMPlus1
cmpFromText "D-1" = CDMinus1
cmpFromText "A-1" = CAMinus1
cmpFromText "M-1" = CMMinus1
cmpFromText "D+A" = CDPlusA
cmpFromText "D+M" = CDPlusM
cmpFromText "D-A" = CDMinusA
cmpFromText "D-M" = CDMinusM
cmpFromText "A-D" = CAMinusD
cmpFromText "M-D" = CMMinusD
cmpFromText "D&A" = CDAndA
cmpFromText "D&M" = CDAndM
cmpFromText "D|A" = CDOrA
cmpFromText "D|M" = CDOrM
cmpFromText t = error $ T.unpack t ++ " is not a valid Computation" 


cmpToBinary :: CMP -> T.Text
cmpToBinary C0 =        "0101010"
cmpToBinary C1 =        "0111111"
cmpToBinary CMinus1 =   "0111010"
cmpToBinary CD =        "0001100"
cmpToBinary CA =        "0110000"
cmpToBinary CM =        "1110000"
cmpToBinary CNotD =     "0001101"
cmpToBinary CNotA =     "0110001"
cmpToBinary CNotM =     "1110001"
cmpToBinary CMinusD =   "0001111"
cmpToBinary CMinusA =   "0110011"
cmpToBinary CMinusM =   "1110011"
cmpToBinary CDPlus1 =   "0011111"
cmpToBinary CAPlus1 =   "0110111"
cmpToBinary CMPlus1 =   "1110111"
cmpToBinary CDMinus1 =  "0001110"
cmpToBinary CAMinus1 =  "0110010"
cmpToBinary CMMinus1 =  "1110010"
cmpToBinary CDPlusA =   "0000010"
cmpToBinary CDPlusM =   "1000010"
cmpToBinary CDMinusA =  "0010011"
cmpToBinary CDMinusM =  "1010011"
cmpToBinary CAMinusD =  "0000111"
cmpToBinary CMMinusD =  "1000111"
cmpToBinary CDAndA =    "0000000"
cmpToBinary CDAndM =    "1000000"
cmpToBinary CDOrA =     "0010101"
cmpToBinary CDOrM =     "1010101"

fromText :: T.Text -> CInstruction
fromText t = case T.findIndex (== '=') t of
    Nothing -> uncurry (CInstruction (destFromText Nothing)) (computationAndJump t)
    Just eIndex -> uncurry (CInstruction (destFromText (Just . fst $ T.splitAt eIndex t))) $ computationAndJump (T.drop 1 . snd $ T.splitAt eIndex t)
  where
    computationAndJump t = case T.findIndex (== ';') t of
      Nothing -> (cmpFromText t, jmpFromText Nothing)
      Just semiIndex -> (cmpFromText . fst $ T.splitAt semiIndex t, jmpFromText . Just $ T.drop 1 . snd $  T.splitAt semiIndex t)

cInstructionToBinary :: CInstruction -> T.Text
cInstructionToBinary (CInstruction dest cmp jmp) = "111" <> cmpToBinary cmp <> destToBinary dest <> jmpToBinary jmp

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
