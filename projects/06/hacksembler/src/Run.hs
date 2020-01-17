{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Run (run) where

import Import
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified RIO.HashMap as HM
import qualified RIO.State as S
import qualified RIO.Char as C
import qualified RIO.List.Partial as L'
import qualified RIO.List as L


import Util


run :: RIO App ()
run = do
  (App _ _ (Options fileName)) <- ask
  contents <- B.readFile fileName 
  let (_ , resolvedSymbolsTable) = (resolveSymbols (15, firstPass contents) (filter isAInstruction (linesWithoutComments contents)))
  -- resolvedSymbolTable <- liftRIO . return $ S.evalState  (resolveSymbols (filter isAInstruction (linesWithoutComments contents))) (16, firstPass contents)
  mapM_ (logInfo . display) $ map (toBinary resolvedSymbolsTable) $ filter (not . isLabel) (linesWithoutComments contents)
  -- (logInfo . displayShow) $ resolvedSymbolsTable
  -- (logInfo . displayShow) $ firstPass contents

  return ()

  where
    textlines contents = T.lines $ T.decodeUtf8With lenientDecode contents
    linesWithoutSpaces contents = map T.strip (textlines contents)
    lineStrippedOfComment line = T.unwords . T.words $ (T.strip . fst $ T.span (/= '/') line)
    linesWithoutComments contents = map lineStrippedOfComment $ filter (not . T.null) (filter (not . T.isPrefixOf "//") (linesWithoutSpaces contents))
    assingRomAddress contents = L.foldl' (\(lines, romAddress) line -> if isLabel line 
                                                                        then (lines ++ [(line, romAddress)], romAddress)
                                                                        else (lines ++ [(line, romAddress)], romAddress+1)) ([] :: [(T.Text, Int)], 0) (linesWithoutComments contents)
    firstPass contents = HM.union predefinedSymbolTable $ HM.fromList $ map (\t -> ( T.dropAround (\c -> c == '(' || c == ')') . fst $ t, T.pack . show . snd $ t )) $ filter (isLabel . fst ) $ fst . assingRomAddress $ contents
    
    -- resolveSymbols :: [T.Text] -> S.State (Int, HashMap T.Text T.Text) (HashMap T.Text T.Text)
    -- resolveSymbols aInstructionsLines =
    --   forM aInstructionsLines $ \aInstruction -> do
    --     (currentAddress, symbolTable) <- S.get
    --     let instruction =  T.drop 1 aInstruction
    --     if C.isDigit (L'.head (T.unpack instruction))
    --       then return (currentAddress, symbolTable)
    --       else return (currentAddress+1, symbolTable)

    resolveSymbols = L.foldl' (\(currentAddress, symbolTable) aInstruction ->
        let instruction =  T.drop 1 aInstruction in
          if C.isDigit (L'.head (T.unpack instruction))
          then (currentAddress, symbolTable)
          else (if instruction ` HM.member` symbolTable 
                  then currentAddress
                  else currentAddress+1
               , HM.alter (\case Just v -> if instruction `HM.member` symbolTable
                                          then Just v
                                          else Just . T.pack . show $ currentAddress+1
                                 Nothing -> (Just . T.pack . show $ currentAddress+1)
                          ) instruction symbolTable
               )
          )



          --   

    toBinary symbolTable t = if isAInstruction t
      then aInstructionToBinary $ "@" <> HM.lookupDefault (T.drop 1 t) (T.drop 1 t) symbolTable
      else cInstructionToBinary . fromText $ t
      