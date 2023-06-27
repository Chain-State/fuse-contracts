{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Deploy where

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.Aeson                          as DataAeson
import           Data.Aeson.Text       (encodeToLazyText)

import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Data.String               as S

import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
-- import qualified Ledger
import           Cardano.Api.Shelley ( fromPlutusData )
import qualified Data.Text.Lazy.IO   as LT
-- import           Plutus.V1.Ledger.Value                             as ValueV1

import qualified SwapOnChain            as OnChain
import qualified FTokens                as FTokens

import           Utilities                 (writeCodeToFile, writePolicyToFile, writeValidatorToFile)

main :: IO ()
main = do
    writeInitDatum
    writeContractDatum
    writeSwapRedeemer

    _ <- writeSwapValidatorScript
    _ <- saveSignedPolicy

    -- fileContents <- readJSON $ basePath++"borrow-request-redeemer.json"
    -- print fileContents
    -- print $ "USDK Currency Symbol -------- "++ show $ FTokens.signedCurrencySymbol fTokensParams
    -- putStrLn $ " maxAdaValue--------"++show maxAdaValue++"\n swapAmount--------"++show swapAmount++" Lovelace"++
    --         "\n loanAmount--------"++show $ loanAmount swapAmount++" USDH"++"\n interestAmount--------"++
    --         show $ interestAmount (loanAmount swapAmount) 5++" USDH"

    return ()

basePath :: FilePath
basePath = "./assets/" 
    
dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeJSONFromPlutus :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSONFromPlutus file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . LedgerApiV2.toData 

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeMintingValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

writeInitDatum :: IO ()
writeInitDatum = writeJSON (basePath++"datum/unit.json") ()

writeSwapRedeemer :: IO ()
writeSwapRedeemer = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData () 
    in writeJSONFromPlutus (basePath++"redeemer/swap-redeemer.json") red 

readJSON :: FilePath -> IO LBS.ByteString
readJSON = LBS.readFile

writeContractDatum :: IO ()
writeContractDatum = 
    let contributor = swapDatum
        d = PlutusTx.toBuiltinData contributor
    in writeJSONFromPlutus (basePath++"datum/swap-datum.json") d

-- Virtual Fixed Ada price based on the 
-- Average high and low Ada USD price of all time.
-- TODO: Should be calculated daily
minAdaValue :: Double
minAdaValue = 0.487209213

currentAdaValue :: Double
currentAdaValue = 0.377838

-- swapAmount :: Integer -> Integer
-- swapAmount i = i

lockDatum :: Integer
lockDatum =  1_000

swapDatum :: OnChain.SwapDatum
swapDatum =  OnChain.SwapDatum {   
    OnChain.lockDat = lockDatum
}
-- fTokensCs :: LedgerApiV2.CurrencySymbol
-- fTokensCs = FTokens.signedCurrencySymbol fTokensParams

-- fTokensParams :: FTokens.SignParam
-- fTokensParams = FTokens.SignParam {
--     FTokens.beneficiary = LedgerApiV2.PubKeyHash "6dde623cf9cccc589d33172139ba09fa8274c962ea3b6521d084cfc9"
-- }./

swapPubkeyhash :: LedgerApiV2.PubKeyHash
swapPubkeyhash = LedgerApiV2.PubKeyHash $ LedgerApiV2.getLedgerBytes $ S.fromString "b2089d44c9fc3ce37cd446b2441d5c039b4687c5836b1a2ecb8f1553"

contractParams :: OnChain.ContractParam
contractParams =  OnChain.ContractParam {   
    OnChain.swaper = swapPubkeyhash,
    OnChain.tokenCs = LedgerApiV2.CurrencySymbol "",
    OnChain.tokenTn = LedgerApiV2.TokenName "",
    OnChain.swapAmnt = 15000000
}

writeSwapValidatorScript :: IO ()
writeSwapValidatorScript =  writeValidatorToFile (basePath++"plutus-scripts/swap.plutus") $ OnChain.validator contractParams

pubkeyhash :: LedgerApiV2.PubKeyHash
pubkeyhash = LedgerApiV2.PubKeyHash $ LedgerApiV2.getLedgerBytes $ S.fromString "eb8484d58f0dabda41b3497db0de4eac82dce04ac5791d6ae96a640c"

saveSwapLucidCode :: IO ()
saveSwapLucidCode = writeCodeToFile (basePath++"plutus-scripts/swap-lucid.plutus") OnChain.validatorCode

saveSignedPolicy :: IO ()
saveSignedPolicy = writePolicyToFile (basePath++"plutus-scripts/ftokens.plutus") $ FTokens.signedPolicy pubkeyhash

-- saveSignedPolicy :: IO ()
-- saveSignedPolicy = writePolicyToFile (basePath++"plutus-scripts/ftokens.plutus") $ FTokens.signedPolicy pubkeyhash
