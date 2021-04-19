{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Week02.Homework2 where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map (toList)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger
  ( Address (ScriptAddress),
    Redeemer (Redeemer),
    Validator,
    ValidatorCtx,
    ValidatorHash,
    txId,
  )
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.Constraints as Constraints
  ( TxConstraints,
    mustPayToTheScript,
    mustSpendScriptOutput,
    otherScript,
    unspentOutputs,
  )
import qualified Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, printSchemas)
import Playground.TH (ensureKnownCurrencies, mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
  ( AsContractError,
    BlockchainActions,
    Contract,
    Endpoint,
    HasBlockchainActions,
    awaitTxConfirmed,
    endpoint,
    logInfo,
    select,
    submitTxConstraints,
    submitTxConstraintsWith,
    utxoAt,
    (>>),
    type (.\/),
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    Eq ((==)),
    IO,
    Integer,
    Monad ((>>=)),
    String,
    fst,
    mconcat,
    traceIfFalse,
    ($),
    (<$>),
  )
import Text.Printf (printf)
import Prelude (Semigroup (..))

data MyRedeemer = MyRedeemer
  { flag1 :: Bool,
    flag2 :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINEABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
mkValidator :: () -> MyRedeemer -> ValidatorCtx -> Bool
mkValidator _ (MyRedeemer f1 f2) _ = traceIfFalse "Redeemer mismatch" $ f1 == f2

data Typed

instance Scripts.ScriptType Typed where
  type DatumType Typed = ()
  type RedeemerType Typed = MyRedeemer

inst :: Scripts.ScriptInstance Typed
inst =
  Scripts.validator @Typed
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @MyRedeemer

validator :: Validator
validator = Scripts.validatorScript inst

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash

type GiftSchema =
  BlockchainActions
    .\/ Endpoint "give" Integer
    .\/ Endpoint "grab" MyRedeemer

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
  let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => MyRedeemer -> Contract w s e ()
grab r = do
  utxos <- utxoAt scrAddress
  let orefs = fst <$> Map.toList utxos
      lookups =
        Constraints.unspentOutputs utxos
          <> Constraints.otherScript validator
      tx :: TxConstraints Void Void
      tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData r | oref <- orefs]
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
