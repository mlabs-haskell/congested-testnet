{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AlwaysTrueScript where

import PlutusTx
    ( CompiledCode,
      BuiltinData,
      UnsafeFromData(unsafeFromBuiltinData),
      -- unsafeApplyCode,
      liftCode,
      makeLift,
      compile )
-- import PlutusLedgerApi.V2 (ScriptContext)
import PlutusTx.Prelude (check, (!!), (&&), foldr)
-- import PlutusCore.Core (plcVersion100)


newtype AlwaysTrueScriptParams = AlwaysTrueScriptParams {size :: Integer}

PlutusTx.makeLift ''AlwaysTrueScriptParams


-- {-# INLINEABLE typedValidator #-}
-- typedValidator :: AlwaysTrueScriptParams -> () -> () -> PlutusLedgerApi.V2.ScriptContext -> Bool
-- typedValidator AlwaysTrueScriptParams {size} _ _ _ = True --res 
--   -- where
--   --   list = PlutusTx.Prelude.replicate size True
--   --   res  = PlutusTx.Prelude.foldr (PlutusTx.Prelude.&&) True list
--
--
-- {-# INLINEABLE untypedValidator #-}
-- untypedValidator :: AlwaysTrueScriptParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- untypedValidator params datum redeemer ctx =
--   check
--     ( typedValidator
--         params
--         (PlutusTx.unsafeFromBuiltinData datum)
--         (PlutusTx.unsafeFromBuiltinData redeemer)
--         (PlutusTx.unsafeFromBuiltinData ctx)
--     )
--
-- script ::
--   AlwaysTrueScriptParams ->
--   CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- script params =
--  $$(PlutusTx.compile [||untypedValidator||])
--     `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusCore.Core.plcVersion100 params

alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])
