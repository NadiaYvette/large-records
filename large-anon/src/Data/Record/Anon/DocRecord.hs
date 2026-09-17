{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Self-documenting records with descriptions, defaults, schema reflection,
-- JSON parsing, and CLI flag generation (inspired by porcupine's docrecords).
module Data.Record.Anon.DocRecord (
    -- * Documented functor and constructors
    Doc(..)
  , doc
  , docDef
  , docVal
  , docDefVal
    -- * Documented records
  , DocRecord
  , resolveDocRecord
  , resolveDocRecordAll
    -- * Schema and Markdown
  , FieldDoc(..)
  , docRecordSchema
  , docRecordMarkdown
    -- * JSON Parsing with Defaults
  , parseJSONDocRecord
    -- * CLI Parser Generation
  , docRecordParser
  , docRecordParserWith
  ) where

import Data.Aeson (FromJSON, Object, Value, withObject, (.:?))
import Data.Aeson.Types (Parser)
import Data.Functor.Product (Product(Pair))
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import qualified Options.Applicative as OA
import Data.SOP.BasicFunctors ((:.:)(..), I(..), K(..))

import Data.Record.Anon (AllFields, KnownFields)
import qualified Data.Record.Anon.Advanced as Advanced
import qualified Data.Record.Anon.Simple as Simple
import Data.Record.Anon.Plugin.Internal.Runtime (Row)

-- | Functor holding field documentation, an optional default, and an optional value
data Doc a = Doc {
    docDescription :: String
  , docDefault     :: Maybe a
  , docValue       :: Maybe a
  } deriving (Show, Eq, Functor)

-- | Construct a required documented field
doc :: String -> Doc a
doc desc = Doc desc Nothing Nothing

-- | Construct a documented field with a default fallback
docDef :: String -> a -> Doc a
docDef desc def = Doc desc (Just def) Nothing

-- | Construct a documented field with an explicit value
docVal :: String -> a -> Doc a
docVal desc val = Doc desc Nothing (Just val)

-- | Construct a documented field with both a default and an explicit value
docDefVal :: String -> a -> a -> Doc a
docDefVal desc def val = Doc desc (Just def) (Just val)

-- | A documented record over row @r@
type DocRecord (r :: Row Type) = Advanced.Record Doc r

-- | Resolve a 'DocRecord' into a concrete 'Simple.Record'.
-- Fails with the first missing required field error.
resolveDocRecord ::
     forall (r :: Row Type).
     KnownFields r
  => DocRecord r
  -> Either String (Simple.Record r)
resolveDocRecord docRec =
    Simple.fromAdvanced <$> Advanced.mapM aux (Advanced.zip (Advanced.reifyKnownFields (Proxy @r)) docRec)
  where
    aux :: forall x. Product (K String) Doc x -> Either String (I x)
    aux (Pair (K name) (Doc desc mDef mV)) =
      case mV of
        Just v  -> Right (I v)
        Nothing -> case mDef of
          Just d  -> Right (I d)
          Nothing -> Left $ "Missing required field '" ++ name ++ "': " ++ desc

-- | Resolve a 'DocRecord', accumulating all missing required field errors
resolveDocRecordAll ::
     forall (r :: Row Type).
     KnownFields r
  => DocRecord r
  -> Either [String] (Simple.Record r)
resolveDocRecordAll docRec =
    let pairs = Advanced.collapse $
                  Advanced.zipWith
                    (\(K name) (Doc desc mDef mV) ->
                       K (name, desc, isJust mDef || isJust mV))
                    (Advanced.reifyKnownFields (Proxy @r))
                    docRec
        errs = [ "Missing required field '" ++ name ++ "': " ++ desc
               | (name, desc, hasVal) <- pairs
               , not hasVal
               ]
    in if null errs
         then case resolveDocRecord docRec of
                Right r  -> Right r
                Left err -> Left [err]
         else Left errs

-- | Reflected field schema
data FieldDoc = FieldDoc {
    fieldDocName        :: String
  , fieldDocDescription :: String
  , fieldDocHasDefault  :: Bool
  , fieldDocHasValue    :: Bool
  } deriving (Show, Eq)

-- | Reflect schema information from a 'DocRecord'
docRecordSchema ::
     forall (r :: Row Type).
     KnownFields r
  => DocRecord r
  -> [FieldDoc]
docRecordSchema docRec =
    Advanced.collapse $
      Advanced.zipWith
        (\(K name) (Doc desc mDef mV) ->
           K $ FieldDoc {
               fieldDocName        = name
             , fieldDocDescription = desc
             , fieldDocHasDefault  = isJust mDef
             , fieldDocHasValue    = isJust mV
             })
        (Advanced.reifyKnownFields (Proxy @r))
        docRec

-- | Render a 'DocRecord' schema as a Markdown table
docRecordMarkdown ::
     forall (r :: Row Type).
     KnownFields r
  => DocRecord r
  -> String
docRecordMarkdown docRec =
    unlines $
      [ "| Field | Description | Default? | Provided? |"
      , "|:---|:---|:---|:---|"
      ] ++
      [ "| `" ++ fieldDocName fd ++ "` | "
        ++ fieldDocDescription fd ++ " | "
        ++ (if fieldDocHasDefault fd then "Yes" else "No") ++ " | "
        ++ (if fieldDocHasValue fd then "Yes" else "No") ++ " |"
      | fd <- docRecordSchema docRec
      ]

-- | Parse a JSON Object into a 'Simple.Record' using defaults from the 'DocRecord'
parseJSONDocRecord ::
     forall (r :: Row Type).
     (AllFields r FromJSON, KnownFields r)
  => DocRecord r
  -> Value
  -> Parser (Simple.Record r)
parseJSONDocRecord docRec =
    withObject "DocRecord" $ \obj -> do
      recI <- Advanced.sequenceA $
                Advanced.czipWith
                  (Proxy @FromJSON)
                  (aux obj)
                  (Advanced.reifyKnownFields (Proxy @r))
                  docRec
      return (Simple.fromAdvanced recI)
  where
    aux :: forall x. FromJSON x => Object -> K String x -> Doc x -> (Parser :.: I) x
    aux obj (K name) (Doc desc mDef mV) = Comp $ do
      mParsed <- obj .:? fromString name
      case mParsed of
        Just v -> return (I v)
        Nothing -> case mV of
          Just v  -> return (I v)
          Nothing -> case mDef of
            Just d  -> return (I d)
            Nothing -> fail $ "Missing required field '" ++ name ++ "': " ++ desc

-- | Generate an @optparse-applicative@ command-line parser from a 'DocRecord'
docRecordParser ::
     forall (r :: Row Type).
     (AllFields r Read, KnownFields r)
  => DocRecord r
  -> OA.Parser (Simple.Record r)
docRecordParser docRec =
    Simple.fromAdvanced <$> Advanced.sequenceA (
      Advanced.czipWith
        (Proxy @Read)
        makeFieldParser
        (Advanced.reifyKnownFields (Proxy @r))
        docRec
    )
  where
    makeFieldParser :: forall x. Read x => K String x -> Doc x -> (OA.Parser :.: I) x
    makeFieldParser (K name) (Doc desc mDef mV) = Comp $ do
      let baseMods = OA.long name <> OA.help desc
      case mV of
        Just v -> pure (I v)
        Nothing -> case mDef of
          Just d  -> I <$> OA.option OA.auto (baseMods <> OA.value d)
          Nothing -> I <$> OA.option OA.auto baseMods

-- | Generate an @optparse-applicative@ parser with custom field parser function
docRecordParserWith ::
     forall (r :: Row Type).
     KnownFields r
  => (forall x. String -> Doc x -> OA.Parser x)
  -> DocRecord r
  -> OA.Parser (Simple.Record r)
docRecordParserWith parseFld docRec =
    Simple.fromAdvanced <$> Advanced.sequenceA (
      Advanced.map
        (\(Pair (K name) d) -> Comp (I <$> parseFld name d))
        (Advanced.zip (Advanced.reifyKnownFields (Proxy @r)) docRec)
    )
