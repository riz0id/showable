{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Showable
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Showable' datatype along with basic operations for 
-- working with 'Showable' values. The 'Showable' type pairs an opaque value 
-- along with that value's 'Data' and 'Show' dictionaries. 'Showable' is useful
-- in circumstances where:
--
--   * Arbitrary values need to be stored with the sole intention that they will 
--     either be displayed as a 'String' with functions like 'show' or discarded
--     at some point in the future.
--
--   * It does not make sense to convert and store these arbitrary values as a
--     'String' by first applying 'show'. 
--
--   * Storing the 'String' resulting from the applocation of 'show' to some 
--     value requires a significantly larger heap allocation as opposed to just
--     storing the value itself.
--
-- 'Showable' may also be used to wrap values in newtypes or records when
-- parameterization is undesirable. For example, the type parameter \"a\" for 
-- the following record \"BadValue\" is necessary in order apply the 'Show' 
-- constraint and use 'show' in the function "showBadValue":
--
-- @ 
-- data BadValue a = BadValue 
--   { value  :: a
--   , reason :: String 
--   }
--
-- showBadValue :: Show a => BadValue -> String
-- showBadValue (BadValue x desc) = "illegal value " ++ show x ++ ": " ++ desc
-- @
--
-- However, the type parameter in \"BadValue\" can be removed by replacing the
-- type of the \"value\" field with 'Showable'. This modification still enables
-- the \"showBadValue\" function to be implemented via 'unpack':
--
-- @ 
-- data BadValue a = BadValue 
--   { value  :: a
--   , reason :: String 
--   }
--
-- showBadValue :: Show a => BadValue -> String
-- showBadValue (BadValue x desc) = "illegal value " ++ unpack x ++ ": " ++ desc
-- @
--
-- @since 1.0.0
module Data.Showable
  ( Showable (..),

    -- * Basic Operations
    withShowable,

    -- * Show
    unpack,
    unpacks,
    unpacksPrec,
  ) where

import Data.Data (Data (..), Fixity (..), Constr, DataType)
import Data.Data qualified as Data
import Data.Kind (Type)

import Language.Haskell.TH.Syntax (Lift (..), liftData, unsafeCodeCoerce)

--------------------------------------------------------------------------------

constr :: Constr
constr = Data.mkConstr dataType "Showable" [] Prefix

dataType :: DataType
dataType = Data.mkDataType "Module.T" [constr]

-- Showable --------------------------------------------------------------------

-- | 'Showable' wraps an existentially quantified value along with the value's 
-- dictionaries for the 'Data' and 'Show' typeclasses.
--
-- @since 1.0.0
data Showable :: Type where
  Showable :: (Data a, Show a) => a -> Showable

-- | __Warning:__ The implementation 'gunfold' for 'Showable' is undefined and 
-- will throw an 'error'. 
--
-- @since 1.0.0
instance Data Showable where
  gfoldl k z (Showable x) = z Showable `k` x

  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold: cannot apply 'gunfold' to value of type 'Showable'"

  toConstr _ = constr

  dataTypeOf _ = dataType

-- | @since 1.0.0
instance Lift Showable where
  lift = liftData 
  {-# INLINE lift #-}

  liftTyped = unsafeCodeCoerce . lift
  {-# INLINE liftTyped #-}

-- | __Note:__ The implementations for 'Show' functions prefix the value inside 
-- 'Showable' with the constructor name @"Showable"@. 
--
-- >>> show (Showable (5 :: Int))
-- "Showable 5"
--
-- @since 1.0.0
instance Show Showable where
  show = showString "Showable " . unpack
  {-# INLINE show #-}

  showsPrec p x = showString "Showable " . unpacksPrec p x
  {-# INLINE showsPrec #-}

-- Showable - Basic Operations -------------------------------------------------

-- | Deconstructs a 'Showable' and applies the given function to the unwrapped 
-- value. This function can be used as a helper when implementing functions 
-- similar to 'unpack':
--
-- @
-- unpackWithPrefix :: String -> 'Showable' -> 'String'
-- unpackWithPrefix prefix = 'withShowable' ('mappend' prefix . 'show')
-- @ 
--
-- @since 1.0.0
withShowable :: (forall x. (Data x, Show x) => x -> a) -> Showable -> a
withShowable f (Showable x) = f x

-- Showable - Show -------------------------------------------------------------

-- | The analog of 'show' for 'Showable' value. This function unwraps the given
-- 'Showable' and applies 'show' to the unwrapped value. 
--
-- >>> unpack (Showable 'a')
-- "'a'"
-- 
-- __Note:__ This differs from @('show' (x :: 'Showable'))@ which prefixes the 
-- resulting 'String' with @"Showable "@.
--
-- @since 1.0.0
unpack :: Showable -> String
unpack = withShowable show

-- | The analog of 'shows' for 'Showable' value. This function unwraps the given
-- 'Showable' and applies 'shows' to the unwrapped value. 
--
-- >>> unpacks (Showable (1 :: Int)) "#"
-- "1#"
-- 
-- __Note:__ This differs from @('shows' (x :: 'Showable'))@ which prefixes the 
-- resulting 'ShowS' with @(showString "Showable ")@.
--
-- @since 1.0.0
unpacks :: Showable -> ShowS
unpacks = withShowable shows

-- | The analog of 'showsPrec' for 'Showable' value. This function unwraps the 
-- given 'Showable' and applies 'showsPrec' to the unwrapped value with the 
-- specified precedence. 
-- 
-- __Note:__ This differs from @('showsPrec' p (x :: 'Showable'))@ which 
-- prefixes the resulting 'ShowS' with @(showString "Showable ")@.
--
-- @since 1.0.0
unpacksPrec :: Int -> Showable -> ShowS
unpacksPrec p = withShowable (showsPrec p)
