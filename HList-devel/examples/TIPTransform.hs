{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

-- Transforming a TIP: applying to a TIP a (polyvariadic) function
-- that takes arguments from a TIP and updates the TIP with the result.
-- 
-- In more detail: we have a typed-indexed collection TIP and we
-- would like to apply a transformation function to it, whose argument
-- types and the result type are all in the TIP. The function should locate
-- its arguments based on their types, and update the TIP
-- with the result. The function may have any number of arguments,
-- including zero; the order of arguments should not matter.

-- The problem was posed by Andrew U. Frank on Haskell-Cafe, Sep 10, 2009.
-- http://www.haskell.org/pipermail/haskell-cafe/2009-September/066217.html
-- The problem is an interesting variation of the keyword argument problem.

module TIPTransform where

import Data.HList
import Data.Typeable

-- We start with the examples

newtype MyVal = MyVal Int deriving (Show, Typeable)

-- or if no typeable, use
-- instance ShowLabel MyVal where showLabel _ = "MyVal"

-- A sample TIP
tip1 = MyVal 20 .*. (1::Int) .*. True .*. emptyTIP
-- TIP (HCons (MyVal 20) (HCons 1 (HCons True HNil)))

-- Update the Int component of tip1 to 2. The Int component must
-- exist. Otherwise, it is a type error
tip2 = ttip (2::Int) tip1
-- TIP (HCons (MyVal 20) (HCons 2 (HCons True HNil)))

-- Negate the boolean component of tip1
tip3 = ttip not tip1
-- TIP (HCons (MyVal 20) (HCons 1 (HCons False HNil)))

-- Update the Int component from the values of two other components
tip4 = ttip (\(MyVal x) y -> x+y) tip1
-- TIP (HCons (MyVal 20) (HCons 21 (HCons True HNil)))

-- Update the MyVal component from the values of three other components
tip5 = ttip (\b (MyVal x) y -> MyVal $ if b then x+y else 0) tip1
-- TIP (HCons (MyVal 21) (HCons 1 (HCons True HNil)))

-- The same but with the permuted argument order.
-- The order of arguments is immaterial: the values will be looked up using
-- their types
tip5' = ttip (\b y (MyVal x)-> MyVal $ if b then x+y else 0) tip1
-- TIP (HCons (MyVal 21) (HCons 1 (HCons True HNil)))

-- The implementation
-- part of HList proper now


main = mapM_ putStrLn [show tip1, show tip2, show tip3, show tip4,
		       show tip5, show tip5']
