{-# LANGUAGE GADTs,
             KindSignatures,
             TypeOperators,
             TypeFamilies,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             StandaloneDeriving,
             UndecidableInstances,
             FunctionalDependencies,
             ConstraintKinds,
             ScopedTypeVariables#-}

module Example where

import AspectAG
import HList

--datatype

data Root = Root Tree
          deriving Show
data Tree = Leaf Int
          | Node Tree Tree
          deriving Show

proxy = Proxy


-- Labels (attribute names) for the example
data Att_smin
data Att_ival
data Att_sres

smin = Label :: Label Att_smin
ival = Label :: Label Att_ival
sres = Label :: Label Att_sres

-- Labels for childs
data Ch_tree -- root
data Ch_r    -- node
data Ch_l    -- node
data Ch_i    -- leaf

ch_tree = Label :: Label (Ch_tree, Tree)
ch_r = Label :: Label (Ch_r, Tree)
ch_l = Label :: Label (Ch_l, Tree)
ch_i = Label :: Label (Ch_i, Int)
-- both the name and the type of the child are encoded

type SP = HList '[Att (Label Att_smin) Int,
            Att (Label Att_sres) Tree]
type IL = HList '[Att (Label Att_ival) Int]
type IR = HList '[Att (Label Att_ival) Int]

type IC = HList '[Chi (Label (Ch_l,Tree)) IL,
            Chi (Label (Ch_r,Tree)) IR]


type family UnWrap (l :: Type) :: [Type]
type instance UnWrap (HList l) = l
type Output_Node = Fam (UnWrap IC) (UnWrap SP)


-- Figure 6

leaf_smin (Fam chi par)
  = syndef smin (chi # ch_i)
