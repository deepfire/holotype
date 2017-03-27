{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}

module Types (
              CatName(..)
             , Category(..)
             , ViewAPI(..)
             -- , View(..)
             -- , ViewPref(..)
             -- , VPEntry(..)
             ) where

import           Data.List (find)
import           Data.Kind
import           GHC.Exts (Constraint)


type family   C a ∷ Constraint
type instance C a = (Eq a, Show a)

data CatName
    = Graph
    | Dag
    | Set
    deriving (Eq, Ord, Show, Read)

class C (ViewName cn) ⇒ Category (cn ∷ CatName) where
----- site of GHC 7.10.2 failure
   data ViewName cn ∷ Type

instance Category 'Graph where
   data ViewName 'Graph = SideGraph | DownGraph deriving (Eq, Show)
instance Category 'Set where
   data ViewName 'Set   = Grid | List           deriving (Eq, Show)

class (Category cn) ⇒ ViewAPI (cn ∷ CatName) (vn ∷ ViewName cn) where
                ----- site of the GHC 8.0.0.20160111 failure, without no TypeInType

$(return [])

instance ViewAPI 'Set ('Grid ∷ ViewName 'Set) where
                      ----- site of the "nokinds" failure

--     make_view ∷ ViewName cn → View
--     elect_view       ∷ CatName → ViewPref → Maybe (ViewName cn) → ViewName cn

-- data ViewPref where
--     ViewPref ∷ [VPEntry] → ViewPref
-- data VPEntry where
--     VPEntry ∷ Category cn ⇒ CatName → ViewName cn → VPEntry

-- data View where
--     View ∷ ViewAPI cn vn ⇒ CatName → ViewName cn → View

-- prefer_engi ∷ Category cn ⇒ CatName → ViewPref → Maybe (ViewName cn) → ViewName cn
-- prefer_engi _ (ViewPref _) _ = undefined
--     case find (\(VPEntry cn ename) → wanted_cn == cn) xs of
--       Just (VPEntry icat ename) → ename
--       Nothing                   → defname
