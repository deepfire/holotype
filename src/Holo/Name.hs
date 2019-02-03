{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}
module Holo.Name
  ( Name(..)
  , As(..), defName, defStyGeoName
  , Style(..), sStyle, sStyleGene, initStyle, defStyle
  , StyleGene(..), fromStyleGene
  , Visual(..), VPort
  )
where
import           ExternalImports

-- Local imports
import           Graphics.Flex                            (Geo, defGeo)

import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Prelude
import           Holo.Port                                (IdToken, Drawable)
import qualified Holo.Port                         as Port


-- * Name
--    ..as per Пиотровский Р. Г. Текст, машина, человек — Л.: Наука, 1975
--    Which is supposed to make sense in context of As/Denoted
data Name (a ∷ Type) where
  Name ∷
    { nToken     ∷ IdToken
    , nStyle     ∷ Style a
    , nGeo       ∷ Geo
    , n          ∷ a
    } → Name a

defName ∷ ∀ a. As a ⇒ IdToken → a → Name a
defName tok n = Name tok defStyle defGeo n

-- default of Named.compName
defStyGeoName ∷ As n ⇒ Proxy a → IdToken → n → Name n
defStyGeoName _ tok n = Name tok (initStyle $ compSty n) defGeo n


-- * Style wrapper
--
newtype StyleGene = StyleGene { _fromStyleGene ∷ Int } deriving (Eq, Ord)
fromStyleGene ∷ Lens' StyleGene Int
fromStyleGene f (StyleGene x) = f x <&> StyleGene

data Style a where
  Style ∷
    { _sStyle      ∷ Sty a
    , _sStyleGene  ∷ StyleGene
    } → Style a

sStyle     ∷ Lens' (Style a) (Sty a)
sStyle     f s@Style{..} = f _sStyle     <&> \x→ s{_sStyle=x}
sStyleGene ∷ Lens' (Style a) StyleGene
sStyleGene f s@Style{..} = f _sStyleGene <&> \x→ s{_sStyleGene=x}

initStyle ∷ Sty a → Style a
initStyle s = Style { _sStyle = s, _sStyleGene = StyleGene 0 }

defStyle ∷ ∀ a. As a ⇒ Style a
defStyle = initStyle $ defSty (Proxy @a)


-- * Visual wrapper
--
data Visual (a ∷ Type) where
  Visual ∷ As a ⇒
    { vVisual   ∷ Vis a
    , vDrawable ∷ Drawable
    } → Visual a

type VPort = Port.Port Visual

instance Port.PortVisual Visual where
  pvDrawable = vDrawable
  pvFree _pC pA = \case
    Visual{..} → freeVis pA vVisual
