{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Library where

import PdePreludat hiding ((++), reverse, length, minimum, unzip, take, drop, replicate, zipWith, (+), (/), (*), (-))
import qualified PdePreludat as P
import GHC.Exts
import GHC.TypeLits (TypeError, ErrorMessage(..), Nat, KnownNat, Symbol, AppendSymbol, CmpSymbol)
import Data.Proxy
import qualified GHC.TypeNats as N
import GHC.Natural (naturalToInt)
import Data.Type.Bool

data Medida where
  Escalar :: Medida
  MedidaSimple :: Symbol -> Symbol -> Medida
  Producto :: [Medida] -> Medida
  Division :: [Medida] -> Medida

newtype Medicion (medida :: Medida) = Medicion { unidades :: Double } deriving (Eq, Ord)

class UnidadDeMedida medida where
  nombreDeUnidad :: String
  multiplicadorDeUnidad :: Double

instance (UnidadDeMedida medida) => Show (Medicion medida) where
  show unaMedicion@(Medicion unasUnidades) =
    show (unasUnidades P./ multiplicadorDeUnidad @medida) <> " " <> (nombreDeUnidad @medida)

(#) = flip ($)

instance UnidadDeMedida (Producto '[]) where
  nombreDeUnidad = mempty
  multiplicadorDeUnidad = 1

instance (UnidadDeMedida unidad, UnidadDeMedida (Producto (segundaUnidad ': otrasUnidades))) => UnidadDeMedida (Producto (primeraUnidad ': segundaUnidad ': otrasUnidades)) where
  nombreDeUnidad = nombreDeUnidad @unidad <> " * " <> nombreDeUnidad @(Producto (segundaUnidad ': otrasUnidades))
  multiplicadorDeUnidad = multiplicadorDeUnidad @unidad

instance (UnidadDeMedida unaUnidad, UnidadDeMedida otraUnidad) => UnidadDeMedida (Division [unaUnidad, otraUnidad]) where
  nombreDeUnidad = "(" <> nombreDeUnidad @unaUnidad <> " / " <> nombreDeUnidad @otraUnidad <> ")"
  multiplicadorDeUnidad = (multiplicadorDeUnidad @unaUnidad) P./ (multiplicadorDeUnidad @otraUnidad)

type family (*) a b :: Medida where
  (medida :: Medida) * (otraMedida :: Medida) = Producto [medida, otraMedida]

type family (/) a b :: Medida where
  (medida :: Medida) / (otraMedida :: Medida) = Division [medida, otraMedida]

instance UnidadDeMedida Escalar where
  nombreDeUnidad = "escalar"
  multiplicadorDeUnidad = 1

escalar :: Double -> Medicion Escalar
escalar = medicion

type Metro = MedidaSimple "Distancia" "Metro"
instance UnidadDeMedida Metro where
  nombreDeUnidad = "metros"
  multiplicadorDeUnidad = 1

metros :: Double -> Medicion Metro
metros = medicion

type Kilometro = MedidaSimple "Distancia" "Kilometro"
instance UnidadDeMedida Kilometro where
  nombreDeUnidad = "kilometros"
  multiplicadorDeUnidad = multiplicadorDeUnidad @Metro P.* 1000

kilometros :: Double -> Medicion Kilometro
kilometros = medicion

type Segundo = MedidaSimple "Tiempo" "Segundo"
instance UnidadDeMedida Segundo where
  nombreDeUnidad = "segundos"
  multiplicadorDeUnidad = 1

segundos :: Double -> Medicion Segundo
segundos = medicion

type Minuto = MedidaSimple "Tiempo" "Minuto"
instance UnidadDeMedida Minuto where
  nombreDeUnidad = "minutos"
  multiplicadorDeUnidad = multiplicadorDeUnidad @Segundo P.* 60

minutos :: Double -> Medicion Minuto
minutos = medicion

type Hora = MedidaSimple "Tiempo" "Hora"
instance UnidadDeMedida Hora where
  nombreDeUnidad = "horas"
  multiplicadorDeUnidad = multiplicadorDeUnidad @Minuto P.* 60

horas :: Double -> Medicion Hora
horas = medicion

type MetrosCuadrados = Metro * Metro
metrosCuadrados :: Double -> Medicion MetrosCuadrados
metrosCuadrados = medicion

type MetrosPorSegundo = Metro / Segundo
metrosPorSegundo :: Double -> Medicion MetrosPorSegundo
metrosPorSegundo = medicion

-- TODO: agregar horas
-- TODO: agregar kilometros por hora

cantidad :: forall medida. (UnidadDeMedida medida) => Medicion medida -> Double
cantidad unaMedicion = unidades unaMedicion P./ multiplicadorDeUnidad @medida

doubleFromNat :: forall (nat :: Nat) . KnownNat nat => Double
doubleFromNat = fromIntegral $ nat @nat

nat :: forall (nat :: Nat) . KnownNat nat => Int
nat = naturalToInt . N.natVal $ Proxy @nat

medicion :: forall medida. (UnidadDeMedida medida) => Double -> Medicion medida
medicion unasUnidades = Medicion (unasUnidades P.* multiplicadorDeUnidad @medida)

pasarA :: forall b a. (MismaDimension a b, UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b
pasarA unaMedicion = Medicion (unidades unaMedicion)

infixl 1 -
infixl 1 +
infixl 2 *
infixl 2 /

(-) :: forall a b. (MismaDimension b a, UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion a
(-) unaMedicion otraMedicion = unaMedicion `restar` (pasarA otraMedicion :: Medicion a)

(+) :: forall a b. (MismaDimension b a, UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion a
(+) unaMedicion otraMedicion = unaMedicion `sumar` (pasarA otraMedicion :: Medicion a)

(*) :: forall a b. (UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion (Por a b)
(*) unaMedicion otraMedicion = Medicion (unidades unaMedicion P.* unidades otraMedicion)

(/) :: forall a b. (UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion (Dividir a b)
(/) unaMedicion otraMedicion = Medicion (unidades unaMedicion P./ unidades otraMedicion)

restar :: Medicion a -> Medicion a -> Medicion a
restar unaMedicion otraMedicion = Medicion $ unidades unaMedicion P.- unidades otraMedicion

sumar :: Medicion a -> Medicion a -> Medicion a
sumar unaMedicion otraMedicion = Medicion $ unidades unaMedicion P.+ unidades otraMedicion

class IffC b t f => Iff (b :: Bool) (t :: Constraint) (f :: Constraint) where
  type IffC b t f :: Constraint
instance t => Iff True  t f where
  type IffC True  t f = t
instance f => Iff False t f where
  type IffC False t f = f

type family DimensionesSonIguales (dimensiones :: [Symbol]) (otrasDimensiones :: [Symbol]) :: Bool where
  DimensionesSonIguales dimensiones dimensiones = True
  DimensionesSonIguales _ _ = False

type family TienenMismasDimensiones (unaMedida :: Medida) (otraMedida :: Medida) :: Bool where
  TienenMismasDimensiones unaMedida otraMedida = DimensionesSonIguales (Dimensiones unaMedida) (Dimensiones otraMedida)

type family MismaDimension (unaMedida :: Medida) (otraMedida :: Medida) :: Constraint where
  MismaDimension unaMedida otraMedida =
    IffC (TienenMismasDimensiones unaMedida otraMedida) ()
                                                        (TypeError (Text "Estás sumando 🍐s con 🍌s, o mas bien, " :<>: Text (Dimension unaMedida) :<>: Text " con " :<>: Text (Dimension otraMedida)))

type family Dimension (medida :: Medida) :: Symbol where
  Dimension Escalar = ""
  Dimension (MedidaSimple dimension _) = dimension
  Dimension (Producto '[]) = ""
  Dimension (Producto (medida ': '[])) = Dimension medida
  Dimension (Producto (medida ': medidas)) = AppendSymbol (AppendSymbol (Dimension medida) " * ") (Dimension (Producto medidas))
  Dimension (Division '[]) = ""
  Dimension (Division (medida ': '[])) = Dimension medida
  Dimension (Division (medida ': medidas)) = AppendSymbol (AppendSymbol (Dimension medida) " / ") (Dimension (Division medidas))

type family Dimensiones (medida :: Medida) where
  Dimensiones Escalar = '[]
  Dimensiones (MedidaSimple dimension _) = dimension ': '[]
  Dimensiones (Producto '[]) = '[]
  Dimensiones (Producto (medida ': medidas)) = Sort ((Dimensiones medida) ++ (Dimensiones (Producto medidas)))
  Dimensiones (Division '[]) = '[]
  Dimensiones (Division (medida ': medidas)) = Sort ((Dimensiones medida) ++ (Dimensiones (Division medidas)))

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

type family Sort xs where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

type family Insert x xs where
  Insert x '[] = x ': '[]
  Insert x (y ': ys) = Insert' (CmpSymbol x y) x y ys

type family Insert' b x y ys where
  Insert' 'LT  x y ys = x ': (y ': ys)
  Insert' _    x y ys = y ': Insert x ys

type family Dividir (medida :: Medida) (otraMedida :: Medida) :: Medida where
  Dividir medida medida = Escalar
  Dividir (Producto [unaMedida, otraMedida]) unaMedida = otraMedida
  Dividir (Producto [unaMedida, otraMedida]) otraMedida = unaMedida
  Dividir (Division [unaMedida, otraMedida]) unaMedida = Escalar / otraMedida
  Dividir unaMedida otraMedida = If (TienenMismasDimensiones unaMedida otraMedida) Escalar (Division [unaMedida, otraMedida])

type family Por (medida :: Medida) (otraMedida :: Medida) :: Medida where
  Por Escalar medida = medida
  Por medida Escalar = medida
  Por (Division [numerador, denominador]) medida =
    If (TienenMismasDimensiones denominador medida)
          numerador
          (Division [numerador, Por denominador medida])
  Por medida otraMedida = Producto [medida, otraMedida]
