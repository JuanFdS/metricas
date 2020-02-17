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
import Data.Type.Equality

data Medida where
  Escalar :: Medida
  MedidaSimple :: Symbol -> Symbol -> Medida
  Producto :: Medida -> Medida -> Medida
  Division :: Medida -> Medida -> Medida

newtype Medicion (medida :: Medida) = Medicion { unidades :: Double } deriving (Eq, Ord)

class UnidadDeMedida medida where
  nombreDeUnidad :: String
  multiplicadorDeUnidad :: Double

instance (UnidadDeMedida medida) => Show (Medicion medida) where
  show unaMedicion@(Medicion unasUnidades) =
    show (unasUnidades P./ multiplicadorDeUnidad @medida) <> " " <> (nombreDeUnidad @medida)

(#) = flip ($)

-- type family (*) a b :: Medida where
--   (medida :: Medida) * (otraMedida :: Medida) = Producto medida otraMedida

instance (UnidadDeMedida unaUnidad, UnidadDeMedida otraUnidad) => UnidadDeMedida (Producto unaUnidad otraUnidad) where
  nombreDeUnidad = nombreDeUnidad @unaUnidad <> " * " <> nombreDeUnidad @otraUnidad
  multiplicadorDeUnidad = multiplicadorDeUnidad @unaUnidad P.* multiplicadorDeUnidad @otraUnidad

instance (UnidadDeMedida unaUnidad, UnidadDeMedida otraUnidad) => UnidadDeMedida (Division unaUnidad otraUnidad) where
  nombreDeUnidad = nombreDeUnidad @unaUnidad <> " / " <> nombreDeUnidad @otraUnidad
  multiplicadorDeUnidad = multiplicadorDeUnidad @unaUnidad P./ multiplicadorDeUnidad @otraUnidad

instance UnidadDeMedida Escalar where
  nombreDeUnidad = "escalar"
  multiplicadorDeUnidad = 1


-- instance UnidadDeMedida (Producto '[]) where
--   nombreDeUnidad = mempty
--   multiplicadorDeUnidad = 1

-- instance (UnidadDeMedida unidad) => UnidadDeMedida (Producto '[unidad]) where
--   nombreDeUnidad = nombreDeUnidad @unidad
--   multiplicadorDeUnidad = multiplicadorDeUnidad @unidad

-- instance (UnidadDeMedida unidad, UnidadDeMedida (Producto (segundaUnidad ': otrasUnidades))) => UnidadDeMedida (Producto (primeraUnidad ': segundaUnidad ': otrasUnidades)) where
--   nombreDeUnidad = nombreDeUnidad @unidad <> " * " <> nombreDeUnidad @(Producto (segundaUnidad ': otrasUnidades))
--   multiplicadorDeUnidad = multiplicadorDeUnidad @unidad

-- instance (UnidadDeMedida unaUnidad, UnidadDeMedida otraUnidad) => UnidadDeMedida (Division [unaUnidad, otraUnidad]) where
--   nombreDeUnidad = "(" <> nombreDeUnidad @unaUnidad <> " / " <> nombreDeUnidad @otraUnidad <> ")"
--   multiplicadorDeUnidad = (multiplicadorDeUnidad @unaUnidad) P./ (multiplicadorDeUnidad @otraUnidad)

escalar :: Double -> Medicion Escalar
escalar = medicion

type Metro = MedidaSimple "Distancia" "Metro"
instance UnidadDeMedida Metro where
  nombreDeUnidad = "metros"
  multiplicadorDeUnidad = 1

type MariCuadra = MedidaSimple "Distancia" "MariCuadra"
instance UnidadDeMedida MariCuadra where
  nombreDeUnidad = "maricuadras"
  multiplicadorDeUnidad = multiplicadorDeUnidad @Metro P.* 525

type Cuadra = MedidaSimple "Distancia" "Cuadra"
instance UnidadDeMedida Cuadra where
  nombreDeUnidad = "cuadras"
  multiplicadorDeUnidad = multiplicadorDeUnidad @MariCuadra P./ 5.25

cuadras :: Double -> Medicion Cuadra
cuadras = medicion

cuadra = cuadras 1

mariCuadras :: Double -> Medicion MariCuadra
mariCuadras = medicion

mariCuadra = mariCuadras 1

metros :: Double -> Medicion Metro
metros = medicion

metro = metros 1

type Kilometro = MedidaSimple "Distancia" "Kilometro"
instance UnidadDeMedida Kilometro where
  nombreDeUnidad = "kilometros"
  multiplicadorDeUnidad = multiplicadorDeUnidad @Metro P.* 1000

kilometros :: Double -> Medicion Kilometro
kilometros = medicion

kilometro = kilometros 1

type Litro = MedidaSimple "Volumen" "Litro"
instance UnidadDeMedida Litro where
  nombreDeUnidad = "litros"
  multiplicadorDeUnidad = 1

litros :: Double -> Medicion Litro
litros = medicion

litro = litros 1

type Segundo = MedidaSimple "Tiempo" "Segundo"
instance UnidadDeMedida Segundo where
  nombreDeUnidad = "segundos"
  multiplicadorDeUnidad = 1

segundos :: Double -> Medicion Segundo
segundos = medicion

segundo = segundos 1

type Minuto = MedidaSimple "Tiempo" "Minuto"
instance UnidadDeMedida Minuto where
  nombreDeUnidad = "minutos"
  multiplicadorDeUnidad = multiplicadorDeUnidad @Segundo P.* 60

minutos :: Double -> Medicion Minuto
minutos = medicion

-- type Hora = MedidaSimple "Tiempo" "Hora"
-- instance UnidadDeMedida Hora where
--   nombreDeUnidad = "horas"
--   multiplicadorDeUnidad = multiplicadorDeUnidad @Minuto P.* 60

-- horas :: Double -> Medicion Hora
-- horas = medicion

type MetrosCuadrados = Metro * Metro
metrosCuadrados :: Double -> Medicion MetrosCuadrados
metrosCuadrados = medicion

type LitrosCuadrados = Litro * Litro

type SegundosPorMetro = Segundo / Metro

type LitrosPorSegundo = Litro / Segundo

type SegundosCuadrados = Segundo * Segundo
segundosCuadrados :: Double -> Medicion SegundosCuadrados
segundosCuadrados = medicion

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

pasarA :: forall b a. (TienenMismaDimension a b, UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b
pasarA unaMedicion = Medicion (unidades unaMedicion)

-- infixl 1 -
-- infixl 1 +
-- infixl 2 *
-- infixl 2 /

-- (-) :: forall a b. (MismaDimension b a, UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion a
-- (-) unaMedicion otraMedicion = unaMedicion `restar` (pasarA otraMedicion :: Medicion a)

(+) :: forall a b. (TienenMismaDimension b a, UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion a
(+) unaMedicion otraMedicion = unaMedicion `sumar` (pasarA otraMedicion :: Medicion a)

(*) :: forall a b. (UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion (a * b)
(*) unaMedicion otraMedicion = Medicion (unidades unaMedicion P.* unidades otraMedicion)

(/) :: forall a b. (UnidadDeMedida a, UnidadDeMedida b) => Medicion a -> Medicion b -> Medicion (a / b)
(/) unaMedicion otraMedicion = Medicion (unidades unaMedicion P./ unidades otraMedicion)

-- restar :: Medicion a -> Medicion a -> Medicion a
-- restar unaMedicion otraMedicion = Medicion $ unidades unaMedicion P.- unidades otraMedicion

sumar :: Medicion a -> Medicion a -> Medicion a
sumar unaMedicion otraMedicion = Medicion $ unidades unaMedicion P.+ unidades otraMedicion

class IffC b t f => Iff (b :: Bool) (t :: Constraint) (f :: Constraint) where
  type IffC b t f :: Constraint
instance t => Iff True  t f where
  type IffC True  t f = t
instance f => Iff False t f where
  type IffC False t f = f

-- type family DimensionesSonIguales (dimensiones :: [Symbol]) (otrasDimensiones :: [Symbol]) :: Bool where
--   DimensionesSonIguales dimensiones dimensiones = True
--   DimensionesSonIguales _ _ = False

-- type family TienenMismasDimensiones (unaMedida :: Medida) (otraMedida :: Medida) :: Bool where
--   TienenMismasDimensiones unaMedida otraMedida = DimensionesSonIguales (Dimensiones unaMedida) (Dimensiones otraMedida)

-- type family MismaDimension (unaMedida :: Medida) (otraMedida :: Medida) :: Constraint where
--   MismaDimension medida medida = ()
--   MismaDimension (MedidaSimple dimension _) (MedidaSimple dimension _) = ()
--   MismaDimension (Producto d1 d2) (Producto d3 d4) = (MismaDimension d1 d3, MismaDimension d2 d4)
--   MismaDimension unaMedida otraMedida = TypeError (Text "Estás sumando 🍐s con 🍌s, o mas bien, " :<>: Text (Dimension unaMedida) :<>: Text " con " :<>: Text (Dimension otraMedida))

-- type family MismaDimension (unaMedida :: Medida) (otraMedida :: Medida) :: Constraint where
--   MismaDimension unaMedida otraMedida =
--     IffC (TienenMismasDimensiones unaMedida otraMedida) ()
--                                                         (TypeError (Text "Estás sumando 🍐s con 🍌s, o mas bien, " :<>: Text (Dimension unaMedida) :<>: Text " con " :<>: Text (Dimension otraMedida)))

type family Dimension (medida :: Medida) :: Symbol where
  Dimension Escalar = "escalar"
  Dimension (MedidaSimple dimension _) = dimension
  Dimension (Producto unaMedida otraMedida) = AppendSymbol (AppendSymbol (Dimension unaMedida) " * ") (Dimension otraMedida)
  Dimension (Division unaMedida otraMedida) = AppendSymbol (AppendSymbol (Dimension unaMedida) " / ") (Dimension otraMedida)

-- type family Dimensiones (medida :: Medida) where
--   Dimensiones Escalar = '[]
--   Dimensiones (MedidaSimple dimension _) = dimension ': '[]
--   Dimensiones (Producto '[]) = '[]
--   Dimensiones (Producto (medida ': medidas)) = Sort ((Dimensiones medida) ++ (Dimensiones (Producto medidas)))
--   Dimensiones (Division '[]) = '[]
--   Dimensiones (Division (medida ': medidas)) = Sort ((Dimensiones medida) ++ (Dimensiones (Division medidas)))

-- type family (++) (as :: [k]) (bs :: [k]) :: [k] where
--   (++) a '[] = a
--   (++) '[] b = b
--   (++) (a ': as) bs = a ': (as ++ bs)

-- type family Sort xs where
--   Sort '[] = '[]
--   Sort (x ': xs) = Insert x (Sort xs)

-- type family Insert x xs where
--   Insert x '[] = x ': '[]
--   Insert x (y ': ys) = Insert' (CmpSymbol x y) x y ys

-- type family Insert' b x y ys where
--   Insert' 'LT  x y ys = x ': (y ': ys)
--   Insert' _    x y ys = y ': Insert x ys

type family (/) (medida :: Medida) (otraMedida :: Medida) :: Medida where
  (/) medida medida = Escalar
  (/) a (Producto b c) = a / b / c
  (/) (Producto unaMedida otraMedida) unaMedida = otraMedida
  (/) (Producto unaMedida otraMedida) otraMedida = unaMedida
  (/) (Producto a b) c = (a / c) * (b / c) * c
  (/) (Division unaMedida otraMedida) unaMedida = Escalar / otraMedida
  (/) (Division a b) (Division c d) = (a * d) / (c * b)
  (/) unaMedida otraMedida = Division unaMedida otraMedida

type family (*) (medida :: Medida) (otraMedida :: Medida) :: Medida where
  (*) Escalar medida = medida
  (*) medida Escalar = medida
  (*) (Division a b) (Division c a) = c / b
  (*) (Division a b) (Division b c) = a / c
  (*) medida (Division numerador denominador) = medida * numerador / denominador
  (*) (Division numerador denominador) (Producto a b) = (Division numerador denominador) * a * b
  (*) (Division numerador denominador) medida = IfMismaDimension denominador medida
                                                  numerador
                                                  (Division (numerador * medida) denominador)
  (*) (Producto a b) (Producto c d) = a * b * c * d
  (*) (Producto a b) medida = AgregarFactor (Producto a b) medida
  (*) medida (Producto a b) = (Producto a b) * medida
  (*) medida otraMedida = IfLessThan (CmpDimension medida otraMedida)
                                     (Producto medida otraMedida)
                                     (Producto otraMedida medida)

type family TienenMismaDimension (unaMedida :: Medida) (otraMedida :: Medida) :: Constraint where
  TienenMismaDimension unaMedida otraMedida = IffC (MismaDimension unaMedida otraMedida)
                                                    ()
                                                    (TypeError (Text "Estás sumando 🍐s con 🍌s, o mas bien, " :<>: Text (Dimension unaMedida) :<>: Text " con " :<>: Text (Dimension otraMedida)))

type family IfMismaDimension (unaMedida :: Medida) (otraMedida :: Medida) (ifTrue :: sometype) (ifFalse :: sometype) :: sometype where
  IfMismaDimension unaMedida otraMedida ifTrue ifFalse = If (MismaDimension unaMedida otraMedida) ifTrue ifFalse

type family MismaDimension (unaMedida :: Medida) (otraMedida :: Medida) :: Bool where
  MismaDimension unaMedida otraMedida = (Dimension unaMedida) == (Dimension otraMedida)

type family CmpDimension (medida :: Medida) (otraMedida :: Medida) :: Ordering where
  CmpDimension unaMedida otraMedida = CmpSymbol (Dimension unaMedida) (Dimension otraMedida)

type family AgregarFactor (medida :: Medida) (otraMedida :: Medida) :: Medida where
  AgregarFactor (Producto a b) c = AgregarFactorAux (Producto a b) c (CmpDimension a c) (CmpDimension b c)

type family AgregarFactorAux (medida :: Medida) (otraMedida :: Medida) (cmpAyC :: Ordering) (cmpByC :: Ordering) where
  AgregarFactorAux (Producto a (Producto ba bb)) c LT LT = Producto a (AgregarFactor (Producto ba bb) c)
  AgregarFactorAux (Producto a b) c LT LT = Producto a (Producto b c)
  AgregarFactorAux (Producto a b) c LT _ = Producto a (Producto c b)
  AgregarFactorAux (Producto a b) c _ _ = Producto c (Producto a b)

type family IfLessThan (cmp :: Ordering) (a :: sometype) (b :: sometype) :: sometype where
  IfLessThan LT a _ = a
  IfLessThan _ _ b = b

a :: Medicion (Metro / Metro)
a = undefined :: Medicion Escalar

b :: Medicion (Metro / Metro)
b = undefined :: Medicion (Metro / Metro / Segundo * Segundo)

c :: Medicion (Escalar)
c = undefined :: Medicion ((Metro * Segundo) / (Segundo * Metro))

d :: Medicion (Escalar)
d = undefined :: Medicion ((Metro * Segundo * Litro) / (Litro * Metro * Segundo))