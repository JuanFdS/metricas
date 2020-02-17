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

import PdePreludat
import Library
import Test.TypeSpec
import Test.TypeSpecCrazy

main :: IO ()
main = print specs

specs ::
    "Operadores de medidas"
    ########################
        "Multiplicación"
        ~~~~~~~~~~~~~~~
            "Conmutatividad"
            ~~~~~~~~~~~~~~~
              It "es conmutativa para escalares con medidas simples" (
                (Escalar * Metro) `Is` (Metro * Escalar)
              )
            -*
              It "es conmutativa entre medidas simples" (
                (Metro * Segundo) `Is` (Segundo * Metro)
              )
            -*
              It "es conmutativa para escalares con productos" (
                (Escalar * MetrosCuadrados) `Is` (MetrosCuadrados * Escalar)
              )
            -*
              It "es conmutativa para medidas simples con productos" (
                (Segundo * MetrosCuadrados) `Is` (MetrosCuadrados * Segundo)
              )
            -*
              It "es conmutativa para productos entre si" (
                (SegundosCuadrados * MetrosCuadrados) `Is` (MetrosCuadrados * SegundosCuadrados)
              )
            -*
              It "es conmutativa para divisiones con escalares" (
                (Escalar * MetrosPorSegundo) `Is` (MetrosPorSegundo * Escalar)
              )
            -*
              It "es conmutativa para divisiones con medidas simples que son de la misma dimension que el numerador" (
                (Metro * MetrosPorSegundo) `Is` (MetrosPorSegundo * Metro)
              )
            -*
              It "es conmutativa para divisiones con medidas simples que son de la misma dimension que el denominador" (
                (Segundo * MetrosPorSegundo) `Is` (MetrosPorSegundo * Segundo)
              )
            -*
              It "es conmutativa para divisiones con medidas simples que no estan presentes en la division" (
                (Litro * MetrosPorSegundo) `Is` (Litro * MetrosPorSegundo)
              )
            -*
              It "es conmutativa para divisiones con productos" (
                  (MetrosCuadrados * MetrosPorSegundo) `Is` (MetrosPorSegundo * MetrosCuadrados)
                -*
                  (SegundosCuadrados * MetrosPorSegundo) `Is` (MetrosPorSegundo * SegundosCuadrados)
                -*
                  (LitrosCuadrados * MetrosPorSegundo) `Is` (MetrosPorSegundo * LitrosCuadrados)
              )
            -*
              It "es conmutativa entre divisiones" (
                  (SegundosPorMetro * MetrosPorSegundo) `Is` (MetrosPorSegundo * SegundosPorMetro)
                -*
                  (MetrosPorSegundo * LitrosPorSegundo) `Is` (LitrosPorSegundo * MetrosPorSegundo)
                -*
                  (SegundosPorMetro * LitrosPorSegundo) `Is` (LitrosPorSegundo * SegundosPorMetro)
              )
        -/-
            "Elemento neutro"
            ~~~~~~~~~~~~~~~~~~
              It "su neutro es el escalar" (
                  (Escalar * Escalar) `Is` Escalar
                -*
                  (Escalar * Metro) `Is` Metro
                -*
                  (Metro * Escalar) `Is` Metro
              )
        -/-
            "Asociatividad"
            ~~~~~~~~~~~~~~~~~~
              It "es asociativa" (
                TheseAreEqual ((Metro * Segundo) * Litro)
                              (Metro * (Segundo * Litro))
              -*
                TheseAreEqual (Metro * Segundo * Litro)
                              (Metro * (Segundo * Litro))
              )
specs = Valid
