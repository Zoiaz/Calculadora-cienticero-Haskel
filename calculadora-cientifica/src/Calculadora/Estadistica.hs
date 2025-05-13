module Calculadora.Estadistica (
    promedio,
    mediana,
    desviacionEstandar,
    permutaciones,
    combinaciones,
    coeficienteBinomial
) where

import Data.List (sort)
import qualified Calculadora.Basicas as Bas

-- Promedio
promedio :: [Double] -> Either String Double
promedio [] = Left "Lista vacía"
promedio xs = Right $ sum xs / fromIntegral (length xs)

-- Mediana
mediana :: [Double] -> Either String Double
mediana [] = Left "Lista vacía"
mediana xs
    | odd n     = Right $ sorted !! middle
    | otherwise = Right $ (sorted !! (middle - 1) + sorted !! middle) / 2
  where
    sorted = sort xs
    n = length xs
    middle = n `div` 2

-- Desviación estándar
desviacionEstandar :: [Double] -> Either String Double
desviacionEstandar [] = Left "Lista vacía"
desviacionEstandar xs = do
    prom <- promedio xs
    let var = sum [(x - prom)^2 | x <- xs] / fromIntegral (length xs)
    Right $ sqrt var

-- Factorial auxiliar para enteros no negativos
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Permutaciones: P(n, r) = n! / (n - r)!
permutaciones :: Integer -> Integer -> Either String Integer
permutaciones n r
    | r > n || n < 0 || r < 0 = Left "Valores inválidos"
    | otherwise = Right $ factorial n `div` factorial (n - r)

-- Combinaciones: C(n, r) = n! / (r! * (n - r)!)
combinaciones :: Integer -> Integer -> Either String Integer
combinaciones n r
    | r > n || n < 0 || r < 0 = Left "Valores inválidos"
    | otherwise = Right $ factorial n `div` (factorial r * factorial (n - r))

-- Coeficiente binomial (igual que combinaciones)
coeficienteBinomial :: Integer -> Integer -> Either String Integer
coeficienteBinomial = combinaciones
