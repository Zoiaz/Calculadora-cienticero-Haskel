module Calculadora.Basicas (suma, resta, multiplicacion, division, potencia, raizCuadrada, modulo, factorial) where

-- Suma
suma :: Double -> Double -> Double
suma x y = x + y

-- Resta
resta :: Double -> Double -> Double
resta x y = x - y

-- Multiplicación
multiplicacion :: Double -> Double -> Double
multiplicacion x y = x * y

-- División segura
division :: Double -> Double -> Either String Double
division _ 0 = Left "Error: No se puede dividir entre cero."
division x y = Right (x / y)

-- Potencia
potencia :: Double -> Double -> Double
potencia x y = x ** y

-- Raíz cuadrada segura
raizCuadrada :: Double -> Either String Double
raizCuadrada x
    | x < 0     = Left "Error: No se puede sacar raíz de un número negativo."
    | otherwise = Right (sqrt x)

-- Módulo (resto de división entera)
modulo :: Int -> Int -> Either String Int
modulo _ 0 = Left "Error: División entre cero."
modulo x y = Right (mod x y)

-- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

