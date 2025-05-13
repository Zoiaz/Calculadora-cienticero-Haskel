module Calculadora.Trigonometria (
    gradosARadianes,
    radianesAGrados,
    seno,
    coseno,
    tangente,
    sinh',
    cosh',
    tanh'
) where


-- Todas las funciones usan radianes internamente

gradosARadianes :: Double -> Double
gradosARadianes g = g * pi / 180

radianesAGrados :: Double -> Double
radianesAGrados r = r * 180 / pi

seno :: Double -> Double
seno x = sin (gradosARadianes x)

coseno :: Double -> Double
coseno x = cos (gradosARadianes x)

tangente :: Double -> Double
tangente x = tan (gradosARadianes x)

sinh' :: Double -> Double
sinh' = sinh

cosh' :: Double -> Double
cosh' = cosh

tanh' :: Double -> Double
tanh' = tanh
