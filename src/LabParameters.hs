module LabParameters
( coilsRadius
, coilsRadiusError
, coilsNumber
, mu0
, tensionError
, currentError
, constant
, radiusError
, i0
, compassCurrentError
, compassCoilsNumber
, angleError
) where


mu0 = 1.25663706143592E-06 :: Float
coilsNumber = 130 :: Int
coilsRadius = 0.15504 :: Float -- m
coilsRadiusError = 0.0004 :: Float -- m
needleRadius = 0.05 :: Float -- m

tensionError = 0.1 :: Float -- V
currentError = 0.001 :: Float -- mA
-- mu0 * (4/5 ^ 3/2) * coilsNumber
constant = 0.000116892917124518 :: Float

-- radiusError = 0.0015 :: Float -- m
radiusError = 0.003 :: Float -- m

i0 = 100 :: Float -- mA
compassCurrentError = 0.01 :: Float -- mA
compassCoilsNumber = 240 :: Int

angleError = 1 :: Float -- rad
