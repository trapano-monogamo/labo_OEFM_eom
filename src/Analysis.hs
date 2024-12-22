module Analysis
( earthMagneticField
, earthMagneticFieldError
, eom
, eomError
, eomRegressionPoints
, covariance
) where

import LabParameters

degToRad :: Float -> Float
degToRad d = (pi / 180) * d

covariance :: [(Float,Float)] -> Float
covariance l = s / (fromIntegral $ length l)
  where (xs,ys) = unzip l
        xMean = (sum xs) / (fromIntegral $ length xs)
        yMean = (sum ys) / (fromIntegral $ length ys)
        s = sum $ zipWith (*) (map (\x -> x-xMean) xs) (map (\y -> y-yMean) ys)


earthMagneticField :: [[Float]] -> [Float]
earthMagneticField = map op
  where op (deg:i:br:bz:[]) = 10**(-7) * (i/i0) * (bz * (1/(tan theta)) + br)
          where theta = degToRad deg
        op _ = 0

earthMagneticFieldError :: [[Float]] -> [Float]
earthMagneticFieldError = map op
  where op (deg:i:br:bz:[]) = 10**(-7) * (sqrt $ t1**2 + t2**2)
          where t1 = (i/i0) * (bz / ((sin theta)**2)) * (degToRad angleError) -- angle term
                t2 = compassCurrentError * (bz * (1 / (tan theta)) + br) / i0 -- current term
                theta = degToRad deg
        op _ = 0

eom :: Float -> [[Float]] -> [Float]
eom earthField = map op
  where op (v:_:r:i:corr:[]) = em
          where b0 = constant * i / coilsRadius
                br = corr * b0 + earthField
                em = 2*v / ((r*br)**2)
        op _ = 0


{- Attempts:

1. whatever this is:

  t1 = 2*tensionError / ( (r*corr*constant*i / coilsRadius)**2 ) -- tension term
  t2 = 4*radiusError*v / (r*(corr*constant*i/coilsRadius)**2) -- radius term
  t3 = 4*currentError*v / (i*(r*corr*constant/coilsRadius)**2) -- current term
  t4 = 4*coilsRadiusError*v*coilsRadius / ((r*corr*constant*i)**2) -- coils radius term
  b = corr*constant * i/coilsRadius + earthField
  bErr = sqrt $ earthFieldError**2 + (coilsRadiusError*constant*i/coilsRadius**2)**2


2. removed errors on J and V for some unknown reason:

  t1 = 4*v*radiusError / (b**2 * r)
  t2 = 4*v*bErr / (b * r**2)
  b = corr*constant * i/coilsRadius + earthField
  bErr = sqrt $ earthFieldError**2 + (coilsRadiusError*constant*i/coilsRadius**2)**2


3. bErr corrected:

  t1 = 4 * tensionError / (r**2 * b**2)
  t2 = 8 * v * radiusError / (r**3 * b**2)
  t3 = 8 * v * bErr / (r**2 * b**3)
  b = corr*constant * i/coilsRadius + earthField
  bErr = sqrt $ (constant * currentError / coilsRadius)**2 + earthFieldError**2 + (coilsRadiusError*constant*i/coilsRadius**2)**2

-}

eomError :: Float -> Float -> [[Float]] -> [Float]
eomError earthField earthFieldError = map op
  where op (v:_:r:i:corr:[]) = sqrt $ t1**2 + t2**2 + t3**2
          where b = corr*constant * i/coilsRadius + earthField
                bErr = sqrt $ (constant * currentError / coilsRadius)**2 + (coilsRadiusError*constant*i/coilsRadius**2)**2 + earthFieldError**2
                t1 = 4 * tensionError / (r**2 * b**2)
                t2 = 8 * v * radiusError / (r**3 * b**2)
                t3 = 8 * v * bErr / (r**2 * b**3)
        op _ = 0

eomRegressionPoints :: Float -> Float -> [[Float]] -> [(Float,Float,Float,Float)]
eomRegressionPoints earthField earthFieldError = map op
  where op (v:_:r:i:corr:[]) = ( 2 * v
                               , b**2 * r**2
                               , 2 * tensionError
                               , 2 * b**2 * r * radiusError + 2 * r**2 * b * bErr)
          where b = corr * constant * i / coilsRadius + earthField
                bErr = sqrt $ earthFieldError**2 + (coilsRadiusError*constant*i/coilsRadius**2)**2
        op _ = (0,0,0,0)
