module Main (main) where

import System.IO

import Statistics.Distribution
import Statistics.Distribution.StudentT

import LabParameters
import Analysis


colorRed = "\ESC[31m" :: String
colorGreen = "\ESC[32m" :: String
colorDefault = "\ESC[0m" :: String


weightedAverage :: [(Float,Float)] -> (Float,Float)
weightedAverage l = ( avg, err )
  where (xs,ws) = unzip $ map (\(x,y) -> (x, 1 / (y**2))) l
        avg = (sum (zipWith (*) xs ws)) / (sum ws)
        err = sqrt (1 / (sum ws))


stdAverage :: [(Float,Float)] -> (Float,Float)
stdAverage l = ( avg, sqrt $ var / (fromIntegral $ length xs) )
  where (xs,_) = unzip l
        avg = (sum xs) / (fromIntegral $ length xs)
        var = (sum $ map (\x -> (x - avg)**2) xs) / ((fromIntegral $ length xs) - 1)



singleTailCriticalTValue :: Float -> Int -> Float
singleTailCriticalTValue significance df = realToFrac $ quantile (studentT $ fromIntegral df) (1 - realToFrac significance)

doubleTailCriticalTValue :: Float -> Int -> Float
doubleTailCriticalTValue significance df = realToFrac $ q2 - q1
  where alpha = significance / 2
        q1 = quantile (studentT $ fromIntegral df) (realToFrac alpha)
        q2 = quantile (studentT $ fromIntegral df) (1 - realToFrac alpha)



simpleParser :: String -> [[Float]]
simpleParser = filter (not . null) . map (map read) . (map words) . lines



data TTest = ConfidenceInterval Float | SignificanceTest Float Float | NoTest

-- makeTTest :: Int -> Float -> IO ()
-- makeTTest df scaleFactor = {- ... -}




processFile :: String ->                               -- filename
               (String -> [[Float]]) ->                -- parser
               ([[Float]] -> [Float]) ->               -- operation on dataset
               ([[Float]] -> [Float]) ->               -- dataset errors
               ([(Float,Float)] -> (Float,Float)) ->   -- avg+-err statistical estimator
               String ->                               -- measurement units
               Float ->                                -- scale factor
               TTest ->                                -- t-test
               IO (Float, Float, [[Float]])            -- return (avg,err)
processFile path parseContents processData calcErrors bestEstimate units scaleFactor ttest = do
  putStrLn $ colorGreen ++ "\n[*] Processing " ++ path ++ colorDefault

  withFile path ReadMode (\handle -> do
    -- read file, get the data and calculate stuff
    contents <- hGetContents handle
    let rawData = parseContents contents
        processedData = processData rawData
        errors = calcErrors rawData
        (avg, err) = bestEstimate $ zip processedData errors
        degOfFreedom = (length processedData) - 1 -- 1 because average is the only constraint

    putStrLn "\nResults:"
    _ <- sequence $ map
      (\(x,e) -> putStrLn $ (show $ scaleFactor * x) ++ " +- " ++ (show $ scaleFactor * e) ++ " " ++ units)
      $ zip processedData errors

    putStrLn "\nFinal Measure:"
    putStrLn $ (show $ scaleFactor * avg) ++ " +- " ++ (show $ scaleFactor * err) ++ " " ++ units

    case ttest of
      -- consider implementing a way of testing against multiple significance levels
      SignificanceTest expectedValue significance -> do
        let t0 = (abs $ avg - expectedValue) / err
            tc = singleTailCriticalTValue significance degOfFreedom
            p0 = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac $ -t0)
            pc = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac $ -tc)
        putStrLn $ "\nStatistical Significance at " ++ (show $ 100 * significance) ++ "% for expected value of " ++ (show $ scaleFactor * expectedValue) ++ ":"
        putStrLn $ "t0:  P(-inf <= -" ++ (show t0) ++ ") = " ++ (show $ 100 * p0) ++ "%"
        putStrLn $ "t_c: P(-inf <= -" ++ (show tc) ++ ") = " ++ (show $ 100 * pc) ++ "%" -- just to check: P(<=tc) should be equal to significance
      ConfidenceInterval confidence -> do
        let tc = doubleTailCriticalTValue confidence degOfFreedom
            lowerBound = avg - tc * err
            upperBound = avg + tc * err
        putStrLn $ "\nMargins of error for confidence level of " ++ (show $ 100 * confidence) ++ "%:"
        putStrLn $ "tc = " ++ (show tc) ++ ": (" ++ (show $ scaleFactor * lowerBound) ++ ", " ++ (show $ scaleFactor * upperBound) ++ ")"
      NoTest -> do putStrLn "\nNo test to see here..."

    -- return results of calculations
    return (avg, err, rawData))


data LinReg = LinReg { slope :: Float
                     , intercept :: Float
                     , slopeError :: Float
                     , interceptError :: Float
                     , chi2 :: Float
                     , chi2r :: Float } deriving (Show)

linReg :: [(Float,Float,Float,Float)] -> LinReg
linReg regData =
  LinReg { slope = m
         , intercept = q
         , slopeError = em
         , interceptError = eq
         , chi2 = 0.0
         , chi2r = 0.0 }
  where (xs,ys,exs,eys) = foldl (\(accA,accB,accC,accD) (a,b,c,d) ->
            (accA ++ [a], accB ++ [b], accC ++ [c], accD ++ [d])
          ) ([],[],[],[]) regData
        testM = ((last ys) - (head ys)) / ((last xs) - (head xs))
        ws = map (\(ex,ey) -> 1 / ((testM*ex)**2 + ey**2)) $ zip exs eys
        sx = sum $ zipWith (*) xs ws
        sy = sum $ zipWith (*) ys ws
        sx2 = sum $ zipWith (*) (map (**2) xs) ws
        sxy = sum $ zipWith (*) (zipWith (*) xs ys) ws
        sw = sum ws
        d = sw * sx2 - sx**2
        m = (1/d) * (sw*sxy - sx*sy)
        q = (1/d) * (sx2*sy - sx*sxy)
        em = sqrt $ (1/d) * sw
        eq = sqrt $ (1/d) * sx2

linRegPointsToString :: [(Float,Float,Float,Float)] -> String
linRegPointsToString = foldl (\acc (a,b,c,d) -> acc ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ "\n") ""



-- ..:: Entry Point ::..




main :: IO ()
main = do
  (field,fieldError,_)   <- processFile "./data/cm_terrestre.csv"     (simpleParser) (earthMagneticField) (earthMagneticFieldError)   (weightedAverage) "T"    (1) (NoTest)
  (_,_,orthogonalData)   <- processFile "./data/cm_ortogonale.csv"    (simpleParser) (eom      0.0)       (eomError          0.0 0.0) (stdAverage) "C/Kg" (1) (NoTest)
  (_,_,parallelData)     <- processFile "./data/cm_parallelo.csv"     (simpleParser) (eom    field)       (eomError field fieldError) (stdAverage) "C/Kg" (1) (NoTest)
  (_,_,antiParallelData) <- processFile "./data/cm_antiparallelo.csv" (simpleParser) (eom (-field))       (eomError field fieldError) (stdAverage) "C/Kg" (1) (NoTest)

  let orthoRegPoints = eomRegressionPoints field fieldError orthogonalData
      parallelRegPoints = eomRegressionPoints field fieldError parallelData
      antiParallelRegPoints = eomRegressionPoints field fieldError antiParallelData
      orthoRegResults = linReg orthoRegPoints
      parallelRegResults = linReg parallelRegPoints
      antiParallelRegResults = linReg antiParallelRegPoints

  putStrLn $ colorRed ++ "\nLinear regression for 'cm_ortogonale.csv'" ++ colorDefault
  putStrLn $ show orthoRegResults
  putStrLn $ "e/m = " ++ (show $ 1 / (slope orthoRegResults))
  writeFile "./ortho_reg.csv" $ linRegPointsToString orthoRegPoints

  putStrLn $ colorRed ++ "\nLinear regression for 'cm_parallelo.csv'" ++ colorDefault
  putStrLn $ show parallelRegResults
  putStrLn $ "e/m = " ++ (show $ 1 / (slope parallelRegResults))
  writeFile "./parallel_reg.csv" $ linRegPointsToString parallelRegPoints

  putStrLn $ colorRed ++ "\nLinear regression for 'cm_antiparallelo.csv'" ++ colorDefault
  putStrLn $ show antiParallelRegResults
  putStrLn $ "e/m = " ++ (show $ 1 / (slope antiParallelRegResults))
  writeFile "./antiparallel_reg.csv" $ linRegPointsToString antiParallelRegPoints

  return ()