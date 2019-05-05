module Spec where
  import Test.HUnit

  import MetaHS.DataModel.MetaModel
  import MetaHS.EDSL.MetaModel

  import MetaHS.Extensions.MacroLevelAggregation.Average
  import MetaHS.Extensions.MacroLevelAggregation.Median
  import MetaHS.Extensions.MacroLevelAggregation.GiniCoefficient
  import MetaHS.Extensions.MacroLevelAggregation.IdealValueDeviation
  import MetaHS.Extensions.MacroLevelAggregation.Distribution

  import qualified Data.Set as Set
  import qualified Data.Map.Strict as Map

  -- | Creates a dummy metamodel for one kind of key with Int values. E.g., LCOM
  createTestMm :: [Int]         -- ^ List with Int values whicj shall be used for the Element value.
               -> RelationKey -- ^ Relation key to be simulated.
               -> MetaModel   -- ^ Dummy metamodel to run function under test on.
  createTestMm xs key = MetaModel msr
    where msr = Map.insert key rel Map.empty
          rel = Set.fromList ls
          ls = zip [createMod k | k <- [1 .. length xs]] [createVal v | v <- xs]
          createMod k = Module {name = "mod" ++ show k}
          createVal v = IntValue {intValue = v}

  -- AVERAGE
  testAverage0 =        TestCase $ assertEqual "Average_0 [0] == 0" 0 (average "LCOM" $ createTestMm [0] "LCOM")
  testAverageRatio =    TestCase $ assertEqual "Average_Ratio [1,2] == 1,5" 1.5 (average "LCOM" $ createTestMm [1,2] "LCOM")
  testAverageNeg =      TestCase $ assertEqual "Average_Neg [-2,1] == -0.5" (-0.5) (average "LCOM" $ createTestMm [-2,1] "LCOM")
  testAverageOutlier =  TestCase $ assertEqual "Average_Outlier [10,10,10,10,60,60,60,60,60,60,260] == 60" 60 (average "LCOM"
                                      $ createTestMm [10,10,10,10,60,60,60,60,60,60,260] "LCOM")

  -- MEDIAN
  testMedianEven =      TestCase $ assertEqual "Median_Even [2,2] == 2" 2 (median "LCOM" $ createTestMm [2,2] "LCOM")
  testMedianOdd =       TestCase $ assertEqual "Median_Odd [2,2,2] == 2" 2 (median "LCOM" $ createTestMm [2,2,2] "LCOM")
  testMedian0 =         TestCase $ assertEqual "Median_0 [0] == 0" 0 (median "LCOM" $ createTestMm [0] "LCOM")
  testMedianRatio =     TestCase $ assertEqual "Median_Ratio [1,2] == 1,5" 1.5 (median "LCOM" $ createTestMm [1,2] "LCOM")
  testMedianNeg =       TestCase $ assertEqual "Median_Neg [-2,1] == -0.5" (-0.5) (median "LCOM" $ createTestMm [-2,1] "LCOM")
  testMedianOutlier =   TestCase $ assertEqual "Median_Outlier [10,10,10,10,60,60,60,60,60,60,260] == 60" 60 (median "LCOM"
                                      $ createTestMm [10,10,10,10,60,60,60,60,60,60,260] "LCOM")

  -- GINI
  testGini0 =           TestCase $ assertEqual "Gini_0 [0] == 0" 0 (giniCoefficient "LCOM" $ createTestMm [0] "LCOM")
  testGiniEq =          TestCase $ assertEqual "Gini_0 [1,1] == 0" 0 (giniCoefficient "LCOM" $ createTestMm [1,1] "LCOM")
  testGiniHalf =        TestCase $ assertEqual "Gini_0 [0,0,1,1] == 0.5" 0.5 (giniCoefficient "LCOM" $ createTestMm [0,0,1,1] "LCOM")
  testGiniThreeQuart =  TestCase $ assertEqual "Gini_0 [0,0,0,1] == 0.75" 0.75 (giniCoefficient "LCOM" $ createTestMm [0,0,0,1] "LCOM")
  testGiniOneQuart =    TestCase $ assertEqual "Gini_0 [0,1,1,1] == 0.25" 0.25 (giniCoefficient "LCOM" $ createTestMm [0,1,1,1] "LCOM")
  testGiniToNeq =       TestCase $ assertEqual "Gini_0 [0,0,0,0,1] == 0.8" 0.8 (giniCoefficient "LCOM" $ createTestMm [0,0,0,0,1] "LCOM")
  testGiniAproxNeq =    TestCase $ assertEqual "Gini_0 [0,0,0,0,0,0,0,0,0,1] == 0.9" 0.9 (giniCoefficient "LCOM" $
                                          createTestMm [0,0,0,0,0,0,0,0,0,1] "LCOM")

  -- IVD lower-bound on 4, ideal value on 10, upper-bound on 20
  testIvdIdeal =        TestCase $ assertEqual "Ivd_Ideal [5,10,15] == 1" 1 (idealValueDeviation "LCOM"
                                             (createTestMm [5,10,15] "LCOM") 4 10 20)
  testIvdLowerBound =   TestCase $ assertEqual "Ivd_LowerBound [2,6] == 0" 0 (idealValueDeviation "LCOM"
                                               (createTestMm [2,6] "LCOM") 4 10 20)
  testIvdUpperBound =   TestCase $ assertEqual "Ivd_UpperBound [10,30] == 0" 0 (idealValueDeviation "LCOM"
                                               (createTestMm [10,30] "LCOM") 4 10 20)
  testIvdTooLow =       TestCase $ assertEqual "Ivd_TooLow [1,2,3] == 0" 0 (idealValueDeviation "LCOM"
                                               (createTestMm [1,2,3] "LCOM") 4 10 20)
  testIvdTooHigh =      TestCase $ assertEqual "Ivd_TooHigh [21,22,23] == 0" 0 (idealValueDeviation "LCOM"
                                               (createTestMm [21,22,23] "LCOM") 4 10 20)
  testIvdLowIdeal =     TestCase $ assertEqual "Ivd_LowIdeal [5,4,10,9] == 0.5" 0.5 (idealValueDeviation "LCOM"
                                               (createTestMm [5,4,10,9] "LCOM") 4 10 20)
  testIvdIdealHigh =    TestCase $ assertEqual "Ivd_IdealHigh [6,11,19,25] == 0.5" 0.5 (idealValueDeviation "LCOM"
                                               (createTestMm [6,11,19,25] "LCOM") 4 10 20)

  -- DISTRIBUTION
  testDist =           TestCase $ assertEqual "Dist [1,2,2,3,3,3] == [(1,1),(2,2),(3,3)]"
                                              (Set.fromList([(1,1),(2,2),(3,3)])) (distribution "LCOM" $
                                              createTestMm [1,2,2,3,3,3] "LCOM")

  testlist = TestList [ TestLabel "testAverage_0" testAverage0,
                        TestLabel "testAverage_ratio" testAverageRatio,
                        TestLabel "testAverage_neg" testAverageNeg,
                        TestLabel "testAverage_outlier" testAverageOutlier,

                        TestLabel "testMedian_0" testMedianEven,
                        TestLabel "testMedian_1" testMedianOdd,
                        TestLabel "testMedian_0" testMedian0,
                        TestLabel "testMedian_ratio" testMedianRatio,
                        TestLabel "testMedian_neg" testMedianNeg,
                        TestLabel "testMedian_outlier" testMedianOutlier,

                        TestLabel "testGini0" testGini0,
                        TestLabel "testGiniEq" testGiniEq,
                        TestLabel "testGiniHalf" testGiniHalf,
                        TestLabel "testGiniThreeQuart" testGiniThreeQuart,
                        TestLabel "testGiniOneQuart" testGiniOneQuart,
                        TestLabel "testGiniP5divP4" testGiniToNeq,
                        TestLabel "testGiniP5divP4"testGiniAproxNeq,

                        TestLabel "testIvdIdeal" testIvdIdeal,
                        TestLabel "testIvdLowerBound" testIvdLowerBound,
                        TestLabel "testIvdUpperBound" testIvdUpperBound,
                        TestLabel "testIvdTooLow" testIvdTooLow,
                        TestLabel "testIvdTooHigh" testIvdTooHigh,
                        TestLabel "testIvdLowIdeal" testIvdLowIdeal,
                        TestLabel "testIvdIdealHigh"testIvdIdealHigh,

                        TestLabel "testDist"testDist
                      ]

  main :: IO ()
  main = do
    runTestTT testlist
    return ()
