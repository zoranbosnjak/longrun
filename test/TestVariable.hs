module TestVariable (
    testVariable
) where

import System.Log.Logger hiding (logM)
import Test.Framework (buildTest, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Utils (assertConstantMemory, runAppWithoutLogging, testLogsOfMatch)
import qualified Control.Concurrent.Longrun as Longrun


testVariable :: Test
testVariable = testGroup "testVariable"
    [ testVar1
    , testVar2
    , testChvar
    , testLongChvar
    ]

-- | Basic variable
var1 :: Longrun.Process ()
var1 = do
    var <- Longrun.newVar "content"
    val1 <- Longrun.getVar var
    Longrun.logM INFO $ show val1
    Longrun.setVar var "new content"
    val2 <- Longrun.getVar var
    Longrun.logM INFO $ show val2

testVar1 :: Test
testVar1 = testLogsOfMatch "var1" DEBUG var1
    [ (INFO, "\"content\"")
    , (INFO, "\"new content\"")
    ]


-- | Modify variable
var2 :: Longrun.Process ()
var2 = do
    var <- Longrun.newVar (1::Int)
    (oldVal,newVal) <- Longrun.modifyVar var (+1)
    val <- Longrun.getVar var
    Longrun.logM INFO $ show oldVal
    Longrun.logM INFO $ show newVal
    Longrun.logM INFO $ show val

testVar2 :: Test
testVar2 = testLogsOfMatch "var2" DEBUG var2
    [ (INFO, "1")
    , (INFO, "2")
    , (INFO, "2")
    ]


-- | Variable change
chvar :: Longrun.Process ()
chvar = do
    var <- Longrun.newVar (1::Int)
    _ <- Longrun.spawnProcess $ Longrun.onChangeVar 0 var $ \oldVal newVal -> do
        Longrun.logM INFO $ "changed: " ++ show oldVal ++ " -> " ++ show newVal

    Longrun.sleep 0.1
    Longrun.setVar var 2
    Longrun.sleep 0.1
    Longrun.setVar var 3
    Longrun.sleep 0.1

testChvar :: Test
testChvar = testLogsOfMatch "variable change" INFO chvar
    [ (INFO, "changed: 0 -> 1")
    , (INFO, "changed: 1 -> 2")
    , (INFO, "changed: 2 -> 3")
    ]


testLongChvar :: Test
testLongChvar = buildTest $ fmap (testCase "long running onChangeVar") $
    runAppWithoutLogging $ do
        var <- Longrun.newVar False
        mutator <- Longrun.spawnProcess $
            Longrun.forever $ do
                Longrun.modifyVar_ var not
                Longrun.sleep 0.0001
        Longrun.spawnProcess_ $
             Longrun.onChangeVar False var $ \_ _ -> return ()
        assertion <- assertConstantMemory 100 1.2 $
            Longrun.sleep 0.001
        Longrun.stop_ mutator
        return assertion
