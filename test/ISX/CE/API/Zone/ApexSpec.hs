module ISX.CE.API.Zone.ApexSpec (spec) where


import ISX.CE.API.Test


spec :: Spec
spec = snapAPI $
    describe "apex" $
        it "=> 200" $ do
            let req = get "/" emptyP
            res <- runRequest req
            rspStatus res `shouldBe` 200
            b <- getResponseBody res
            b ^. key "t_now" . _String `shouldContain` "T"
            b ^. key "version" . _String `shouldBe` "0.0.0"
            b ^. _Object `shouldMeasure` 2
