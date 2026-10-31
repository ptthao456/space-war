module Game.StateSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Game.State
import Game.TestUtil
import Game.Types

spec :: Spec
spec = do
  describe "getPlayer / setPlayer / modifyPlayer" $ do
    it "doc dung nguoi choi" $ do
      playerPos (getPlayer Player1 baseState) `shouldBe` Pos 50 50
      playerPos (getPlayer Player2 baseState) `shouldBe` Pos 50 40
    it "setPlayer chi doi mot nguoi" $ do
      let p = (getPlayer Player1 baseState) { playerBattery = 7 }
          gs = setPlayer Player1 p baseState
      playerBattery (getPlayer Player1 gs) `shouldBe` 7
      getPlayer Player2 gs `shouldBe` getPlayer Player2 baseState
    it "modifyPlayer ap dung ham len dung nguoi" $ do
      let gs = modifyPlayer Player2 (\p -> p { playerAP = 1 }) baseState
      playerAP (getPlayer Player2 gs) `shouldBe` 1
      playerAP (getPlayer Player1 gs) `shouldBe` 5

  describe "otherPlayer" $
    it "doi nguoi, goi hai lan thi ve lai" $ do
      otherPlayer Player1 `shouldBe` Player2
      otherPlayer (otherPlayer Player2) `shouldBe` Player2

  describe "planetAt" $ do
    it "tim thay hanh tinh dung o" $
      fmap planetId (planetAt (Pos 60 60) baseState) `shouldBe` Just 1
    it "o khong co hanh tinh thi Nothing" $
      planetAt (Pos 61 60) baseState `shouldBe` Nothing

  describe "tai nguyen" $ do
    it "addResources cong tung loai" $
      addResources (rs [(Iron, 2), (Crystal, 1)]) (rs [(Iron, 3), (RareOre, 4)])
        `shouldBe` rs [(Iron, 5), (Crystal, 1), (RareOre, 4)]
    it "removeResources tru du thi tra Just va bo loai bang 0" $
      removeResources (rs [(Iron, 5), (Crystal, 2)]) (rs [(Iron, 5)])
        `shouldBe` Just (rs [(Crystal, 2)])
    it "removeResources thieu so luong thi Nothing" $
      removeResources (rs [(Iron, 2)]) (rs [(Iron, 3)]) `shouldBe` Nothing
    it "removeResources loai khong co thi Nothing" $
      removeResources (rs [(Iron, 2)]) (rs [(Crystal, 1)]) `shouldBe` Nothing
    it "totalUnits cong so don vi" $
      totalUnits (rs [(Iron, 2), (Crystal, 3)]) `shouldBe` 5
    it "resourcesValue tinh diem theo Config" $
      resourcesValue cfg (rs [(Iron, 2), (Crystal, 1), (ExoticMatter, 1)])
        `shouldBe` 2 * 1 + 1 * 3 + 1 * 25
    it "playerScore chi tinh kho Earth, khong tinh cargo" $ do
      let p = (getPlayer Player1 baseState)
                { playerCargo = rs [(ExoticMatter, 10)]
                , playerStorage = rs [(Crystal, 2)]
                }
      playerScore cfg p `shouldBe` 6
    it "kho rong co diem 0" $
      resourcesValue cfg Map.empty `shouldBe` 0