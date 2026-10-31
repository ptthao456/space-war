module Game.WorldSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Config
import Game.Types
import Game.World


-- | Sinh Pos ngau nhien nho (de test khong tran so).
genPos :: Gen Pos
genPos = Pos <$> choose (-50, 150) <*> choose (-50, 150)

spec :: Spec
spec = do
  describe "distance" $ do
    it "Manhattan: (0,0) -> (3,4) = 7" $
      distance (Pos 0 0) (Pos 3 4) `shouldBe` 7
    it "doi xung" $ property $
      forAll genPos $ \a -> forAll genPos $ \b -> distance a b == distance b a
    it "khoang cach toi chinh no la 0" $ property $
      forAll genPos $ \a -> distance a a == 0
    it "bat dang thuc tam giac" $ property $
      forAll genPos $ \a -> forAll genPos $ \b -> forAll genPos $ \c ->
        distance a c <= distance a b + distance b c

  describe "inBounds" $ do
    it "(0,0) va (99,99) trong map 100x100" $ do
      inBounds defaultConfig (Pos 0 0) `shouldBe` True
      inBounds defaultConfig (Pos 99 99) `shouldBe` True
    it "(-1,0), (0,-1), (100,0), (0,100) ngoai map" $ do
      inBounds defaultConfig (Pos (-1) 0) `shouldBe` False
      inBounds defaultConfig (Pos 0 (-1)) `shouldBe` False
      inBounds defaultConfig (Pos 100 0) `shouldBe` False
      inBounds defaultConfig (Pos 0 100) `shouldBe` False

  describe "allCells" $
    it "du 100x100 = 10000 o" $
      length (allCells defaultConfig) `shouldBe` 10000

  describe "movePos" $ do
    it "DirUp giam y, DirDown tang y" $ do
      movePos DirUp (Pos 5 5) `shouldBe` Pos 5 4
      movePos DirDown (Pos 5 5) `shouldBe` Pos 5 6
    it "DirLeft giam x, DirRight tang x" $ do
      movePos DirLeft (Pos 5 5) `shouldBe` Pos 4 5
      movePos DirRight (Pos 5 5) `shouldBe` Pos 6 5
    it "moi buoc di cach dung 1 o" $ property $
      forAll genPos $ \p -> all (\d -> distance p (movePos d p) == 1) [minBound .. maxBound]

  describe "compassFrom" $ do
    it "cung o -> Here" $
      compassFrom (Pos 3 3) (Pos 3 3) `shouldBe` Here
    it "bon huong chinh" $ do
      compassFrom (Pos 0 0) (Pos 0 (-5)) `shouldBe` North
      compassFrom (Pos 0 0) (Pos 0 5) `shouldBe` South
      compassFrom (Pos 0 0) (Pos 5 0) `shouldBe` East
      compassFrom (Pos 0 0) (Pos (-5) 0) `shouldBe` West
    it "bon huong cheo" $ do
      compassFrom (Pos 0 0) (Pos 3 3) `shouldBe` SouthEast
      compassFrom (Pos 0 0) (Pos 3 (-3)) `shouldBe` NorthEast
      compassFrom (Pos 0 0) (Pos (-3) 3) `shouldBe` SouthWest
      compassFrom (Pos 0 0) (Pos (-4) (-4)) `shouldBe` NorthWest
    it "quy tac 2:1: lech dung 2 lan thi tinh la huong chinh" $ do
      compassFrom (Pos 0 0) (Pos 4 2) `shouldBe` East
      compassFrom (Pos 0 0) (Pos 2 4) `shouldBe` South
    it "lech chua toi 2 lan thi la huong cheo" $
      compassFrom (Pos 0 0) (Pos 3 2) `shouldBe` SouthEast

  describe "withinRadius" $ do
    it "tren bien ban kinh thi tinh la trong" $
      withinRadius 3 (Pos 0 0) (Pos 3 0) `shouldBe` True
    it "ngoai ban kinh 1 o thi khong" $
      withinRadius 3 (Pos 0 0) (Pos 2 2) `shouldBe` False

  describe "ringCells" $ do
    it "quanh Earth cach 1 den 5 o co dung 60 o" $
      length (ringCells defaultConfig (Pos 50 50) 1 5) `shouldBe` 60
    it "o goc map chi con 2 o cach dung 1" $
      length (ringCells defaultConfig (Pos 0 0) 1 1) `shouldBe` 2
    it "moi o tra ve deu trong map va dung tam khoang cach" $ property $
      forAll (choose (0, 99)) $ \x -> forAll (choose (0, 99)) $ \y ->
        let c = Pos x y
        in all (\p -> inBounds defaultConfig p
                      && distance c p >= 2 && distance c p <= 4)
               (ringCells defaultConfig c 2 4)