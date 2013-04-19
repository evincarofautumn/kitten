import Test.Hspec

import qualified Test.Token

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Test.Token" Test.Token.spec
