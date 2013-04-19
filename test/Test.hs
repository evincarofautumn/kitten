import Test.Hspec

import qualified Test.Term
import qualified Test.Token

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Test.Term" Test.Term.spec
  describe "Test.Token" Test.Token.spec
