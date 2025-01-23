import Test.Hspec
import qualified TestTypeMachine.Functions
import qualified TestTypeMachine.Is

main :: IO ()
main = hspec $ do
    TestTypeMachine.Functions.specs
    TestTypeMachine.Is.specs
