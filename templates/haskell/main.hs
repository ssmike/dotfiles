import Control.Monad(liftM, forM_)
import qualified Data.ByteString.Char8 as B
import Data.Maybe

readI :: B.ByteString -> Integer
readI = fst . fromJust . B.readInteger

main = undefined
