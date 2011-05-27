import qualified Data.ByteString as S

main = S.writeFile "sample/binary" $ S.pack [50, 51, 52, 0, 49, 50, 51]
