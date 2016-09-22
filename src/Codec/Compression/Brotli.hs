module Codec.Compression.Brotli (
                  compress
                  ) where



import            Control.Monad.Trans.Class               (lift)

import            System.IO.Unsafe                        (unsafePerformIO)

import            Codec.Compression.Brotli.Internal.FFI
import qualified  Data.ByteString                         as B
import qualified  Data.ByteString.Lazy                    as LB



-- | Compress the whole string as a brotli file.
--   This implementation is lazy, if that helps in some
--   way.
compress :: LB.ByteString -> LB.ByteString
compress input = unsafePerformIO $ do
    enc <- createEncoder
    let
      input_cnks = LB.toChunks input
    new_chunks <- go enc input_cnks
    return $ LB.fromChunks new_chunks
  where
    go enc []  = do
        encFlush enc bROTLI_OPERATION_FINISH
    go enc (p:rest)  = do
        compressed_pieces <- encAddInput enc p
        other_pieces <- go enc rest
        return $ compressed_pieces ++ other_pieces
