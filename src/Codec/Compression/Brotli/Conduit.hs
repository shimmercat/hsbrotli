module Codec.Compression.Brotli.Conduit (
                  compressDelimited,
                  compress
                  ) where


import            Control.Monad.Trans.Class               (lift)

import            Codec.Compression.Brotli.Internal.FFI
import            Data.Conduit
import qualified  Data.ByteString                          as B


-- | In order to have some control over the streaming process,
--   the following type can be used to hint a flush from time
--   to time.
data DelimitedData =
    Flush_DD                -- ^ Means that the compressor is flushed at this point
    | Data_DD B.ByteString  -- ^ Well just more data to the stream



compressDelimited :: Conduit DelimitedData IO B.ByteString
compressDelimited = do
    enc <- lift createEncoder
    go enc
  where
    go enc = do
        maybe_data <- await
        case maybe_data of
            Nothing -> do
                pieces <- lift $ encFlush enc bROTLI_OPERATION_FINISH
                mapM_ yield pieces
            Just Flush_DD -> do
                pieces <- lift $ encFlush enc bROTLI_OPERATION_FLUSH
                mapM_ yield pieces
                go enc

            Just (Data_DD b) -> do
                pieces <- lift $ encAddInput enc b
                mapM_ yield pieces
                go enc


compress :: Conduit B.ByteString IO B.ByteString
compress = mapInput
   Data_DD
   (const Nothing)
   compressDelimited
