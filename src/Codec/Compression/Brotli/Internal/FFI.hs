{-# LANGUAGE ForeignFunctionInterface, MultiWayIf #-}
module Codec.Compression.Brotli.Internal.FFI (
                  createEncoder,
                  encAddInput,
                  encFlush,

                  bROTLI_OPERATION_FLUSH,
                  bROTLI_OPERATION_FINISH
                  ) where

import            Data.Int
import            Data.Word                            (Word8)
import qualified  Data.ByteString                      as B
import qualified  Data.ByteString.Unsafe               as B

import            Foreign.Ptr
import            Foreign.Marshal.Alloc
import            Foreign.C.Types
import            Foreign.ForeignPtr
import            Foreign.Storable

type VoidPtr = Ptr ()

newtype EncoderInstance = EncoderInstance ()

type EncoderInstancePtr = Ptr EncoderInstance
type EncoderInstanceFPtr = ForeignPtr EncoderInstance


bROTLI_OPERATION_PROCESS :: Int
bROTLI_OPERATION_PROCESS = 0
bROTLI_OPERATION_FLUSH :: Int
bROTLI_OPERATION_FLUSH = 1
bROTLI_OPERATION_FINISH :: Int
bROTLI_OPERATION_FINISH = 2
bROTLI_OPERATION_EMIT_METADATA :: Int
bROTLI_OPERATION_EMIT_METADATA = 3


-- Let's take care of the encoding functions first
foreign import ccall "BrotliEncoderCreateInstance" enc_create_instance::
    VoidPtr ->
    VoidPtr ->
    VoidPtr ->
    IO EncoderInstancePtr


foreign import ccall "&BrotliEncoderDestroyInstance" enc_destroy_instance::
    FunPtr( EncoderInstancePtr -> IO () )


foreign import ccall "BrotliEncoderCompress" enc_compress ::
    EncoderInstancePtr ->  --
    CInt ->  -- operation
    Ptr CSize ->  -- available_in,
    Ptr (Ptr Word8) -> --  next_in
    Ptr CSize  -> -- available out,
    Ptr (Ptr Word8) -> -- next_out,
    Ptr CSize -> -- total out
    IO Int8 -- bool thingy in return value


foreign import ccall "BrotliEncoderHasMoreOutput" enc_has_more_output ::
    EncoderInstancePtr ->
    IO Int8


createEncoder :: IO EncoderInstanceFPtr
createEncoder = do
    p <- enc_create_instance
            nullPtr
            nullPtr
            nullPtr
    newForeignPtr enc_destroy_instance p


-- | Adds more input to the encoder and gets all the output produced by the
--   encoder while processing that input.
--   Doesn't do any flush, and there can be data stuck in the encoder after
--   returning from here.
encAddInput ::
    EncoderInstanceFPtr ->
    B.ByteString ->
    IO [B.ByteString]
encAddInput
    encoder_instance_fptr
    input
    =
    withForeignPtr encoder_instance_fptr $ \ ep ->
        B.unsafeUseAsCStringLen input $ \ (pcchar, strlen) ->
            alloca $ \ csize_avail_in ->
                alloca $ \ ppnext_in ->  do
                    --
                    --
                    poke csize_avail_in (fromIntegral strlen :: CSize)
                    poke ppnext_in (castPtr pcchar :: Ptr Word8)
                    goIter
                        bROTLI_OPERATION_PROCESS
                        ep
                        csize_avail_in
                        ppnext_in



encFlush ::
    EncoderInstanceFPtr ->
    Int ->
    IO [B.ByteString]
encFlush
    encoder_instance_fptr op
    =
    withForeignPtr encoder_instance_fptr $ \ ep ->
        alloca $ \ csize_avail_in ->
            alloca $ \ ppnext_in ->  do
                --
                --
                poke csize_avail_in (0 :: CSize)
                goIter
                    op
                    ep
                    csize_avail_in
                    ppnext_in



goIter :: Int ->  EncoderInstancePtr -> Ptr CSize -> Ptr (Ptr Word8) -> IO [B.ByteString]
goIter op ep csize_avail_in ppnext_in =
    alloca $ \ pavail_out ->
        alloca $ \ ppnext_out ->
            alloca $ \ ptotal_out ->
                allocaBytes 4096 $ \ pnext_out -> do
                    --
                    --
                    poke ppnext_out (pnext_out :: Ptr Word8)
                    poke pavail_out (4096 :: CSize)
                    result <- enc_compress
                        ep
                        (fromIntegral op)
                        csize_avail_in
                        ppnext_in
                        pavail_out
                        ppnext_out
                        ptotal_out
                    if result /= 0
                      then do
                        avail_in <- peek csize_avail_in
                        new_pnext_out <- peek ppnext_out
                        total_out <- peek ptotal_out
                        if | avail_in > 0 && total_out == 0     -> error "BrotliCompressUnexpectedCase"
                           | avail_in == 0 && total_out == 0    -> return []
                           | avail_in < 0 || total_out < 0      -> error "SomebodyFCKDHardAroundBrotli"
                           | avail_in > 0                       -> do
                                                                       new_str <- B.packCStringLen (castPtr pnext_out,  fromIntegral total_out)
                                                                       rest_of_list <- goIter op ep csize_avail_in ppnext_in
                                                                       return (new_str:rest_of_list)
                           | avail_in == 0                      -> do
                                                                       new_str <- B.packCStringLen (castPtr pnext_out,  fromIntegral total_out)
                                                                       return [new_str]
                      else
                        error "Brotli got stuck"
