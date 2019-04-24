signature AES =
sig
  type t
  structure Mode =
  struct
    type t
    val CFB: t
  end
  val aes: Word8VectorSlice.slice * Mode.t -> t
  val encrypt: t * Word8VectorSlice.slice -> Word8VectorSlice.slice
  val decrypt: t * Word8VectorSlice.slice -> Word8VectorSlice.slice
end
