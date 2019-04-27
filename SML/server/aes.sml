signature AES =
sig
  type t
  structure Mode:
  sig
    type t
    val CFB: t
  end
  val aes: Word8VectorSlice.slice * Word8VectorSlice.slice * Mode.t -> t
  val encrypt: t * Word8VectorSlice.slice -> Word8VectorSlice.slice
  val decrypt: t * Word8VectorSlice.slice -> Word8VectorSlice.slice
end

structure Aes :> AES =
struct
  datatype t = T of {
    iv: Word8ArraySlice.slice
  }
  val EVP_CTX_t = MLton.Pointer.t

  val EVP_CIPHER_CTX_new = _import "EVP_CIPHER_CTX_new" public:
        unit -> Pointer.t
  val EVP_CIPHER_CTX_free = _import "EVP_CIPHER_CTX_free" public:
        Pointer.t -> unit
  val EVP_EncryptInit_ex = _import "EVP_EncryptInit_ex" public:
        Pointer.t
  fun encrypt key iv mode:
    let
end
