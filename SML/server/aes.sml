signature AES =
sig
  type t
  structure Mode:
  sig
    type t
    val CFB: t
  end
  type vector = Word8Vector.vector
  type array = Word8Array.array
  (* val aes: Word8VectorSlice.slice * Word8VectorSlice.slice * Mode.t -> t *)
  val encrypt: vector * array * vector * Mode.t -> vector
  (* val decrypt: t * Word8Vector.vector -> Word8Vector.vector *)
end

structure Aes :> AES =
struct
  datatype t = T of {
    iv: Word8ArraySlice.slice
  }
  type vector = Word8Vector.vector
  type array = Word8Array.array
  structure Array = Word8Array
  structure Vector = Word8Vector
  structure ArraySlice = Word8ArraySlice

  type EVP_CTX_t = MLton.Pointer.t
  type EVP_CIPHER_t = MLton.Pointer.t
  type EVP_ENGINE_t = MLton.Pointer.t
  val NULL = MLton.Pointer.null

  val EVP_CIPHER_CTX_new = _import "EVP_CIPHER_CTX_new" public:
        unit -> EVP_CTX_t;
  val EVP_CIPHER_CTX_free = _import "EVP_CIPHER_CTX_free" public:
        EVP_CTX_t -> unit;
  val EVP_EncryptInit_ex = _import "EVP_EncryptInit_ex" public:
        EVP_CTX_t * EVP_CIPHER_t * EVP_ENGINE_t * vector * array -> int;
  val EVP_EncryptUpdate = _import "EVP_EncryptUpdate" public:
        EVP_CTX_t * array * int ref * vector * int -> int;
  val EVP_EncryptFinal_ex = _import "EVP_EncryptFinal_ex" public:
        EVP_CTX_t * array * int ref -> int;
  structure Mode =
  struct
    type t = EVP_CIPHER_t
    val CFB = let val f = _import  "EVP_aes_256_cfb1" public: unit -> EVP_CIPHER_t;
              in f ()
              end
  end
  fun encrypt (key, iv, plain, mode) =
    let val cipher = Word8Array.array (Word8Vector.length plain, Word8.fromInt 0)
        val final = Word8Array.array (Word8Vector.length plain, Word8.fromInt 0)
        val len = ref 0
        val lenf = ref 0

        val ctx = EVP_CIPHER_CTX_new ()
        val ret = EVP_EncryptInit_ex (ctx, Mode.CFB, NULL, key, iv)
        val _ = if ret <> 1 then raise Fail "EVP_EncryptInit_ex" else ()
        val ret = EVP_EncryptUpdate (ctx, cipher, len, plain, Word8Vector.length plain)
        val _ = if ret <> 1 then raise Fail "EVP_EncryptUpdate" else ()
        val ret = EVP_EncryptFinal_ex (ctx, final, lenf)
        val _ = if ret <> 1 then raise Fail "EVP_EncryptFinal_ex" else ()
        val _ = EVP_CIPHER_CTX_free (ctx)
        
        val total = !len + !lenf
        val _ = print ("Total " ^ (Int.toString total) ^ " bytes\n")
        fun makeVector (a1,len1) (a2,len2) =
          let val toSlice = ArraySlice.vector o ArraySlice.slice
              val v1 = toSlice (a1, 0, SOME(len1))
              val v2 = toSlice (a2, 0, SOME(len2))
          in  Vector.concat [v1, v2]
          end
    in
      makeVector (cipher, !len) (final, !lenf)
    end
end

val string = "The quick brown fox jumps over the lazy dog"
val key = "01234567890123456789012345678901"
val iv = "0123456789012345"
val iv_a = Word8Array.array (String.size iv, Word8.fromInt 0)
val _ = Word8Array.copyVec {src = Byte.stringToBytes iv, dst = iv_a, di = 0}
val _ = print ("Original iv: " ^ ((Byte.unpackString o Word8ArraySlice.full) iv_a) ^ "\n")
val cipher = Aes.encrypt (Byte.stringToBytes key, iv_a, Byte.stringToBytes string, Aes.Mode.CFB)
val _ = print ("Original size " ^ (Int.toString (String.size string)) ^ "\n")
val _ = print ("Output iv: " ^ ((Byte.unpackString o Word8ArraySlice.full) iv_a) ^ "\n")
val _ = print ("Cipher size " ^ ((Int.toString o Word8Vector.length) cipher) ^ "\n")
val _ = print ("Cipher : " ^ (String.toCString (Byte.bytesToString cipher)) ^ "\n")
