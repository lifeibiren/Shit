signature EVP =
sig
  structure Aes:
  sig
    structure Mode:
    sig
      type t
      val cfb: t
      val ecb: t
    end
    structure Engine:
    sig
      type t
      val default: t
    end
    type t
    val new: unit -> t
    val free: t -> unit
    val encryptInit: t * Mode.t * Engine.t * Word8Vector.vector * Word8Vector.vector -> unit
    val encryptUpdate: t * Word8Vector.vector -> Word8Vector.vector
    val encryptFinal: t -> Word8Vector.vector
    val decryptInit: t * Mode.t * Engine.t * Word8Vector.vector * Word8Vector.vector -> unit
    val decryptUpdate: t * Word8Vector.vector -> Word8Vector.vector
    val decryptFinal: t -> Word8Vector.vector
  end
end

structure Evp :> EVP =
struct
  structure Aes =
  struct
    type EVP_CTX_t = MLton.Pointer.t
    type EVP_CIPHER_t = MLton.Pointer.t
    type EVP_ENGINE_t = MLton.Pointer.t
    val NULL = MLton.Pointer.null

    structure Mode =
    struct
      type t = EVP_CIPHER_t
      val cfb = let val f = _import "EVP_aes_256_cfb1" public: unit -> EVP_CIPHER_t;
                in f ()
                end
      val ecb = let val f = _import "EVP_aes_256_ecb" public: unit -> EVP_CIPHER_t;
                in f ()
                end
    end
    structure Engine =
    struct
      type t = EVP_ENGINE_t
      val default = NULL
    end
    type t = EVP_CTX_t

    type vector = Word8Vector.vector
    type array = Word8Array.array
    structure Array = Word8Array
    structure Vector = Word8Vector
    structure ArraySlice = Word8ArraySlice

    val EVP_CIPHER_CTX_new = _import "EVP_CIPHER_CTX_new" public:
          unit -> EVP_CTX_t;
    val EVP_CIPHER_CTX_free = _import "EVP_CIPHER_CTX_free" public:
          EVP_CTX_t -> unit;
    val EVP_EncryptInit_ex = _import "EVP_EncryptInit_ex" public:
          EVP_CTX_t * EVP_CIPHER_t * EVP_ENGINE_t * vector * vector -> int;
    val EVP_EncryptUpdate = _import "EVP_EncryptUpdate" public:
          EVP_CTX_t * array * int ref * vector * int -> int;
    val EVP_EncryptFinal_ex = _import "EVP_EncryptFinal_ex" public:
          EVP_CTX_t * array * int ref -> int;
    val EVP_DecryptInit_ex = _import "EVP_DecryptInit_ex" public:
          EVP_CTX_t * EVP_CIPHER_t * EVP_ENGINE_t * vector * vector -> int;
    val EVP_DecryptUpdate = _import "EVP_DecryptUpdate" public:
          EVP_CTX_t * array * int ref * vector * int -> int;
    val EVP_DecryptFinal_ex = _import "EVP_DecryptFinal_ex" public:
          EVP_CTX_t * array * int ref -> int;

    val new = EVP_CIPHER_CTX_new
    val free = EVP_CIPHER_CTX_free
    fun encryptInit args = case EVP_EncryptInit_ex args of
                             1 => ()
                           | _ => raise Fail "EVP_EncryptInit_ex"
    fun decryptInit args = case EVP_DecryptInit_ex args of
                             1 => ()
                           | _ => raise Fail "EVP_DecryptInit_ex"

    local
      fun arrayToVector (arr, start, len) =
        (ArraySlice.vector o ArraySlice.slice) (arr, start, SOME(len))
    in
      fun encryptUpdate (ctx, plain) =
        let
          val cipher = Array.array (Vector.length plain, Word8.fromInt 0)
          val len = ref 0
          val _ = case EVP_EncryptUpdate (ctx, cipher, len, plain, Vector.length plain) of
                    1 => ()
                  | _ => raise Fail "EVP_EncryptUpdate"
        in
          arrayToVector (cipher, 0, !len)
        end

      fun encryptFinal ctx =
        let
          val cipher = Array.array (32, Word8.fromInt 0)
          val len = ref 0
          val _ = case EVP_EncryptFinal_ex (ctx, cipher, len) of
                    1 => ()
                  | _ => raise Fail "EVP_EncryptFinal"
        in
          arrayToVector (cipher, 0, !len)
        end

      fun decryptUpdate (ctx, cipher) =
        let
          val plain = Array.array (Vector.length cipher, Word8.fromInt 0)
          val len = ref 0
          val _ = case EVP_DecryptUpdate (ctx, plain, len, cipher, Vector.length cipher) of
                    1 => ()
                  | _ => raise Fail "EVP_EncryptUpdate"
        in
          arrayToVector (plain, 0, !len)
        end

      fun decryptFinal ctx =
        let
          val plain = Array.array (32, Word8.fromInt 0)
          val len = ref 0
          val _ = case EVP_DecryptFinal_ex (ctx, plain, len) of
                    1 => ()
                  | _ => raise Fail "EVP_EncryptFinal"
        in
          arrayToVector (plain, 0, !len)
        end
      end
  end
end

fun test () =
let
  val string = "The quick brown fox jumps over the lazy dog"
  val key = "01234567890123456789012345678901"
  val iv = "0123456789012345"
  val ectx = Evp.Aes.new ()
  val _ = Evp.Aes.encryptInit (ectx, Evp.Aes.Mode.cfb, Evp.Aes.Engine.default,
            Byte.stringToBytes key, Byte.stringToBytes iv)
  val dctx = Evp.Aes.new ()
  val _ = Evp.Aes.decryptInit (dctx, Evp.Aes.Mode.cfb, Evp.Aes.Engine.default,
            Byte.stringToBytes key, Byte.stringToBytes iv)
  val cipher = Evp.Aes.encryptUpdate (ectx, Byte.stringToBytes string)
  val _ = print ("Cipher : " ^ (String.toCString (Byte.bytesToString cipher)) ^ "\n")
  val plain = Evp.Aes.decryptUpdate (dctx, cipher)
  val _ = print ("Plain : " ^ (String.toCString (Byte.bytesToString plain)) ^ "\n")
in
  ()
end
(* val _ = print ("Original size " ^ (Int.toString (String.size string)) ^ "\n")
val _ = print ("Output iv: " ^ ((Byte.unpackString o Word8ArraySlice.full) iv_a) ^ "\n")
val _ = print ("Cipher size " ^ ((Int.toString o Word8Vector.length) cipher) ^ "\n")
val _ = print ("Cipher : " ^ (String.toCString (Byte.bytesToString cipher)) ^ "\n") *)
