signature CARRAY =
sig
  type t
  type word = Word64.word
  type byte = Word8.word
  val cArray: word -> t
  val sub: t * word -> byte
  val length: t -> word
  val update: t * word * byte -> unit
end

structure CArray :> CARRAY =
struct
  open MLton
  type t = Pointer.t
  type word = Word64.word
  type byte = Word8.word
  val CArray_new = _import "CArray_new" public: word -> Pointer.t;
  val CArray_sub = _import "CArray_sub" public: Pointer.t * word -> byte;
  val CArray_length = _import "CArray_length" public: Pointer.t -> word;
  val CArray_update = _import "CArray_update" public: Pointer.t * word * byte -> unit;
  val CArray_free = _import "CArray_free" public: Pointer.t -> unit;

  val cArray = cArray_new
  val sub = CArray_sub
  val length = CArray_length
  val update = CArray_update
end

val a = CArray.cArray 10
val _ = print (Word64.toString o CArray.length a)
