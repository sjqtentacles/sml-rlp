signature RLP_VALUE =
sig
  datatype value = Bytes of string | List of value list
end

signature RLP =
sig
  include RLP_VALUE

  (* Encode an RLP value to a raw byte string. *)
  val encode : value -> string

  (* Decode a raw byte string to an RLP value.
     Raises Fail if the input is malformed. *)
  val decode : string -> value

  (* Total variant of [decode]: returns NONE instead of raising on malformed
     input (truncated, trailing bytes, bad length prefix, ...).
     decodeOpt (encode v) = SOME v, and decodeOpt s = SOME (decode s) whenever
     [decode s] succeeds. *)
  val decodeOpt : string -> value option

  (* Encode a non-negative big integer as RLP bytes (big-endian, no leading zeros).
     Integer 0 encodes as empty bytes (0x80). *)
  val encodeBigInt : IntInf.int -> string

  (* Decode RLP bytes as a big-endian unsigned integer. *)
  val decodeBigInt : string -> IntInf.int
end
