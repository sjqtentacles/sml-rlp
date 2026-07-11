# sml-rlp

Recursive Length Prefix (RLP) encoder/decoder for Ethereum in pure Standard ML

## Installation

```
smlpkg add github.com/sjqtentacles/sml-rlp
smlpkg sync
```

## Usage

```sml
open Rlp

(* Encode byte strings and nested lists *)
val encStr  = encode (Bytes "dog")
val encList = encode (List [Bytes "cat", Bytes "dog"])
val encNested = encode (List [List [Bytes "\000"], List [], List []])

(* Decode RLP bytes back to a value *)
val decoded = decode encStr
(* => Bytes "dog" *)

(* Total variant: NONE on malformed input instead of raising *)
val ok  = decodeOpt encStr   (* SOME (Bytes "dog") *)
val bad = decodeOpt ""       (* NONE *)

val (List [Bytes cat, Bytes dog]) = decode encList

(* Helper: encode a non-negative integer as minimal big-endian bytes *)
val encInt = encode (Bytes (bigIntToBytes (IntInf.fromInt 1000)))
```

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
encodes and decodes RLP values — a short string, the canonical
`["cat","dog"]` list example, and a round trip through `encodeBigInt`/
`decodeBigInt` and `decodeOpt` (output is byte-identical under MLton and
Poly/ML):

```
Encode a single short string "cat":
  83636174

Encode the canonical list ["cat","dog"]:
  c88363617483646f67  (9 bytes)

Decode it back:
  List ["cat", "dog"]

Big-integer round trip via encodeBigInt/decodeBigInt:
  0 -> 0x80 -> 0
  1024 -> 0x820400 -> 1024
  65535 -> 0x82ffff -> 65535

decodeOpt on malformed input:
  decodeOpt "" = NONE
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
