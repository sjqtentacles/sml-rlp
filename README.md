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

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
