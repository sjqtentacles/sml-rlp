(* demo.sml - RLP (Recursive Length Prefix), Ethereum's serialization
   format: encode/decode bytes, lists, and big integers.
   Deterministic: identical output on every run and both compilers. *)

structure R = Rlp

fun toHex s =
  let
    val digits = "0123456789abcdef"
    fun byteHex c =
      let val n = Char.ord c
      in String.str (String.sub (digits, n div 16))
         ^ String.str (String.sub (digits, n mod 16))
      end
  in String.concat (List.map byteHex (String.explode s)) end

val () = print "Encode a single short string \"cat\":\n"
val () = print ("  " ^ toHex (R.encode (R.Bytes "cat")) ^ "\n")

val () = print "\nEncode the canonical list [\"cat\",\"dog\"]:\n"
val listVal = R.List [R.Bytes "cat", R.Bytes "dog"]
val encoded = R.encode listVal
val () = print ("  " ^ toHex encoded ^ "  (" ^ Int.toString (String.size encoded) ^ " bytes)\n")

val () = print "\nDecode it back:\n"
val () =
  case R.decode encoded of
      R.List [R.Bytes a, R.Bytes b] => print ("  List [\"" ^ a ^ "\", \"" ^ b ^ "\"]\n")
    | _ => print "  (unexpected shape)\n"

val () = print "\nBig-integer round trip via encodeBigInt/decodeBigInt:\n"
val () =
  List.app
    (fn n =>
      let val enc = R.encodeBigInt n
      in print ("  " ^ IntInf.toString n ^ " -> 0x" ^ toHex enc
               ^ " -> " ^ IntInf.toString (R.decodeBigInt enc) ^ "\n")
      end)
    [0, 1024, 65535]

val () = print "\ndecodeOpt on malformed input:\n"
val () = print ("  decodeOpt \"\" = "
                ^ (case R.decodeOpt "" of NONE => "NONE" | SOME _ => "SOME ...") ^ "\n")
