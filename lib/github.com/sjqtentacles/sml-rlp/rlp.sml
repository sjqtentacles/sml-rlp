structure Rlp :> RLP =
struct

  datatype value = Bytes of string | List of value list

  (* ------------------------------------------------------------------ *)
  (* Helpers                                                              *)
  (* ------------------------------------------------------------------ *)

  (* Convert a non-negative IntInf to a minimal big-endian byte string.
     Returns "" for zero. *)
  fun bigIntToBytes (n : IntInf.int) : string =
    if n = IntInf.fromInt 0
    then ""
    else
      let
        fun collect acc m =
          if m = IntInf.fromInt 0
          then acc
          else
            let
              val byteVal = IntInf.toInt (IntInf.mod (m, IntInf.fromInt 256))
              val rest    = IntInf.div (m, IntInf.fromInt 256)
            in
              collect (Char.chr byteVal :: acc) rest
            end
      in
        String.implode (collect [] n)
      end

  (* Convert a minimal big-endian byte string to a non-negative IntInf. *)
  fun bytesToBigInt (s : string) : IntInf.int =
    let
      val chars = String.explode s
      fun build acc ch =
        IntInf.+ (
          IntInf.* (acc, IntInf.fromInt 256),
          IntInf.fromInt (Char.ord ch)
        )
    in
      List.foldl (fn (ch, acc) => build acc ch) (IntInf.fromInt 0) chars
    end

  (* Encode a length as a minimal big-endian byte string. *)
  fun lenToBytes (n : int) : string =
    bigIntToBytes (IntInf.fromInt n)

  (* ------------------------------------------------------------------ *)
  (* Encode                                                               *)
  (* ------------------------------------------------------------------ *)

  fun encodeBytes (s : string) : string =
    let
      val len = String.size s
    in
      if len = 1 andalso Char.ord (String.sub (s, 0)) <= 0x7f
      then s
      else if len <= 55
      then String.str (Char.chr (0x80 + len)) ^ s
      else
        let
          val lb     = lenToBytes len
          val lbLen  = String.size lb
        in
          String.str (Char.chr (0xb7 + lbLen)) ^ lb ^ s
        end
    end

  fun encodeList (items : value list) : string =
    let
      val payload = String.concat (List.map encode items)
      val len     = String.size payload
    in
      if len <= 55
      then String.str (Char.chr (0xc0 + len)) ^ payload
      else
        let
          val lb    = lenToBytes len
          val lbLen = String.size lb
        in
          String.str (Char.chr (0xf7 + lbLen)) ^ lb ^ payload
        end
    end

  and encode (v : value) : string =
    case v of
      Bytes s    => encodeBytes s
    | List items => encodeList items

  (* ------------------------------------------------------------------ *)
  (* Decode                                                               *)
  (* ------------------------------------------------------------------ *)

  (* Read n bytes starting at position pos from string s. *)
  fun substr (s : string) (pos : int) (n : int) : string =
    if pos + n > String.size s
    then raise Fail "RLP decode: unexpected end of input"
    else String.substring (s, pos, n)

  (* Returns (value, next_position) *)
  fun decodeAt (s : string) (pos : int) : value * int =
    let
      val _ = if pos >= String.size s
              then raise Fail "RLP decode: unexpected end of input"
              else ()
      val firstByte = Char.ord (String.sub (s, pos))
    in
      if firstByte <= 0x7f then
        (* Single byte *)
        (Bytes (String.str (String.sub (s, pos))), pos + 1)
      else if firstByte <= 0xb7 then
        (* Short string: length = firstByte - 0x80 *)
        let
          val len  = firstByte - 0x80
          val data = substr s (pos + 1) len
        in
          (Bytes data, pos + 1 + len)
        end
      else if firstByte <= 0xbf then
        (* Long string: length-of-length = firstByte - 0xb7 *)
        let
          val lenLen  = firstByte - 0xb7
          val lenBytes = substr s (pos + 1) lenLen
          val len     = IntInf.toInt (bytesToBigInt lenBytes)
          val data    = substr s (pos + 1 + lenLen) len
        in
          (Bytes data, pos + 1 + lenLen + len)
        end
      else if firstByte <= 0xf7 then
        (* Short list: payload length = firstByte - 0xc0 *)
        let
          val payLen = firstByte - 0xc0
          val endPos = pos + 1 + payLen
          fun readItems cur acc =
            if cur >= endPos then List.rev acc
            else
              let
                val (item, next) = decodeAt s cur
              in
                readItems next (item :: acc)
              end
          val items = readItems (pos + 1) []
        in
          (List items, endPos)
        end
      else
        (* Long list: length-of-length = firstByte - 0xf7 *)
        let
          val lenLen   = firstByte - 0xf7
          val lenBytes  = substr s (pos + 1) lenLen
          val payLen   = IntInf.toInt (bytesToBigInt lenBytes)
          val startPos = pos + 1 + lenLen
          val endPos   = startPos + payLen
          fun readItems cur acc =
            if cur >= endPos then List.rev acc
            else
              let
                val (item, next) = decodeAt s cur
              in
                readItems next (item :: acc)
              end
          val items = readItems startPos []
        in
          (List items, endPos)
        end
    end

  fun decode (s : string) : value =
    let
      val (v, consumed) = decodeAt s 0
    in
      if consumed <> String.size s
      then raise Fail "RLP decode: trailing bytes"
      else v
    end

  (* ------------------------------------------------------------------ *)
  (* BigInt encode/decode                                                 *)
  (* ------------------------------------------------------------------ *)

  fun encodeBigInt (n : IntInf.int) : string =
    if n < IntInf.fromInt 0
    then raise Fail "RLP encodeBigInt: negative integer"
    else encode (Bytes (bigIntToBytes n))

  fun decodeBigInt (s : string) : IntInf.int =
    case decode s of
      Bytes b => bytesToBigInt b
    | List _  => raise Fail "RLP decodeBigInt: expected bytes, got list"

end
