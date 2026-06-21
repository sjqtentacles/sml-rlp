(* test.sml — RLP test vectors from Ethereum EIP tests *)

structure Tests =
struct

  open Rlp

  (* Helper: convert a string to a hex dump for readable error messages *)
  fun hexOf (s : string) : string =
    let
      fun byteHex c =
        let
          val n  = Char.ord c
          val hi = n div 16
          val lo = n mod 16
          fun hex d = if d < 10 then Char.chr (d + 48) else Char.chr (d + 87)
        in
          String.implode [hex hi, hex lo]
        end
    in
      String.concat (List.map byteHex (String.explode s))
    end

  (* ------------------------------------------------------------------ *)
  (* Byte string helpers using Char.chr (no \x escapes)                  *)
  (* ------------------------------------------------------------------ *)

  (* 0x80 = 128 *)
  val byte80 = String.str (Char.chr 128)
  (* 0x81 = 129 *)
  val byte81 = String.str (Char.chr 129)
  (* 0x83 = 131 *)
  val byte83 = String.str (Char.chr 131)
  (* 0xb8 = 184 *)
  val byteB8 = String.str (Char.chr 184)
  (* 0xc0 = 192 *)
  val byteC0 = String.str (Char.chr 192)
  (* 0xc8 = 200 *)
  val byteC8 = String.str (Char.chr 200)
  (* 0xc6 = 198 *)
  val byteC6 = String.str (Char.chr 198)
  (* 0xc7 = 199 *)
  val byteC7 = String.str (Char.chr 199)

  (* 56 characters — triggers the long-string path (56+ bytes) *)
  val loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit!"

  fun run () =
    let
      val _ = Harness.reset ()
    in

      (* ---- Section 1: Bytes encoding ---- *)
      Harness.section "Bytes encoding";

      (* 1. Empty string -> 0x80 *)
      Harness.checkString "empty string encodes to 0x80"
        (byte80, encode (Bytes ""));

      (* 2. Single byte 0x00 -> 0x00 *)
      Harness.checkString "single byte 0x00"
        (String.str (Char.chr 0), encode (Bytes (String.str (Char.chr 0))));

      (* 3. Single byte 0x01 -> 0x01 *)
      Harness.checkString "single byte 0x01"
        (String.str (Char.chr 1), encode (Bytes (String.str (Char.chr 1))));

      (* 4. Single byte 0x7f -> 0x7f *)
      Harness.checkString "single byte 0x7f"
        (String.str (Char.chr 127), encode (Bytes (String.str (Char.chr 127))));

      (* 5. Single byte 0x80 -> 0x81 0x80 (two bytes) *)
      Harness.checkString "single byte 0x80 encodes with length prefix"
        (byte81 ^ String.str (Char.chr 128),
         encode (Bytes (String.str (Char.chr 128))));

      (* 6. Short string "dog" -> 0x83 "dog" *)
      Harness.checkString "short string dog"
        (byte83 ^ "dog", encode (Bytes "dog"));

      (* 7. Long string (56 chars) -> 0xb8 0x38 <string> *)
      (* 0x38 = 56 decimal *)
      Harness.checkString "long string 56 chars"
        (byteB8 ^ String.str (Char.chr 56) ^ loremIpsum,
         encode (Bytes loremIpsum));

      (* 8. String "cat" *)
      Harness.checkString "short string cat"
        (String.str (Char.chr 131) ^ "cat", encode (Bytes "cat"));

      (* ---- Section 2: List encoding ---- *)
      Harness.section "List encoding";

      (* 9. Empty list -> 0xc0 *)
      Harness.checkString "empty list encodes to 0xc0"
        (byteC0, encode (List []));

      (* 10. List ["cat","dog"] -> 0xc8 0x83 "cat" 0x83 "dog" *)
      Harness.checkString "list [cat, dog]"
        (byteC8 ^ String.str (Char.chr 131) ^ "cat" ^
                  String.str (Char.chr 131) ^ "dog",
         encode (List [Bytes "cat", Bytes "dog"]));

      (* 11. Nested list [ [], [[]], [[], [[]]] ]
            [ ]           -> c0
            [ [] ]        -> c1 c0
            [ [], [[]] ]  -> c3 c0 c1 c0
            outer: c0 + c1 c0 + c3 c0 c1 c0
                 = 1 + 2 + 4 = 7 bytes payload -> c7 ... *)
      Harness.checkString "nested list [[], [[]], [[], [[]]]]"
        (String.str (Char.chr 199) ^
         String.str (Char.chr 192) ^
         String.str (Char.chr 193) ^ String.str (Char.chr 192) ^
         String.str (Char.chr 195) ^ String.str (Char.chr 192) ^
           String.str (Char.chr 193) ^ String.str (Char.chr 192),
         encode (List [ List []
                      , List [List []]
                      , List [List [], List [List []]]
                      ]));

      (* ---- Section 3: BigInt encoding ---- *)
      Harness.section "BigInt encoding";

      (* 12. Integer 0 -> 0x80 (empty bytes) *)
      Harness.checkString "encodeBigInt 0"
        (byte80, encodeBigInt (IntInf.fromInt 0));

      (* 13. Integer 1 -> 0x01 *)
      Harness.checkString "encodeBigInt 1"
        (String.str (Char.chr 1), encodeBigInt (IntInf.fromInt 1));

      (* 14. Integer 127 -> 0x7f *)
      Harness.checkString "encodeBigInt 127"
        (String.str (Char.chr 127), encodeBigInt (IntInf.fromInt 127));

      (* 15. Integer 128 -> 0x81 0x80 *)
      Harness.checkString "encodeBigInt 128"
        (byte81 ^ String.str (Char.chr 128),
         encodeBigInt (IntInf.fromInt 128));

      (* 16. Integer 1024 = 0x0400 -> 0x82 0x04 0x00 *)
      Harness.checkString "encodeBigInt 1024"
        (String.str (Char.chr 130) ^ String.str (Char.chr 4) ^ String.str (Char.chr 0),
         encodeBigInt (IntInf.fromInt 1024));

      (* ---- Section 4: Decode ---- *)
      Harness.section "Decode";

      (* 17. Decode empty string encoding *)
      Harness.check "decode empty bytes"
        (decode byte80 = Bytes "");

      (* 18. Decode "dog" encoding *)
      Harness.check "decode dog"
        (decode (byte83 ^ "dog") = Bytes "dog");

      (* 19. Decode empty list *)
      Harness.check "decode empty list"
        (decode byteC0 = List []);

      (* 20. Decode [cat, dog] *)
      Harness.check "decode [cat, dog]"
        (decode (byteC8 ^ String.str (Char.chr 131) ^ "cat" ^
                           String.str (Char.chr 131) ^ "dog")
         = List [Bytes "cat", Bytes "dog"]);

      (* ---- Section 5: Roundtrip ---- *)
      Harness.section "Roundtrip encode/decode";

      (* 21. Roundtrip empty bytes *)
      Harness.check "roundtrip Bytes empty"
        (decode (encode (Bytes "")) = Bytes "");

      (* 22. Roundtrip dog *)
      Harness.check "roundtrip Bytes dog"
        (decode (encode (Bytes "dog")) = Bytes "dog");

      (* 23. Roundtrip long string *)
      Harness.check "roundtrip Bytes loremIpsum"
        (decode (encode (Bytes loremIpsum)) = Bytes loremIpsum);

      (* 24. Roundtrip nested list *)
      let
        val nested = List [ List []
                          , List [List []]
                          , List [List [], List [List []]]
                          ]
      in
        Harness.check "roundtrip nested list"
          (decode (encode nested) = nested)
      end;

      (* 25. BigInt roundtrip 0 *)
      Harness.check "decodeBigInt (encodeBigInt 0) = 0"
        (decodeBigInt (encodeBigInt (IntInf.fromInt 0)) = IntInf.fromInt 0);

      (* 26. BigInt roundtrip 1024 *)
      Harness.check "decodeBigInt (encodeBigInt 1024) = 1024"
        (decodeBigInt (encodeBigInt (IntInf.fromInt 1024)) = IntInf.fromInt 1024);

      (* 27. BigInt roundtrip large number *)
      let
        val big = IntInf.fromInt 1000000000
      in
        Harness.check "decodeBigInt roundtrip 1000000000"
          (decodeBigInt (encodeBigInt big) = big)
      end;

      (* ---- Section 6: Error handling ---- *)
      Harness.section "Error handling";

      (* 28. Empty input raises *)
      Harness.checkRaises "decode empty string raises"
        (fn () => decode "");

      (* 29. Truncated input raises *)
      Harness.checkRaises "decode truncated bytes raises"
        (fn () => decode (byte83 ^ "do"));

      (* 30. Trailing bytes raises *)
      Harness.checkRaises "decode with trailing byte raises"
        (fn () => decode (byte80 ^ String.str (Char.chr 0)));

      (* 31. encodeBigInt negative raises *)
      Harness.checkRaises "encodeBigInt negative raises"
        (fn () => encodeBigInt (IntInf.fromInt ~1));

      Harness.run ()
    end

end
