(**
 * IEEEReal structure.
 * @author YAMATODANI Kiyoshi
 * @version $Id: IEEEReal.sml,v 1.5 2005/09/12 15:37:35 kiyoshiy Exp $
 *)
structure IEEEReal :> IEEE_REAL =
struct

  (***************************************************************************)

  datatype real_order = LESS | EQUAL | GREATER | UNORDERED

  datatype nan_mode = QUIET | SIGNALLING

  datatype float_class =
           NAN of nan_mode
         | INF
         | ZERO
         | NORMAL
         | SUBNORMAL

  datatype rounding_mode =
           TO_NEAREST
         | TO_NEGINF
         | TO_POSINF
         | TO_ZERO

  type decimal_approx =
       {
         kind : float_class,
         sign : bool,
         digits : int list,
         exp : int
       }

  (***************************************************************************)

  exception Unordered

  (***************************************************************************)

  local
    val gRoundingMode = ref TO_NEAREST
  in
  fun setRoundingMode roundingMode = gRoundingMode := roundingMode
  fun getRoundingMode () = ! gRoundingMode
  end

  local
    fun intsToString [] = "0"
      | intsToString digits =
        implode (map (fn digit => Char.chr(Char.ord #"0" + digit)) digits)
  in
  fun toString ({kind, sign, digits, exp} : decimal_approx) =
      let
        val string = 
            case kind of
              ZERO => "0.0"
            | NORMAL => "0." ^ intsToString digits
            | SUBNORMAL => "0." ^ intsToString digits
            | INF => "inf"
            | NAN _ => "nan"
        val string = if sign then "~" ^ string else string
        val string =
            if exp <> 0 andalso ((kind = NORMAL) orelse (kind = SUBNORMAL))
            then string ^ "E" ^ (Int.toString exp)
            else string
      in
        string
      end
  end

  local
    structure SC = StringCvt
    structure PC = ParserComb

    fun charToNumber char =
        case char of
          #"0" => 0
        | #"1" => 1
        | #"2" => 2
        | #"3" => 3
        | #"4" => 4
        | #"5" => 5
        | #"6" => 6
        | #"7" => 7
        | #"8" => 8
        | #"9" => 9
        | _ => raise Fail "unexpected char in IEEEReal.charToNumbr."

    val isNumberChar = Char.isDigit

    (* (+|-|~)? *)
    fun scanSign reader stream = 
        PC.or(PC.seqWith #2 (PC.char #"+", PC.result false),
              PC.or (PC.seqWith #2 (PC.char #"~", PC.result true),
                     PC.or(PC.seqWith #2 (PC.char #"-", PC.result true),
                           PC.result false)))
             reader
             stream

    (* scan [0-9] *)
    fun scanNumChar reader stream =
        PC.wrap(PC.eatChar isNumberChar, charToNumber) reader stream

    (* scan: [0-9]+.[0-9]+? *)
    fun scanFirstForm reader stream =
        (PC.seq
             (
               PC.seqWith #1 (PC.oneOrMore scanNumChar, PC.char #"."),
               PC.zeroOrMore scanNumChar
             ))
            reader stream

    (* scan: .[0-9]+ *)
    fun scanSecondForm reader stream =
        (PC.wrap
             (
               PC.seqWith #2 (PC.char #".", PC.oneOrMore scanNumChar),
               fn fractionals => ([], fractionals)
             ))
            reader stream

    (* scan: ([0-9]+.[0-9]+? | .[0-9]+) *)
    fun scanDigits reader steram =
        PC.or (scanFirstForm, scanSecondForm) reader steram

    (* scan: (e|E)[+~-]?[0-9]+? *)
    fun scanExp reader stream =
        (PC.seqWith
             #2
             (
               PC.eatChar (fn ch => ch = #"e" orelse ch = #"E"),
               PC.seq (scanSign, PC.zeroOrMore scanNumChar)
             ))
             reader stream

    (* int list into an integer *)
    fun accumIntList ints =
        foldl (fn (int, accum) => accum * 10 + int) 0 ints

    (* [0, 0, 1, 2, 0] => ([0, 0], [1, 2, 0]) *)
    fun partitionPrefixZeros digits =
        let
          fun scan [] prefix = (prefix, [])
            | scan (0 :: tail) prefix = scan tail (0 :: prefix)
            | scan digits prefix = (prefix, digits)
        in scan digits []
        end

    (* [0, 0, 1, 2, 0] => ([0, 0, 1, 2], [0]) *)
    fun partitionSuffixZeros digits =
        case partitionPrefixZeros (List.rev digits) of
          (suffixZeros, reversedPrefix) =>
          (List.rev reversedPrefix, suffixZeros)
  in
  fun 'a scan (reader : (char, 'a) SC.reader) stream =
      let
        fun buildDecimal
                (sign, ((integers, fractionals), (expSign, expDigits))) =
            let
              val (_, integers) = partitionPrefixZeros integers
              val (fractionals, _) = partitionSuffixZeros fractionals
              val scannedExp =
                  (if expSign then ~1 else 1) * (accumIntList expDigits)

              val (kind, digits, exp) =
                  case (integers, fractionals) of
                    ([], []) => (ZERO, [], 0)
                  | ([], _) =>
                    let
                      val kind = NORMAL
                      val (prefixZeros, digits) =
                          partitionPrefixZeros fractionals
                      val exp = scannedExp - (List.length prefixZeros)
                    in (kind, digits, exp)
                    end
                  | (_ :: _, _) =>
                    let
                      val kind = NORMAL
                      val digits = integers @ fractionals
                      val exp = scannedExp + (List.length integers)
                    in (kind, digits, exp)
                    end

            in
              {kind = kind, sign = sign, digits = digits, exp = exp}
              : decimal_approx
            end
        fun scanNormal reader stream =
            PC.wrap
              (
                PC.seq
                    (
                      scanSign,
                      PC.seq(scanDigits, PC.or(scanExp, PC.result (true, [0])))
                    ),
                buildDecimal
              )
              reader
              stream
        fun scanUnnormal reader stream =
            PC.wrap
                (
                  PC.seq
                      (
                        scanSign, 
                        PC.or'
                            [
                              PC.wrap (PC.string "inf", fn _ => INF),
                              PC.wrap (PC.string "infinity", fn _ => INF),
                              PC.wrap (PC.string "nan", fn _ => NAN QUIET)
                            ]
                      ),
                  fn (sign, kind) => 
                     {kind = kind, sign = sign, digits = [], exp = 0}
                     : decimal_approx
                )
                reader
                stream
      in
        PC.or
            (scanNormal, scanUnnormal)
            reader
            (StringCvt.skipWS reader stream)
      end
  fun fromString string = (SC.scanString scan) string
  end

  (***************************************************************************)

end