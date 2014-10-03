-module(iban).
-export([validate/1]).

validate(Iban) -> I = spaceParser(Iban), ibanParser(I).

spaceParser(Iban) -> 
      X = string:str(Iban, " "),
      if 
        X /= 0 -> Y = string:substr(Iban, 1, X -1),
            Z = string:substr(Iban, X + 1),
            spaceParser(Y++Z);
        true -> Iban
      end.

ibanParser(Iban) ->  
      if Iban == [] -> false;
        true ->
          X = {string:substr(Iban, 1, 2)}, 
          Y = {string:substr(Iban, 3, 2)}, 
          Z = {string:substr(Iban, 5)}, 
          newIban(X, Y, Z)
      end.

newIban({X}, {Y}, {Z}) -> 
      NumX = letterToNumber(X, []),
      NumZ = letterToNumber(Z, []),
      LastNum = NumZ ++ NumX ++ Y,
      {{IntNum, _}} = {string:to_integer(LastNum)},
      D = IntNum rem 97,
      if 
        D == 1 -> true;
        true -> false
      end.

letterToNumber(Letter, Res) ->
      if Letter == []
        -> Res;
        true ->
          H = hd(Letter),
          if H > 58 -> 
            Num = integer_to_list( H - 55);
            true -> Num = integer_to_list(H - 48)
          end,
          R = Res ++ Num,
          letterToNumber(tl(Letter), R)
      end.


-include_lib("eunit/include/eunit.hrl").

validate_test_() -> [
  ?_assertEqual(true,validate("BA39 1290 0794 0102 8494")),
  ?_assertEqual(true,validate("BA39         1290 0794 0102 8494")),
  ?_assertEqual(true,validate("BA391290079401028494")),
  ?_assertEqual(true,validate("GB29 NWBK 6016 1331 9268 19")),
  ?_assertEqual(true,validate("GE29 NB00 0000 0101 9049 17")),
  ?_assertEqual(true,validate("GI75 NWBK 0000 0000 7099 453")),
  ?_assertEqual(true,validate("GL89 6471 0001 0002 06")),
  ?_assertEqual(true,validate("GR16 0110 1250 0000 0001 2300 695")),
  ?_assertEqual(true,validate("HR12 1001 0051 8630 0016 0")),
  ?_assertEqual(true,validate("HU42 1177 3016 1111 1018 0000 0000")),
  ?_assertEqual(true,validate("IE29 AIBK 9311 5212 3456 78")),
  ?_assertEqual(true,validate("IL62 0108 0000 0009 9999 999")),
  ?_assertEqual(true,validate(" IS14 0159 2600 7654 5510 7303 39")),
  ?_assertEqual(true,validate("IT60 X054 2811 1010 0000 0123 456")),
  ?_assertEqual(true,validate("KW81 CBKU 0000 0000 0000 1234 5601 01")),
  ?_assertEqual(true,validate("KZ86 125K ZT50 0410 0100")),
  ?_assertEqual(true,validate("LB62 0999 0000 0001 0019 0122 9114")),
  ?_assertEqual(true,validate("LI21 0881 0000 2324 013A A")),
  ?_assertEqual(true,validate("LT12 1000 0111 0100 1000")),
  ?_assertEqual(true,validate("LV80 BANK 0000 4351 9500 1")),
  ?_assertEqual(true,validate("MC11 1273 9000 7000 1111 1000 H79")),
  ?_assertEqual(true,validate("ME25 5050 0001 2345 6789 51")),
  ?_assertEqual(true,validate("MT84 MALT 0110 0001 2345 MTLC AST0 01S")),
  ?_assertEqual(true,validate("MU17 BOMM 0101 1010 3030 0200 000M UR")),
  ?_assertEqual(true,validate("NL91 ABNA 0417 1643 00")),
  ?_assertEqual(true,validate("NO93 8601 1117 947")),
  ?_assertEqual(true,validate("PT50 0002 0123 1234 5678 9015 4")),
  ?_assertEqual(true,validate("RO49 AAAA 1B31 0075 9384 0000")),
  ?_assertEqual(true,validate("RS35 2600 0560 1001 6113 79")),
  ?_assertEqual(true,validate("SA03 8000 0000 6080 1016 7519")),
  ?_assertEqual(true,validate("SM86 U032 2509 8000 0000 0270 100")),
  ?_assertEqual(true,validate("TR33 0006 1005 1978 6457 8413 26")),
  ?_assertEqual(true,validate("BG80 BNBG 9661 1020 3456 78")),
  ?_assertEqual(true,validate(" FO62 6460 0001 6316 34")),
  ?_assertEqual(true,validate("DK50 0040 0440 1162 43")),
  ?_assertEqual(true,validate("CH93 0076 2011 6238 5295 7")),
  ?_assertEqual(true,validate("DE89 3704 0044 0532 0130 00")),
  ?_assertEqual(true,validate("AD12 0001 2030 2003 5910 0100")),
  ?_assertEqual(true,validate(" AL47 2121 1009 0000 0002 3569 8741")),
  ?_assertEqual(true,validate("AT61 1904 3002 3457 3201")),
  
  ?_assertEqual(false,validate("")),
  ?_assertEqual(false,validate("AT61 1904 4002 3457 3201")),
  ?_assertEqual(false,validate("CH93 0076 2012 6238 5295 7")),
  ?_assertEqual(false,validate("NL91 ABAB 0417 1634 00")),
  ?_assertEqual(false,validate("ME25 5050 0021 2345 6789 51")),
  ?_assertEqual(false,validate("LV80 BANK KK00 4351 9500 1")),
  ?_assertEqual(false,validate("KW81 CBKU A000 0000 0000 1234 5601 01")),
  ?_assertEqual(false,validate("KZ86 125K ZT13 0410 0100")),
  ?_assertEqual(false,validate("IS14 0159 2600 7654 6120 7303 49")),
  ?_assertEqual(false,validate("GR16 0110 12AA 0000 0001 2300 695")),
  ?_assertEqual(false,validate("NL91 ABNA BC17 1643 00")),
  ?_assertEqual(false,validate("BA39 1290 0794 0102 8493")),
  ?_assertEqual(false,validate("BA39 1290 0794 0102 849"))
].
