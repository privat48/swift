-module(iban).
-compile(export_all).
-export([is_valid/1]).

main([]) ->
    inets:start(),
    inets:start(httpd, [{proplist_file, "priv/server_config.erl"}]),
    io:format("Credit card validation server started.~n"),
    user_drv:start(),
    timer:sleep(infinity).

is_valid(Iban) -> I = spaceParser(Iban), ibanParser(I).
spaceParser(Iban) -> string:join(string:tokens(Iban," "),"").

ibanParser(Iban) ->  
      if Iban == [] -> false;
        true ->
          X = {string:substr(Iban, 1, 2)}, 
          Y = {string:substr(Iban, 3, 2)}, 
          Z = {string:substr(Iban, 5)}, 
          checkIban(X, Y, Z)
      end.

newIban(Country,BankNumber) ->
    CountryCode = letterToNumber(Country, []),
    BankNumberCode = letterToNumber(BankNumber, []),
    Res = string:join([Country,checksum(BankNumber,CountryCode),BankNumber],""),
    io:format("IBAN GENERATED: ~p~n",[Res]),
    Res.

twosection(Sum) when Sum < 100 andalso Sum >= 0 -> lists:flatten(io_lib:format("~2..0B",[Sum])).
checksum(BankNumber,Country) ->
      Check = string:join([BankNumber,Country],""),
      Code = lists:foldl(fun(X,Acc) ->
            Padding = twosection(X),
            {IntNum, _} = string:to_integer(Check++Padding),
%            io:format("IntNum: ~p ~p~n",[IntNum,IntNum rem 97]),
            case IntNum rem 97 of 1 -> Padding; _ -> Acc end
      end,"__",lists:seq(0,99)).

checkIban({X}, {Y}, {Z}) ->
      NumX = letterToNumber(X, []),
%      io:format("NumX: ~p~n",[NumX]),
      NumZ = letterToNumber(Z, []),
%      io:format("NumZ: ~p~n",[NumZ]),
      LastNum = NumZ ++ NumX ++ Y,
%      io:format("LastNum: ~p~n",[NumZ ++ NumX ++ "00"]),
      {{IntNum, _}} = {string:to_integer(LastNum)},
%      io:format("IntNum: ~p~n",[IntNum]),
      D = IntNum rem 97,
%      io:format("D: ~p~n",[D]),
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
  ?_assertEqual(true,is_valid("BA39 1290 0794 0102 8494")),
  ?_assertEqual(true,is_valid("BA39         1290 0794 0102 8494")),
  ?_assertEqual(true,is_valid("BA391290079401028494")),
  ?_assertEqual(true,is_valid("GB29 NWBK 6016 1331 9268 19")),
  ?_assertEqual(true,is_valid("GE29 NB00 0000 0101 9049 17")),
  ?_assertEqual(true,is_valid("GI75 NWBK 0000 0000 7099 453")),
  ?_assertEqual(true,is_valid("GL89 6471 0001 0002 06")),
  ?_assertEqual(true,is_valid("GR16 0110 1250 0000 0001 2300 695")),
  ?_assertEqual(true,is_valid("HR12 1001 0051 8630 0016 0")),
  ?_assertEqual(true,is_valid("HU42 1177 3016 1111 1018 0000 0000")),
  ?_assertEqual(true,is_valid("IE29 AIBK 9311 5212 3456 78")),
  ?_assertEqual(true,is_valid("IL62 0108 0000 0009 9999 999")),
  ?_assertEqual(true,is_valid(" IS14 0159 2600 7654 5510 7303 39")),
  ?_assertEqual(true,is_valid("IT60 X054 2811 1010 0000 0123 456")),
  ?_assertEqual(true,is_valid("KW81 CBKU 0000 0000 0000 1234 5601 01")),
  ?_assertEqual(true,is_valid("KZ86 125K ZT50 0410 0100")),
  ?_assertEqual(true,is_valid("LB62 0999 0000 0001 0019 0122 9114")),
  ?_assertEqual(true,is_valid("LI21 0881 0000 2324 013A A")),
  ?_assertEqual(true,is_valid("LT12 1000 0111 0100 1000")),
  ?_assertEqual(true,is_valid("LV80 BANK 0000 4351 9500 1")),
  ?_assertEqual(true,is_valid("MC11 1273 9000 7000 1111 1000 H79")),
  ?_assertEqual(true,is_valid("ME25 5050 0001 2345 6789 51")),
  ?_assertEqual(true,is_valid("MT84 MALT 0110 0001 2345 MTLC AST0 01S")),
  ?_assertEqual(true,is_valid("MU17 BOMM 0101 1010 3030 0200 000M UR")),
  ?_assertEqual(true,is_valid("NL91 ABNA 0417 1643 00")),
  ?_assertEqual(true,is_valid("NO93 8601 1117 947")),
  ?_assertEqual(true,is_valid("PT50 0002 0123 1234 5678 9015 4")),
  ?_assertEqual(true,is_valid("RO49 AAAA 1B31 0075 9384 0000")),
  ?_assertEqual(true,is_valid("RS35 2600 0560 1001 6113 79")),
  ?_assertEqual(true,is_valid("SA03 8000 0000 6080 1016 7519")),
  ?_assertEqual(true,is_valid("SM86 U032 2509 8000 0000 0270 100")),
  ?_assertEqual(true,is_valid("TR33 0006 1005 1978 6457 8413 26")),
  ?_assertEqual(true,is_valid("BG80 BNBG 9661 1020 3456 78")),
  ?_assertEqual(true,is_valid(" FO62 6460 0001 6316 34")),
  ?_assertEqual(true,is_valid("DK50 0040 0440 1162 43")),
  ?_assertEqual(true,is_valid("CH93 0076 2011 6238 5295 7")),
  ?_assertEqual(true,is_valid("DE89 3704 0044 0532 0130 00")),
  ?_assertEqual(true,is_valid("AD12 0001 2030 2003 5910 0100")),
  ?_assertEqual(true,is_valid(" AL47 2121 1009 0000 0002 3569 8741")),
  ?_assertEqual(true,is_valid("AT61 1904 3002 3457 3201")),
  
  ?_assertEqual(false,is_valid("")),
  ?_assertEqual(false,is_valid("AT61 1904 4002 3457 3201")),
  ?_assertEqual(false,is_valid("CH93 0076 2012 6238 5295 7")),
  ?_assertEqual(false,is_valid("NL91 ABAB 0417 1634 00")),
  ?_assertEqual(false,is_valid("ME25 5050 0021 2345 6789 51")),
  ?_assertEqual(false,is_valid("LV80 BANK KK00 4351 9500 1")),
  ?_assertEqual(false,is_valid("KW81 CBKU A000 0000 0000 1234 5601 01")),
  ?_assertEqual(false,is_valid("KZ86 125K ZT13 0410 0100")),
  ?_assertEqual(false,is_valid("IS14 0159 2600 7654 6120 7303 49")),
  ?_assertEqual(false,is_valid("GR16 0110 12AA 0000 0001 2300 695")),
  ?_assertEqual(false,is_valid("NL91 ABNA BC17 1643 00")),
  ?_assertEqual(false,is_valid("BA39 1290 0794 0102 8493")),
  ?_assertEqual(false,is_valid("BA39 1290 0794 0102 849"))
].
