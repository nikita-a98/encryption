%%%-------------------------------------------------------------------
%%% @author nikita
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Март 2019 20:36
%%%-------------------------------------------------------------------
-module(encryption).

-author("nikita").

%% MACROS
-define(START_POSITION, 96).
-define(END_POSITION, 123).
-define(MAX_INTERVAL, 27).
-define(SPACE, 32).
-define(ENCRYPTION_FILE, "../../files/encr_text.txt").
-define(DECRYPTION_FILE, "../../files/decr_text.txt").

-define(START_CYRILLIC_POSITION, 1072).
-define(END_CYRILLIC_POSITION, 1103).

%% API
-export([
  encode_cesar/1,
  decode_cesar/1,
  encode_vizhener/1,
  decode_vizhener/1,
  encode_couple/1,
  decode_couple/1,
  encode_polibiy/0,
  decode_polibiy/0
]).


%% ===============================================================
%% API functions
%% ===============================================================
encode_cesar(Number) when Number =< ?MAX_INTERVAL ->
  {ok, BinText} = file:read_file("../../files/file.txt"),
  Text = string:lowercase(binary:bin_to_list(BinText)),
  F =
    fun
      (Sym, Acc) when Sym == ?SPACE ->
        NewSym = (?START_POSITION + Number) rem ?END_POSITION,
        [NewSym|Acc];
      (Sym, Acc) when Sym + Number == ?END_POSITION ->
        [?SPACE|Acc];
      (Sym, Acc) when Sym + Number > ?END_POSITION ->
        NewSym = ((Sym + Number) rem ?END_POSITION) + ?START_POSITION,
        [NewSym|Acc];
      (Sym, Acc) when Sym < ?END_POSITION andalso Sym > ?START_POSITION ->
        NewSym = (Sym + Number),
        [NewSym|Acc];
      (_, Acc) -> Acc
    end,
  EncrText = lists:foldr(F, [], Text),
  file:write_file(?ENCRYPTION_FILE, EncrText).

decode_cesar(Number) when Number =< ?MAX_INTERVAL ->
  {ok, BinText} = file:read_file(?ENCRYPTION_FILE),
  Text = binary:bin_to_list(BinText),
  F =
    fun
      (Sym, Acc) when Sym-Number == ?START_POSITION ->
        [?SPACE|Acc];
      (Sym, Acc) when Sym == ?SPACE ->
        Symbol = ?START_POSITION-Number + ?MAX_INTERVAL,
        [Symbol|Acc];
      (Sym, Acc) when Sym-Number < ?START_POSITION ->
        Symbol = Sym-Number + ?MAX_INTERVAL,
        [Symbol|Acc];
      (Sym, Acc) ->
        Symbol = Sym-Number,
        [Symbol|Acc]
      end,
  DecrText = lists:foldr(F, [], Text),
  file:write_file(?DECRYPTION_FILE, DecrText).


encode_vizhener(BinKey) ->
  {ok, BinText} = file:read_file("../../files/file.txt"),
  Text = string:lowercase(binary:bin_to_list(BinText)),
  Key = binary:bin_to_list(BinKey),
  F =
    fun(Sym, {Acc, KeyTailIn}) ->
      {Number, KeyTail} = key_number(KeyTailIn, Key),
      case Sym of
        Sym when Sym == ?SPACE ->
          NewSym = (?START_POSITION + Number) rem ?END_POSITION,
          {[NewSym|Acc], KeyTail};
        Sym when Sym + Number == ?END_POSITION ->
          {[?SPACE|Acc], KeyTail};
        Sym when Sym + Number > ?END_POSITION ->
          NewSym = ((Sym + Number) rem ?END_POSITION) + ?START_POSITION,
          {[NewSym|Acc], KeyTail};
        Sym when Sym < ?END_POSITION andalso Sym > ?START_POSITION ->
          NewSym = (Sym + Number),
          {[NewSym|Acc], KeyTail};
        _ ->
          {Acc, KeyTail}
      end
    end,
  {EncrText, _} = lists:foldl(F, {[], Key}, Text),
  file:write_file(?ENCRYPTION_FILE, lists:reverse(EncrText)).

decode_vizhener(BinKey) ->
  {ok, BinText} = file:read_file(?ENCRYPTION_FILE),
  Text = binary:bin_to_list(BinText),
  Key = binary:bin_to_list(BinKey),
  F =
    fun(Sym, {Acc, KeyTailIn}) ->
      {Number, KeyTail} = key_number(KeyTailIn, Key),
      case Sym of
        Sym when Sym-Number == ?START_POSITION ->
          {[?SPACE|Acc], KeyTail};
        Sym when Sym == ?SPACE ->
          Symbol = ?START_POSITION-Number + ?MAX_INTERVAL,
          {[Symbol|Acc], KeyTail};
        Sym when Sym-Number < ?START_POSITION ->
          Symbol = Sym-Number + ?MAX_INTERVAL,
          {[Symbol|Acc], KeyTail};
        Sym ->
          Symbol = Sym-Number,
          {[Symbol|Acc], KeyTail}
      end
    end,
  {DecrText, _} = lists:foldl(F, {[], Key}, Text),
  file:write_file(?DECRYPTION_FILE, lists:reverse(DecrText)).


encode_couple(StringPhrase) ->
  {ok, BinText} = file:read_file("../../files/file.txt"),
  Text = string:lowercase(unicode:characters_to_list(BinText)),
  Phrase = unicode:characters_to_list(StringPhrase),
  SortPhrase = string:lowercase(lists:sort(Phrase)),
  true = (length(SortPhrase)  >= 15),
  EncrText = mask_couple(SortPhrase, Text),
  file:write_file(?ENCRYPTION_FILE, unicode:characters_to_binary(EncrText)).

decode_couple(StringPhrase) ->
  {ok, BinText} = file:read_file(?ENCRYPTION_FILE),
  Text = string:lowercase(unicode:characters_to_list(BinText)),
  Phrase = unicode:characters_to_list(StringPhrase),
  SortPhrase = string:lowercase(lists:sort(Phrase)),
  true = (length(SortPhrase)  >= 15),
  DecrText = mask_couple(SortPhrase, Text),
  file:write_file(?DECRYPTION_FILE, unicode:characters_to_binary(DecrText)).


encode_polibiy() ->
  {ok, BinText} = file:read_file("../../files/file.txt"),
  Text = string:lowercase(unicode:characters_to_list(BinText)),
  Proplist = mask_polibiy(encode),
  F1 =
    fun
      (X, Acc) ->
        X1 = proplists:get_value(X, Proplist),
        case X1 of
          undefined -> Acc;
          _ -> [list_to_binary(integer_to_list(X1)),?SPACE|Acc]
        end
    end,
  EncrText = lists:foldr(F1, [], Text),
  file:write_file(?ENCRYPTION_FILE, EncrText).

decode_polibiy() ->
  {ok, BinText} = file:read_file("../../files/encr_text.txt"),
  Text = string:tokens(binary:bin_to_list(BinText), " "),
  Proplist = mask_polibiy(decode),
  F1 =
    fun
      (X, Acc) ->
        {X1, _} = string:to_integer(X),
        X2 = proplists:get_value(X1, Proplist),
        case X2 of
          undefined -> Acc;
          _ -> [X2|Acc]
        end
    end,
  DecrText = lists:foldr(F1, [], Text),
  file:write_file(?DECRYPTION_FILE, unicode:characters_to_binary(DecrText)).

%% ===============================================================
%% Internal functions
%% ===============================================================

key_number([], Key) ->
  [H|Tail] = Key,
  Number = H - ?START_POSITION,
  {Number, Tail};
key_number([H|Tail], _) ->
  Number = H - ?START_POSITION,
  {Number, Tail}.

mask_couple(SortPhrase, Text) ->
  All = lists:append([?SPACE|lists:seq(?START_CYRILLIC_POSITION, ?END_CYRILLIC_POSITION)], lists:seq(48, 57)),
  WithOutPhrase = lists:subtract(All, SortPhrase),
  F =
    fun
      (X, {Acc, Out}) ->
        [Y|Tail] = Out,
        {[{Y,X}|[{X,Y}|Acc]], Tail}
    end,
  {Tuple, _} = lists:foldl(F, {[], WithOutPhrase}, SortPhrase),
  F1 =
    fun
      (X, Acc) ->
        X1 = proplists:get_value(X, Tuple, X),
        [X1|Acc]
    end,
  lists:foldr(F1, [], Text).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%      1  2  3  4  5  6
%%
%%  1   А  Б  В  Г  Д  Е
%%  2   Ё  Ж  З  И  Й  К
%%  3   Л  М  Н  О  П  Р
%%  4   С  Т  У  Ф  Х  Ц
%%  5   Ч  Ш  Щ  Ъ  Ы  Ь
%%  6   Э  Ю  Я  ,  .  ?
%%  7   !  :  ;  (  )  №
%%  8   -  1  2  3  4  5
%%  9   6  7  8  9  0  _
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mask_polibiy(V) ->
  ListSym =
    lists:append([
      lists:seq(1072, 1103),
      [44,46,63,33,58,59,40,41,45],
      lists:seq(48, 57),
      [32]
    ]),
  F =
    case V of
      encode ->
        fun
          (Sym, {N, Acc}) when Acc == 6 ->
            {{Sym, N+5}, {N+5, 1}};
          (Sym, {N, Acc}) ->
            {{Sym, N+1}, {N+1, Acc+1}}
        end;
      decode ->
        fun
          (Sym, {N, Acc}) when Acc == 6 ->
            {{N+5, Sym}, {N+5, 1}};
          (Sym, {N, Acc}) ->
            {{N+1, Sym}, {N+1, Acc+1}}
        end
    end,
  {Proplist, _} = lists:mapfoldl(F, {10, 0}, ListSym),
  Proplist.