-module(fake_auth).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


init(_,Req, Options) ->
  {Behaviour, Req1} = cowboy_req:binding(behaviour, Req),
  Opts = case Options of
    [] -> [Behaviour];
    _ -> Options
  end,
  {ok, Req1, Opts}.

handle(Req, [Tag]) ->
  {Code, Headers, Body} = reply(Tag, Req),
  {ok, R1} = cowboy_req:reply(Code, Headers, Body, Req),
  {ok, R1, undefined}.

reply(unique_user_id,_) -> {200, [{<<"X-UserId">>, <<"8">>},{<<"X-Unique">>, <<"true">>}], <<"OK">>};
reply(ok,_) -> {200, [], <<"OK">>};
reply(<<"normal">>,_) -> {200, [], "OK"};
reply(<<"600">>,_) -> {200,[{<<"X-AuthDuration">>, <<"600">>}], <<"">>};
reply(<<"deny">>,_) -> {403,[], <<"">>};
reply(<<"500">>,_) -> {500, [], "backend error"};
reply(<<"user15_600">>,_) -> {200,[{<<"X-AuthDuration">>, <<"600">>},{<<"X-UserId">>,<<"15">>}], <<"">>};
reply(<<"user15">>,_) -> {200,[{<<"X-UserId">>,<<"15">>}], <<"">>};
reply(<<"user15_unique">>,_) -> {200,[{<<"X-UserId">>,<<"15">>},{<<"X-Unique">>, <<"true">>}], <<"">>};
reply(<<"user_unique">>,Req) ->
  case cowboy_req:qs_val(<<"token">>,Req) of
    {<<Token/binary>>,_} -> 
      case re:run(Token, "^\\d+$") of
        nomatch -> {403, [], <<>>};
        _ -> {200,[{<<"X-UserId">>,Token},{<<"X-Unique">>, <<"true">>}], <<"">>}
      end;
    _ -> {403,[],<<>>}
  end;
reply(<<"token_a">>,Req) ->
  case cowboy_req:qs_val(<<"token">>,Req) of
    {<<"a">>,_} -> {200,[], <<"">>};
    _ -> {403,[],<<>>}
  end;
reply(<<"token_unique_number">>,Req) ->
  case cowboy_req:qs_val(<<"token">>,Req) of
    {<<Token/binary>>,_} -> 
      case re:run(Token, "^\\d+$") of
        nomatch -> {403, [], <<>>};
        _ -> {200,[{<<"X-UserId">>,<<"15">>},{<<"X-Unique">>, <<"true">>}], <<"">>}
      end;
    _ -> {403,[],<<>>}
  end;
reply(_Tag,_Req) -> {500, [], "backend error"}.



terminate(_,_,_) -> ok.

