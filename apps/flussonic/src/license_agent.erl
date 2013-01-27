-module(license_agent).

-export([get/1, allow/2, load/1]).


get(_) -> undefined.

allow(Realm, Count) ->
  case license_agent:get(Realm) of
    undefined -> true;
    Limit -> Limit >= Count
  end.

load(_LicenseKey) ->
  io:format("Default license_agent started with key ~p ~s\n", [_LicenseKey, <<1,2,3,4,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>]),
  {error, not_implemented}.

