-module(license_agent).
-export([get/1, allow/2, load/2]).


get(_) -> undefined.


allow(Realm, Count) ->
  case license_agent:get(Realm) of
    undefined -> true;
    Limit -> Limit >= Count
  end.

load(_LicenseKey, _CryptedContent) ->
  {error, not_implemented}.

