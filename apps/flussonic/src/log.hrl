-define(D(X), lager:info("~p:~p ~240p~n", [?MODULE, ?LINE, X])).
-define(DBG(Fmt,X), lager:info("~p:~p "++Fmt++"~n", [?MODULE, ?LINE| X])).
-define(ERR(F,A), lager:error(F ++ "~n", A)).

