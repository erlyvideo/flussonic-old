-record(session, {
  session_id :: integer(),
  token :: binary(),
  ip :: binary(),
  name :: binary(),
  user_id = undefined :: undefined | integer(),
  access = denied :: granted | denied,
  type, %  :: <<"hds">>|<<"hls">>|<<"http">>
  created_at,
  auth_time :: non_neg_integer(),
  delete_time :: non_neg_integer(),
  last_access_time :: non_neg_integer(),
  last_verify_time :: non_neg_integer(),
  bytes_sent = 0 :: non_neg_integer(),
  pid = undefined :: undefined | pid(),
  ref = undefined :: undefined | reference(),
  options = []
}).
