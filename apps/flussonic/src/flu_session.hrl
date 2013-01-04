-record(session, {
  session_id :: binary(),
  token :: binary(),
  ip :: binary(),
  name :: binary(),
  user_id = undefined :: undefined | integer(),
  access = denied :: granted | denied,
  type, %  :: <<"hds">>|<<"hls">>|<<"http">>
  created_at,
  auth_time,
  delete_time,
  last_access_time,
  bytes_sent,
  pid,
  ref,
  options = []
}).
