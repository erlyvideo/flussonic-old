-record(session, {
  session_id,
  token,
  ip,
  name,
  user_id,
  flag,
  type, %  :: <<"hds">>|<<"hls">>|<<"http">>
  created_at,
  expire_time,
  last_access_time,
  bytes_sent,
  pid,
  ref,
  options = []
}).
