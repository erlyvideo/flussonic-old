


-record(m3u8_entry, {
  url :: binary(),
  duration :: undefined | non_neg_integer(),
  number = 0 :: non_neg_integer(),
  offset = 0,
  load_time = undefined :: undefined | non_neg_integer(),
  body = undefined :: undefined | binary()
}).

-type m3u8_entry() :: #m3u8_entry{}.

-record(m3u8_playlist, {
  url = undefined :: undefined | binary(),
  bitrate = undefined :: undefined | non_neg_integer(),
  sequence = 0 ::non_neg_integer(),
  type = live :: vod | event | live,
  entries = [] :: list(m3u8_entry())
}).

-type m3u8_playlist() :: #m3u8_playlist{}.


-record(m3u8_mbr_playlist, {
  url = undefined :: undefined | binary(),
  playlists = [] :: list(m3u8_playlist())
}).


