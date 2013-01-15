-record(rtp_channel, {
  sequence = undefined,
  wall_clock = undefined,
  timecode = undefined,
  octet_count = 0,
  packet_count = 0,
  warning_count = 0,
  stream_id,
  payload_type,
  timescale,
  codec,
  buffer,
  stream_info,
  length_size, % H264 stuff: how long is NAL size
  last_sr  % NTP Time then last sender report was received
}).
