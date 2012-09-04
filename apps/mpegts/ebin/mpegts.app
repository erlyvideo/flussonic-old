{application,mpegts,
             [{description,"MPEG-TS handling library"},
              {vsn,"3.1"},
              {registered,[mpegts]},
              {applications,[kernel,stdlib]},
              {mod,{mpegts_app,[]}},
              {modules,[iso8859_5,mpeg2_crc32,mpegts,mpegts_app,mpegts_dumper,
                        mpegts_file_reader,mpegts_play,mpegts_psi,
                        mpegts_reader,mpegts_sup,reverse_mpegts]}]}.
