{application,rtsp,
             [{description,"RTSP handling library"},
              {vsn,"3.1"},
              {registered,[rtsp]},
              {applications,[kernel,stdlib]},
              {mod,{rtsp,[]}},
              {modules,[rtsp,rtsp_example_callback,rtsp_inbound,rtsp_listener,
                        rtsp_outbound,rtsp_socket,rtsp_sup,rtsp_test_client,
                        rtsp_tests]}]}.
