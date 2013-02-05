class Flussonic
  def call(env)
    query = Rack::Utils.parse_query(env["QUERY_STRING"])
    if query["name"] =~ /\d+/
      url = "file:///Users/max/Downloads/BigBuckBunny_320x180.mp4"
      [200, {'Content-Type' => "text/plain", "X-AuthDuration" => "2"}, ["OK\n"]]
    else
      [403, {"Content-Type" => "text/plain"}, ["Forbidden #{env["PATH_INFO"]}\n"]]
    end
  end
  
end
