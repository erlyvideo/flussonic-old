class Flussonic
  def call(env)
    query = Rack::Utils.parse_query(env["QUERY_STRING"])
    if query["file"] =~ /\d+/
      url = "file:///Users/max/Downloads/BigBuckBunny_320x180.mp4"
      [302, {"X-Location" => url, 'Content-Type' => "text/plain"}, ["Redirected to #{url}\n"]]
    else
      [403, {"Content-Type" => "text/plain"}, ["Forbidden #{env["PATH_INFO"]}\n"]]
    end
  end
  
end
