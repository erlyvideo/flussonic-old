class Flussonic
  def call(env)
    query = Rack::Utils.parse_query(env["QUERY_STRING"])
    if query["referer"] =~ /localhost/
      [200, {'Content-Type' => "text/plain", "X-AuthDuration" => "60"}, ["OK\n"]]
    else
      [403, {"Content-Type" => "text/plain"}, ["Forbidden #{env["PATH_INFO"]}\n"]]
    end
  end
  
end
