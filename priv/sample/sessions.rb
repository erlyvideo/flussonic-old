#!/usr/bin/env ruby

require 'rubygems'
require 'rack'
require 'thin'
require 'net/http'


Sessions = {}

class MainPage
  def call(env)
    session = rand(100000000).to_s
    Sessions[session] = Time.now + 60
    body = <<-EOF
<html>
<head>
<title>Example page for flussonic sessions</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<script src="http://localhost:8080/flu/js/swfobject.js" type="text/javascript"></script>
</head>
<body>
<div id="video1" style="width:640px;height:480px">
  Video should be here, replacing this text
</div>

<div id="video2" style="width:640px;height:480px">
  Video should be here, replacing this text
</div>

<script type="text/javascript">

function player(element, session) {
var flashvars = {
  src : "http://localhost:8080/securevod/bunny.mp4/manifest.f4m?session="+session,
  autoPlay: true
};
var paramObj = {allowScriptAccess : "always", allowFullScreen : "true", allowNetworking : "all"};
swfobject.embedSWF("http://localhost:8080/flu/StrobeMediaPlayback.swf", element, 640, 480, "10.3", "http://localhost:8080/flu/expressInstall.swf",
  flashvars, paramObj, {name: "StrobeMediaPlayback"});

}

player("video1", "#{session}");
player("video2", "invalid");
</script>
</body>
</html>
    EOF
    [200, {"Content-Type" => "text/html"}, [body]]
  end
end

class AuthPage
  def call(env)
    query = Rack::Utils.parse_query(env["QUERY_STRING"])
    token = query["token"]
    return [403, {}, ["no token\n"]] if !token
    expire_at = Sessions[token]

    if !expire_at
      return [403, {}, ["session not found\n"]]
    end

    if expire_at < Time.now
      Sessions.delete(token)
      return [403, {}, ["session expired\n"]]
    end

    [200, {"X-AuthDuration" => "4000"}, ["accepted\n"]]
  end
end

app = Rack::Builder.new do
  use Rack::CommonLogger
  use Rack::ShowExceptions
  map "/auth" do
    run AuthPage.new
  end

  map "/" do
    run MainPage.new
  end
end

Rack::Handler::Thin.run app, :Port => 9292
