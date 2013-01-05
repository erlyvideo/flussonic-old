#!/usr/bin/env ruby

require 'rubygems'
require 'rack'
require 'thin'
require 'net/http'
require 'digest/md5'


class MainPage
  def generate_token(name, ip, user_id)
    expire = Time.now.to_i + 600
    salt = rand(1000)
    hash = Digest::MD5.hexdigest("#{expire}:#{salt}:#{name}:#{ip}:#{user_id}")
    "#{expire}:#{user_id}:#{salt}:#{hash}"
  end


  def call(env)
    query = Rack::Utils.parse_query(env["QUERY_STRING"])
    req = Rack::Request.new(env)
    user_id = (query["user_id"] || 15).to_i
    path = query["path"] || "securevod/bunny.mp4"
    token = query["token"] || generate_token(path, req.ip, user_id)
    return [302, {"Location" => "/?path=#{path}&token=#{token}"}, []] unless query["token"]
    streamer = query["streamer"] || "localhost:8080"
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

function player(element, token) {
var flashvars = {
  src : "http://#{streamer}/#{path}/manifest.f4m?token="+token,
  autoPlay: true
};
var paramObj = {allowScriptAccess : "always", allowFullScreen : "true", allowNetworking : "all"};
swfobject.embedSWF("http://localhost:8080/flu/StrobeMediaPlayback.swf", element, 640, 480, "10.3", "http://localhost:8080/flu/expressInstall.swf",
  flashvars, paramObj, {name: "StrobeMediaPlayback"});

}

player("video1", "#{token}");
// player("video2", "invalid");
</script>
</body>
</html>
    EOF
    [200, {"Content-Type" => "text/html"}, [body]]
  end
end

class AuthPage
  def validate_token(token, name, ip)
    expire, user_id, salt, hash = token.split(":")
    expire = expire.to_i
    return :expired if expire < Time.now.to_i
    user_id = user_id.to_i
    good_hash = Digest::MD5.hexdigest("#{expire}:#{salt}:#{name}:#{ip}:#{user_id}")
    if good_hash != hash
      return :invalid_hash
    end
    {:user_id => user_id, :expire => expire}
  end

  def call(env)
    query = Rack::Utils.parse_query(env["QUERY_STRING"])
    req = Rack::Request.new(env)
    token = query["token"]
    name = query["name"]
    ip = req.ip

    res = validate_token(token, name, ip)
    if res.is_a?(Hash)
      [200, {"X-AuthDuration" => "4000", "X-Unique" => "true", "X-UserId" => res[:user_id].to_s}, ["accepted\n"]]
    else
      [403, {}, "forbidden: #{res}\n"]
    end
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
