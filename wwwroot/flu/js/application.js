
(function($) { 

if($.fn.simpleDatepicker) {
  $.fn.simpleDatepicker.formatOutput = function (t) {
    return "" + (t.getYear() + 1900) +"/" +(t.getMonth() + 1) + "/" + t.getDate();
  }
}

Erlyvideo = {
  osmf_player: function(element, url) {
    var width = 640;
    var height = 480;
    
    if(url.indexOf("http://") != 0) {
      if(url[0] != "/") url = "/" + url;
      url = "http://"+window.location.host+url;
    }
    if(url.indexOf(".f4m") == -1) url = url + "/manifest.f4m?session="+((new Date()).getTime())
  	var flashvars = {
  		src : url,
      // javascriptCallbackFunction: "onJavaScriptBridgeCreated",
  		autoPlay: true
  	};
  	var paramObj = {allowScriptAccess : "always", allowFullScreen : "true", allowNetworking : "all"};
    swfobject.embedSWF("/flu/StrobeMediaPlayback.swf", element, width, height, "10.3", "/flu/expressInstall.swf",
      flashvars, paramObj, {name: "StrobeMediaPlayback"});
  },
  
  jwplayer: function(element, url) {
    var app = "live";
    if(url.indexOf("vod/") == 0) {
      app = "vod";
      url = url.substring(3, url.length);
    }
    var flashvars = {
        file: url+"?session="+((new Date()).getTime()),
        streamer:'rtmp://'+window.location.hostname+':1935/'+app,
        'rtmp.tunneling':false,
        autostart: true
      };

      swfobject.embedSWF('/flu/jwplayer.swf',element,'640','480','10.3','false', flashvars,

       {allowfullscreen:'true',allowscriptaccess:'always'},
       {id:'jwplayer',name:'jwplayer'}

      );
  },
  
  hls: function(element, stream) {
    $(element).html("<video width=640 height=480 src=\""+stream+"?session="+((new Date()).getTime())+"\" autoplay controls></video>");
  },
  
  load_stream_info: function() {
    // console.log("loading");
    if(!Erlyvideo.stream_ws && window.WebSocket && !Erlyvideo.disabled_ws) {
      Erlyvideo.stream_ws = new WebSocket("ws://"+window.location.host+"/erlyvideo/api/streams");
      Erlyvideo.stream_ws.onerror = function() {
        Erlyvideo.disabled_ws = true;
        setTimeout(function() {
          Erlyvideo.disabled_ws = false;
        }, 10000);
      }
      Erlyvideo.stream_ws.onmessage = function(reply) {
        Erlyvideo.draw_stream_info(JSON.parse(reply.data));
      }
      Erlyvideo.stream_ws.onclose = function() {
        Erlyvideo.stream_ws = null;
        delete Erlyvideo["stream_ws"];
      }
    }
    if(Erlyvideo.stream_ws) {
      if(Erlyvideo.stream_ws.readyState == WebSocket.OPEN) {
        Erlyvideo.stream_ws.send("streams");      
      } else {
        Erlyvideo.stream_ws.onopen = function() {          
          Erlyvideo.stream_ws.send("streams");      
        }
      }
    } else {
      $.get("/erlyvideo/api/streams", {}, function(streams) {
        Erlyvideo.draw_stream_info(streams);
      });      
    }
    Erlyvideo.stream_load_timer = setTimeout(Erlyvideo.load_stream_info, 3000);
  },

  info_template: "Total clients: {{total}}<br/> \
  Total file clients: {{total_file}}<br/>",
  
  stream_template: "<tr id=\"stream-{{name}}\" playprefix=\"{{play_prefix}}\">\
      <td class='first'  valign='top'>\
      <a href='#' onclick='Erlyvideo.open_stream_tab(\"{{name}}\"); return false;'>{{name}}</a> \
      </td> \
      <td class='stream-play'>\
      <a class='s-hds' style='visibility: {{hds}}' href='#' onclick='Erlyvideo.play_stream(\"{{play_name}}\",\"hds\"); return false;'><span class='hds'></span>{{name}}</a>\
      <a class='s-rtmp' style='visibility: {{rtmp}}' href='#' onclick='Erlyvideo.play_stream(\"{{name}}\",\"rtmp\"); return false;'><span class='rtmp'></span>{{name}}</a> \
      <a class='s-hls' style='visibility: {{hls}}' href='#' onclick='Erlyvideo.play_stream(\"{{play_name}}\",\"hls\"); return false;'><span class='hls'></span>{{name}}</a> \
      <a class='s-dvr' style='visibility: {{dvr}}' href='#' onclick='Erlyvideo.show_dvr_status(\"{{name}}\", {play_name : \"{{play_name}}\"}); return false'><span class='dvr'></span>{{name}}</a>\
      <div id=\"clients-{{name}}\"></div>\
      </td>\
      <td><a href='#' onclick='Erlyvideo.show_clients(\"{{name}}\"); return false;' class='client_count'>{{client_count}}</a></td> \
      <td class='lifetime'>{{lifetime}}</td> \
      <td class='ts_delay'>{{ts_delay}}</td> \
      <td class='retry_count'>{{retry_count}}</td> \
    </tr>",
  
  session_template: "<a href='#' onclick='$(\"#clients-{{name}}\").html(\"\"); return false;' style='width: auto'>Close</a>\
  <table width=\"100%\">\
  <thead><tr><th>IP</th><th>UserID</th><th>Type</th><th>Name</th><th>Time</th></tr></thead>\
  {{#sessions}}<tr><td>{{ip}}</td><td>{{user_id}}</td><td>{{type}}</td><td>{{name}}</td><td>{{duration}}</td></tr>{{/sessions}}\
  </table>",
  
  show_dvr_status: function(name, opts) {
    $("#dvr-list").showDVR(name, opts || {});
  },
  
  show_clients: function(name) {
    $.get("/erlyvideo/api/sessions", {}, function(sessions) {
      var sess1 = [];
      for(var i = 0; i < sessions.length; i++) {
        if(sessions[i].name == name) sess1[sess1.length] = sessions[i];
      }
      $("#clients-"+name).html(Mustache.to_html(Erlyvideo.session_template, {sessions : sess1, name : name}));
    });
  },

  current_streams: {},

  draw_stream_info: function(streams) {
    var i;
    var total = 0;
    var total_file = 0;
    
    if(streams["version"]) {
      $("#flu_version").html(streams["version"]);
    }

    var new_streams = {};
    
    for(i = 0; i < streams["streams"].length; i++) {
      var s = streams["streams"][i];
      s.play_name = s.name;
      s.lifetime = Math.round(s.lifetime / 1000);
      s.ts_delay = s.ts_delay < 5000 ? 0 : Math.round(s.ts_delay / 1000);
      if(s.type == "file") {
        s.ts_delay = 0;
        total_file += s.client_count;
      }
      if(s.play_prefix) {
        s.play_name = s.play_prefix + "/" + s.play_name;
      }
      if(!Erlyvideo.current_streams[s.name]) {
        Erlyvideo.current_streams[s.name] = true;
        $("#stream-list").append(Mustache.to_html(Erlyvideo.stream_template, s));
      } else {
        var s1 = $("#stream-"+s.name);
        s1.find(".client_count").html(s.client_count);
        if(s.lifetime) s1.find(".lifetime").html(s.lifetime);
        if(s.ts_delay) s1.find(".ts_delay").html(s.ts_delay);
        if(s.retry_count) s1.find(".retry_count").html(s.retry_count);
        s1.find(".s-hds").css('visibility', s.hds ? "visible" : "hidden");
        s1.find(".s-hls").css('visibility', s.hls ? "visible" : "hidden");
        s1.find(".s-dvr").css('visibility', s.dvr ? "visible" : "hidden");
        s1.find(".s-rtmp").css('visibility', s.rtmp ? "visible" : "hidden");
      }
      new_streams[s.name] = true;
      total += s.client_count;
    }

    for(var k in Erlyvideo.current_streams) {
      if(!new_streams[k]) {
        delete Erlyvideo.current_streams[k];
        $("#stream-"+k).remove();
      }
    }
    var info = {};
    info["total"] = total;
    info["total_file"] = total_file;
    $("#stream-info").html(Mustache.to_html(Erlyvideo.info_template, info));
    // $("#stream-list").html(Mustache.to_html(Erlyvideo.stream_template, streams));
  },
  
  stop_periodic_stream_loader: function() {
    if(Erlyvideo.stream_load_timer) clearTimeout(Erlyvideo.stream_load_timer);
    Erlyvideo.stream_load_timer = undefined;
  },
  
  load_license_info: function() {
    $.get("/erlyvideo/api/licenses", {}, function(licenses) {
      licenses = licenses["licenses"];
      var i,j;
      for(i = 0; i < licenses.length; i++) {
        var vers = [];
        var name = licenses[i].name;
        for(j = 0; j < licenses[i].versions.length; j++) {
          var ver = licenses[i].versions[j];
          vers[vers.length] = {
            version: ver,
            name: name,
            checked: licenses[i].current_version == ver
          };
        }
        licenses[i].versions = vers;
      }
      if(licenses.length > 0) {
        $("#license-list").html(Mustache.to_html(Erlyvideo.license_template, {"licenses" : licenses}));
      }
    });
  },
  
  enable_licenses: function() {
    $("#license-save-form").submit(function() {
      $.post(this.action, $(this).serialize(), function(reply) {
        reply = eval('('+reply+')');
        if(reply) {
          alert("Licenses loaded, restart erlyvideo to see effects");
        } else {
          alert("Failed to select software versions. Consult logs for details");
        }
      });
      return false;
    });
  },
  
  license_template: "{{#licenses}}<div class=\"column\"> \
  	<div class=\"group\"> \
      <label class=\"label\">{{name}}</label> \
      {{#versions}} \
      <div> \
        <input type=\"radio\" name=\"{{name}}\" class=\"checkbox\" id=\"version_{{name}}_{{version}}\" value=\"{{version}}\" {{#checked}}checked{{/checked}}/> \
        <label for=\"version_{{name}}_{{version}}\" class=\"radio\">{{version}}</label> \
      </div> \
      {{/versions}} \
    </div> \
	</div> \
	{{/licenses}} \
	<div class=\"group navform wat-cf\"> \
    <button class=\"button\" type=\"submit\"> \
      <img src=\"images/icons/tick.png\" alt=\"Save\" /> Save \
    </button> \
    <span class=\"text_button_padding\">or</span> \
    <a class=\"text_button_padding link_button\" href=\"#license\">Cancel</a> \
  </div> \
	",
  
  open_stream_tab: function(stream) {
  },
  
  enable_play_tab: function() {
    $("#play-tab form").submit(function() {
      return false;
    });

    $("#play-tab form button").click(function() {
      var url = $("#requested-stream-name").val();
      if(url.indexOf("/") == 0) {
        url = url.substring(1, url.length);
        $("#requested-stream-name").val(url);
      }
      Erlyvideo.play_stream(url, $(this).val());
      return false;
    });
  },
  
  play_stream: function(stream, player) {
    if(player == "hds") {
      Erlyvideo.osmf_player("player-embed", stream);
    } else if(player == "rtmp") {
      Erlyvideo.jwplayer("player-embed", stream);
    } else if(player == "hls") {
      Erlyvideo.hls("#player-embed", stream.indexOf("m3u8") == -1 ? stream+"/index.m3u8" : stream);
    }
    $("#block-login").dialog('open');
  },
  
  activate_tab: function(tabname) {
    Erlyvideo.stop_periodic_stream_loader();
    Erlyvideo.stop_periodic_traffic_loader();
    $(".tabbed-menu li").removeClass("active");
    $("#main .content").hide();
    $("#"+tabname+"-tab").show();
    $(".tabbed-menu a[href=#"+tabname+"]").parent().addClass("active");
    
    if(tabname == "streams") Erlyvideo.load_stream_info();
    // if(tabname == "license") Erlyvideo.load_license_info();
    // if(tabname == "stats") Erlyvideo.load_traffic_stat();
    return false;
  },
  
  enable_tabs: function() {
    $(".tabbed-menu a, a.link-button").live('click', function() {
      Erlyvideo.activate_tab($(this).attr('href').substring(1));
    });
  },
  
  traffic_template: "<table><caption>Traffic statistics for {{iface}}</caption> \
  <thead><tr><td></td>\
  {{#traffic}}<th>{{time}}</th>{{/traffic}}\
  </tr></thead>\
  <tbody>\
  <tr><td>Input</td>\
  {{#traffic}}<td>{{input}}</td>{{/traffic}} \
  </tr>\
  <tr><td>Output</td>\
  {{#traffic}}<td>{{output}}</td>{{/traffic}} \
  </tr>\
  </tbody>\
  </table>",
  
  load_traffic_stat: function() {
    $.get("/erlyvideo/api/traffic", {}, function(traffic) {
      $("#traffic-stats").html(Mustache.to_html(Erlyvideo.traffic_template, traffic));
      $('#traffic-stats table').visualize({type: 'line', width: '800px'});
    });
    Erlyvideo.traffic_load_timer = setTimeout(Erlyvideo.load_traffic_stat, 3000);
  },
  
  stop_periodic_traffic_loader: function() {
    if(Erlyvideo.traffic_load_timer) clearTimeout(Erlyvideo.traffic_load_timer);
    Erlyvideo.traffic_load_timer = undefined;
  },
  
  on_message: function(event) {
    // console.log(event.data);
    var message = eval("("+event.data+")");
    if(message.event == "stream.next_minute" && Erlyvideo.current_dvr_stream == message.stream) {
      $("div[time=\""+message.options.timestamp+"\"]").addClass("ok").removeClass("fail");
    } else if(message.event == "stream.list") {
      Erlyvideo.draw_stream_info(message);
    }
  }
};

$.mustache = function(template, view, partials) {
  return Mustache.to_html(template, view, partials);
};




$(function() {
  var params = parseQueryString();
  // Erlyvideo.comet = $.bullet("ws://"+window.location.host + "/comet");
  // Erlyvideo.comet.onmessage = Erlyvideo.on_message;
  
  Erlyvideo.enable_tabs();
  Erlyvideo.enable_licenses();
  if(window.location.hash != "") {
    Erlyvideo.activate_tab(window.location.hash.substring(1));
  } else if(params["file"] && params["file"].length > 0) {
    Erlyvideo.activate_tab("play");
    $("#requested-stream-name").val(params["file"]);
  } else {
    Erlyvideo.activate_tab("streams");
  }
  Erlyvideo.enable_play_tab();
  // $('#traffic-stats').visualize({type: 'line', width: '800px'});
	
  $("#block-login").dialog({autoOpen:false, title : "Play Stream", width: 840, height: 700});
  $("#block-login").bind("dialogclose", function() {
    $("#block-login .content").html('<div id="player-embed"></div>');
  });
  
})

})(jQuery);