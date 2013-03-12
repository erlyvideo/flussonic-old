(function($) { 

if($.fn.simpleDatepicker) {
  $.fn.simpleDatepicker.formatOutput = function (t) {
    return "" + (t.getYear() + 1900) +"/" +(t.getMonth() + 1) + "/" + t.getDate();
  }
}

Erlyvideo = {

// Templates for players
  hds_player: "StrobeMediaPlayback",

  // HDS player  
  osmf_player: function(element, url, info) {
    var width = info && info.width || 640;
    var height = info && info.height || 480;
    
    if(url.indexOf("http://") != 0) {
      if(url[0] != "/") url = "/" + url;
      url = "http://"+window.location.host+url;
    }
    if(url.indexOf(".f4m") == -1 && url.indexOf(".m3u8") == -1) url = url + "/manifest.f4m";

    // url = url + "?token="+((new Date()).getTime());
  	var flashvars = {
  		src : url,
      // javascriptCallbackFunction: "onJavaScriptBridgeCreated",
  		autoPlay: true
  	};
  	var paramObj = {allowScriptAccess : "always", allowFullScreen : "true", allowNetworking : "all"};
    swfobject.embedSWF("/flu/"+Erlyvideo.hds_player+".swf", element, width, height, "10.3", "/flu/expressInstall.swf",
      flashvars, paramObj, {name: Erlyvideo.hds_player});
  },
  
  // RTMP JWplayer
  jwplayer: function(element, url, info) {
    var width = info && info.width || 640;
    var height = info && info.height || 480;

    var app = "live";
    if(url[0] == "/") url = url.substring(1, url.length);
    var slash = url.indexOf("/");
    if(slash != -1) {
      app = url.substring(0, slash);
      url = url.substring(slash+1, url.length);
    }
    var flashvars = {
        file: url,
        streamer:'rtmp://'+window.location.hostname+':1935/'+app,
        'rtmp.tunneling':false,
        autostart: true
      };

      swfobject.embedSWF('/flu/jwplayer.swf',element,width,height,'10.3','false', flashvars,

       {allowfullscreen:'true',allowscriptaccess:'always'},
       {id:'jwplayer',name:'jwplayer'}

      );
  },
  

  // HLS player
  hls: function(element, stream, info) {
    var width = info && info.width || 640;
    var height = info && info.height || 480;
    // stream = stream + "?token="+((new Date()).getTime());
    $(element).html("<video width="+width+" height="+height+" src=\""+stream+"\" autoplay controls></video>");
  },


// Tab control

  enable_tabs: function() {
    $(".tabbed-menu a, a.link-button").on('click', function() {
      Erlyvideo.activate_tab($(this).attr('href').substring(1));
    });
  },

  activate_tab: function(tabname) {
    if(tabname == "support" && window.FreshWidget) {
      return false;
    }
    Erlyvideo.stop_periodic_stream_loader();
    Erlyvideo.stop_periodic_pulse_loader();
    $(".tabbed-menu li").removeClass("active");
    $("#main .content").hide();
    $("#"+tabname+"-tab").show();
    $(".tabbed-menu a[href=#"+tabname+"]").parent().addClass("active");
    
    if(tabname == "streams") Erlyvideo.load_stream_info();
    // if(tabname == "license") Erlyvideo.load_license_info();
    if(tabname == "pulse") Erlyvideo.load_pulse();
    return false;
  },
  

// Flussonic connectivity

  // try to maintain persistent connection with flussonic via websockets
  connect: function() {
    if(!Erlyvideo.stream_ws && window.WebSocket && !Erlyvideo.disabled_ws) {
      Erlyvideo.stream_ws = new WebSocket("ws://"+window.location.host+"/erlyvideo/api/events");
      Erlyvideo.stream_ws.onerror = function() {
        Erlyvideo.disabled_ws = true;
        setTimeout(function() {
          Erlyvideo.disabled_ws = false;
          Erlyvideo.connect();
        }, 5000);
      }
      Erlyvideo.stream_ws.onmessage = function(reply) {
        Erlyvideo.on_message(JSON.parse(reply.data));
      }
      Erlyvideo.stream_ws.onclose = function() {
        Erlyvideo.stream_ws = null;
        delete Erlyvideo["stream_ws"];
      }
    }
  },

  // make request to flussonic via websocket or comet
  request: function(resource) {
    Erlyvideo.connect();
    if(Erlyvideo.stream_ws) {
      if(Erlyvideo.stream_ws.readyState == WebSocket.OPEN) {
        Erlyvideo.stream_ws.send(resource);
      } else {
        Erlyvideo.stream_ws.onopen = function() {          
          Erlyvideo.stream_ws.send(resource);
        }
      }
    } else {
      $.get("/erlyvideo/api/"+resource, {}, function(reply) {
        Erlyvideo.on_message(reply);
      });      
    }
  },

  // Messages from flussonic are routed here. Either from WS, either from comet - doesn't matter 
  dump_events: false,

  on_message: function(message) {
    if(Erlyvideo.dump_events) console.log(message);
    Erlyvideo.restart_connect_label_timer();
    switch(message.event) {
      case "server.info":
        Erlyvideo.request("streams");
        Erlyvideo.draw_server_info(message);
        break;
      case "stream.list":
        Erlyvideo.draw_stream_list(message);
        break;
      case "file.list":
        Erlyvideo.draw_stream_list(message);
        break;
      case "stream.add_dvr_fragment":
        Erlyvideo.add_dvr_fragment(message);
        break;
      case "stream.delete_dvr_fragment":
        Erlyvideo.delete_dvr_fragment(message);
        break;
      case "user.list":
        Erlyvideo.draw_clients(message);
        break;
      case "pulse.traffic":
        Erlyvideo.draw_pulse_traffic(message);
        break;
      case "user.connected":
      case "user.disconnected":
        break;
      default:
        console.log(message);
    }
  },

  connect_label_timer: null,
  restart_connect_label_timer: function() {
    if(Erlyvideo.connect_label_timer) clearTimeout(Erlyvideo.connect_label_timer);

    Erlyvideo.connect_label_timer = setTimeout(Erlyvideo.warn_connect_label, 4000);
    $("#connect_label").css("opacity", 0);
  },

  warn_connect_label: function() {
    $("#connect_label").animate({opacity: 1.0}, "slow");
  },

  draw_server_info: function(message) {
    if(message.version) {
      $("#flu_version").html(message.version);
    }
    if(message.license == true) {
      $("#flu_license").html("Commercial");
    }
    if(message.license == false) {
      $("#flu_license").html("Free");
    }
  },

  // Streams main panel. Drawing list of streams, dvr record status, etc
  add_dvr_fragment: function(message) {
    if(message.stream == Erlyvideo.current_opened_dvr) {
      var minute = Math.floor(message.options.time / 60)*60;
      $("div[time=\""+minute+"\"]").addClass("ok").removeClass("fail");
    }
  },

  delete_dvr_fragment: function(message) {
    if(message.stream == Erlyvideo.current_opened_dvr) {
      var minute = Math.floor(message.options.time / 60)*60;
      $("div[time=\""+minute+"\"]").removeClass("ok").addClass("fail");
    }
  },

  restart_stream: function(stream) {
    $.post("/erlyvideo/api/stream_restart/"+stream, {});
  },
  
  load_stream_info: function() {
    // console.log("loading");
    Erlyvideo.request("streams");
    Erlyvideo.request("files");
    Erlyvideo.stream_load_timer = setTimeout(Erlyvideo.load_stream_info, 3000);
  },

  info_template: "Total clients: {{total}}<br/> \
  Total file clients: {{total_file}}<br/>",
  
  stream_template: "<tr id=\"stream-{{vname}}\">\
      <td class='first'  valign='top'>{{name}}</td> \
      <td class='stream-play'>\
      <a class='s-hds' style='visibility: {{hds}}' href='#' onclick='Erlyvideo.play_stream(\"{{play_name}}\",\"hds\"); return false;'><span class='hds'></span>{{name}}</a>\
      <a class='s-rtmp' style='visibility: {{rtmp}}' href='#' onclick='Erlyvideo.play_stream(\"{{name}}\",\"rtmp\"); return false;'><span class='rtmp'></span>{{name}}</a> \
      <a class='s-hls' style='visibility: {{hls}}' href='#' onclick='Erlyvideo.play_stream(\"{{play_name}}\",\"hls\"); return false;'><span class='hls'></span>{{name}}</a> \
      <a class='s-dvr' style='visibility: {{dvr}}' href='#' onclick='Erlyvideo.show_dvr_status(\"{{name}}\", {play_name : \"{{play_name}}\"}); return false'><span class='dvr'></span>{{name}}</a>\
      <div id=\"clients-{{vname}}\" style='display: none'>\
        <a href='#' onclick='Erlyvideo.hide_clients(\"{{vname}}\"); return false;' style='width: auto'>Close</a> \
        <table width=\"100%\">\
          <thead><tr><th>IP</th><th>UserID</th><th>Type</th><th>Name</th><th>Time</th></tr></thead>\
          <tbody id=\"clients-list-{{vname}}\"></tbody>\
        </table>\
      </div>\
      </td>\
      <td><a href='#' onclick='Erlyvideo.show_clients(\"{{name}}\"); return false;' class='client_count'>{{client_count}}</a></td> \
      <td class='lifetime'>{{lifetime}}</td> \
      <td class='ts_delay'>{{ts_delay}}</td> \
      <td class='retry_count'>{{retry_count}}</td> \
      <td class='stream-restart'><a href='#' onclick='Erlyvideo.restart_stream(\"{{name}}\");return false;'><span class='restart'></span>restart</a></td> \
    </tr>",
  
  session_template: "<tr id='session-{{id}}' data-id='{{id}}'><td>{{ip}}</td><td>{{user_id}}</td><td>{{type}}</td><td>{{name}}</td><td class='duration'>{{duration}}</td></tr>",
  
  show_dvr_status: function(name, opts) {
    Erlyvideo.current_opened_dvr = name;
    $("#dvr-list").showDVR(name, opts || {});
  },
  
  current_streams: {
    "streams" : {},
    "files" : {}
  },

  vname: function(name) {
    return name.replace(/\//g, "_").replace(/\./g, "_");
  },

  draw_stream_list: function(msg) {
    var i;
    var total = 0;
    var total_file = 0;
    
    var new_streams = {};

    var type = 
      msg.event == "stream.list" ? "streams" : 
      msg.event == "file.list" ? "files" : [];

    var msg_streams = msg[type];

    for(i = 0; i < msg_streams.length; i++) {
      var s1 = msg_streams[i];
      var s = JSON.parse(JSON.stringify(s1));
      s.vname = Erlyvideo.vname(s.name);
      s.play_name = s.name;
      s.lifetime = Erlyvideo.format_seconds(s.lifetime / 1000);

      s.ts_delay = s.ts_delay < 5000 ? 0 :  Erlyvideo.format_seconds(Math.round(s.ts_delay / 1000));
      if(type == "files") {
        s.ts_delay = 0;
        s.hds = s.hls = s.rtmp = true;
        total_file += s.client_count;
      }
      if(!Erlyvideo.current_streams[type][s.name]) {
        Erlyvideo.current_streams[type][s.name] = s1;
        s.hds = s.hds ? "visible" : "hidden";
        s.hls = s.hls ? "visible" : "hidden";
        s.dvr = s.dvr ? "visible" : "hidden";
        s.rtmp = s.rtmp ? "visible" : "hidden";
        $("#stream-list").append(Mustache.to_html(Erlyvideo.stream_template, s));
      } else {
        var s_ = $("#stream-"+s.vname);
        s_.find(".client_count").html(s.client_count);
        s_.find(".lifetime").html(s.lifetime);
        if(s.ts_delay >= 0) s_.find(".ts_delay").html(s.ts_delay);
        if(s.retry_count >= 0) s_.find(".retry_count").html(s.retry_count);
        s_.find(".s-hds").css('visibility', s.hds ? "visible" : "hidden");
        s_.find(".s-hls").css('visibility', s.hls ? "visible" : "hidden");
        s_.find(".s-dvr").css('visibility', s.dvr ? "visible" : "hidden");
        s_.find(".s-rtmp").css('visibility', s.rtmp ? "visible" : "hidden");
      }
      new_streams[s.name] = s;
      total += s.client_count;
    }

    for(var k in Erlyvideo.current_streams[type]) {
      if(!new_streams[k]) {
        delete Erlyvideo.current_streams[type][k];
        $("#stream-"+Erlyvideo.vname(k)).remove();
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


// Show stream clients list

  show_clients: function(name) {
    $("#clients-"+Erlyvideo.vname(name)).show();
    Erlyvideo.request("sessions?name="+name);
    Erlyvideo.session_load_timer = setTimeout(function() { Erlyvideo.show_clients(name); }, 2000);
  },

  hide_clients: function(vname) {
    Erlyvideo.stop_periodic_session_loader();
    $("#clients-"+vname).hide();
  },

  stop_periodic_session_loader: function() {
    if(Erlyvideo.session_load_timer) clearTimeout(Erlyvideo.session_load_timer);
    Erlyvideo.session_load_timer = undefined;
  },


  draw_clients: function(message) {
    var name = message.name;
    var sessions = message.sessions;
    var new_sessions = {};
    var vname = Erlyvideo.vname(name);
    var list = $("#clients-list-"+vname);
    for(var i = 0; i < sessions.length; i++) {
      if(sessions[i].name == name) {
        new_sessions[sessions[i].id] = true;
        sessions[i].duration = Erlyvideo.format_seconds(sessions[i].duration / 1000);
        var h = $("#session-"+sessions[i].id);
        if(h.length > 0) {
          h.find(".duration").html(sessions[i].duration);
        } else {
          list.append(Mustache.to_html(Erlyvideo.session_template, sessions[i]));
        }
      }
    }
    list.find("tr").each(function() {
      if(!new_sessions[$(this).attr('data-id')]) {
        $(this).remove();
      }
    })
  },

  format_seconds: function(sec) {
    if(! (sec > 0)) {
      return 0;
    }

    sec = Math.round(sec);
    var lt = sec;
    var pad = function(number, width) {
      var input = number + "";  // make sure it's a string
      return("00000000000000000000".slice(0, width - input.length) + input);
    }
    var lt_s = pad(lt % 60, 2); lt = Math.floor(lt / 60);
    if(lt > 0) {
      lt_s = pad(lt % 60,2) + ":" + lt_s;
      lt = Math.floor(lt / 60);
    }
    if(lt > 0) {
      lt_s = pad(lt % 24,2) + ":"  + lt_s;
      lt = Math.floor(lt / 24);
    }
    if(lt > 0) {
      lt_s = lt + "d " + lt_s;
    }
    return lt_s;
  },

  send_logs: function() {
    $("#upload-ticket").show().addClass("loading").html("&nbsp;");
    var text = $("#upload-btn").html();
    $("#upload-btn").html("Uploading logs");
    $.ajax({
      type: 'POST',
      url: "/erlyvideo/api/sendlogs",
      dataType: 'json',

      success: function(reply) {
        var ticket = reply.ticket;
        $("#upload-ticket").removeClass("loading").html(ticket);
        $("#upload-btn").html(text);
      },
      error: function(xhr, reply) {
        var error_text = "Error";
        if(reply.error) error_text = "Error: "+reply.error;
        $("#upload-ticket").removeClass("loading").html(error_text);
        $("#upload-btn").html(text);
      }
    });
  },

  
// Play tab

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
    $("#player-embed").html("<div>Loading file from server</div>");
    $("#block-login").dialog('open');

    var play = function(info) {
      if(info.width && info.height) {
        while(info.width < 640) {
          info.width = Math.round(info.width*1.5);
          info.height = Math.round(info.height*1.5);
        }
        while(info.width > 1024) {
          info.width = Math.round(info.width / 2);
          info.height = Math.round(info.height / 2);
        }
      }

      if(player == "hds") {
        if(false && Erlyvideo.current_streams[stream] && Erlyvideo.current_streams[stream].dvr && stream.indexOf("archive") == -1) {
          var t = Math.round((new Date()) / 1000) - 15*60;
          stream = stream+"/archive/"+t+"/now/manifest.f4m";
        }
        Erlyvideo.osmf_player("player-embed", stream, info);
      } else if(player == "rtmp") {
        Erlyvideo.jwplayer("player-embed", stream, info);
      } else if(player == "hls") {
        if(stream.indexOf("m3u8") == -1) stream = stream+"/index.m3u8";
        if(window.navigator.userAgent.indexOf("Firefox") != -1 || window.navigator.userAgent.indexOf("Chrome") != -1) {
          Erlyvideo.osmf_player("player-embed", stream, info);
        } else {
          Erlyvideo.hls("#player-embed", stream, info);
        }
      }
    }


    var stream_ = stream[0] == "/" ? stream : "/" + stream;
    $.ajax({
      type: 'GET',
      url: "/erlyvideo/api/media_info"+stream_,
      dataType: 'json',

      success: play,
      error: function(xhr, reply) {
        play({});
      }
    });

  },
  


///////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//
//
//
//
//
//
//
//
//
// Statistics tab
  
  
  load_pulse: function() {
    Erlyvideo.request("pulse");
    Erlyvideo.pulse_load_timer = setTimeout(Erlyvideo.load_pulse, 2000);
  },

  format_pulse_minutes: function(t) {
    var d = new Date(t*1000);
    var h = "" + d.getHours();
    if(h.length < 2) h = "0" + h;
    var m = "" + d.getMinutes();
    if(m.length < 2) m = "0" + m;
    return h + ":" + m;
  },


  format_pulse_seconds: function(t) {
    var d = new Date(t*1000);
    var h = "" + d.getHours();
    if(h.length < 2) h = "0" + h;
    var m = "" + d.getMinutes();
    if(m.length < 2) m = "0" + m;
    var s = "" + d.getSeconds();
    if(s.length < 2) s = "0" + s;
    return h + ":" + m + ":" + s;
  },

  draw_pulse_traffic: function(message) {
    var i,j;

    var txtattr = { font: "12px sans-serif" };
    var lineattr = { nostroke: false, axis: "0 0 1 1", colors: ["#995555", "#555599"]};

    for(i = 0; i < message.interfaces.length; i++) {
      var iface = message.interfaces[i];

      if(iface.iface == "lo") continue;

      if($("#stat-"+iface.iface).length == 0) {
        $("#pulse-stats").append("<div id='stat-"+iface.iface+"' style='height: 280px; width: 900px; margin-bottom: 30px'></div>");
      }


      var d = $("#stat-"+iface.iface)[0];
      if(!d.r) {
        d.r = Raphael("stat-"+iface.iface);
        d.r.text(160, 10, "Traffic for last hour on "+iface.iface+" in kbit/s").attr(txtattr);
        d.r.text(600, 10, "Traffic for last minute on "+iface.iface+" in kbit/s").attr(txtattr);      


        d.r.path("M40 260 L70 260 z").attr('stroke', "#995555");
        d.r.text(90, 260, "Input").attr(txtattr).attr('fill', "#995555");
        d.r.path("M120 260 L150 260 z").attr('stroke', "#555599");
        d.r.text(170, 260, "Output").attr(txtattr).attr('fill', "#555599");
      }

      var times = [];
      var in_ = [];
      var out_ = [];
      for(j = 0; j < iface.hour.length; j++) {
        // times.push(Erlyvideo.format_pulse_seconds(iface.hour[j].time));
        times.push(iface.hour[j].time);
        in_.push(iface.hour[j].input);
        out_.push(iface.hour[j].output == iface.hour[j].input ? iface.hour[j].output - 1 : iface.hour[j].output);
      }
      if(d.hour_graph) d.hour_graph.remove();
      if(times.length > 10) {
        d.hour_graph = d.r.linechart(40, 10, 400, 220, [times,times], [in_, out_], lineattr);
        var k;
        var labels = d.hour_graph.axis[0].text.items;
        for(k = 0; k < labels.length; k++) {
          labels[k].attr({'text' : Erlyvideo.format_pulse_minutes(labels[k].attr('text'))});
        }
      }

      var times = [];
      var in_ = [];
      var out_ = [];
      for(j = 0; j < iface.minute.length; j++) {
        // times.push(Erlyvideo.format_pulse_seconds(iface.hour[j].time));
        times.push(iface.minute[j].time);
        in_.push(iface.minute[j].input);
        out_.push(iface.minute[j].output == iface.minute[j].input ? iface.minute[j].output - 1 : iface.minute[j].output);
      }
      if(d.minute_graph) d.minute_graph.remove();
      d.minute_graph = d.r.linechart(490, 10, 400, 220, [times,times], [in_, out_], lineattr);

      var k;
      var labels = d.minute_graph.axis[0].text.items;
      for(k = 0; k < labels.length; k++) {
        labels[k].attr({'text' : Erlyvideo.format_pulse_seconds(labels[k].attr('text'))});
      }

    }




    if($("#stat-file").length == 0) {
      $("#pulse-stats").append("<div id='stat-file' style='height: 280px; width: 900px; margin-bottom: 30px'></div>");      
    }
    var d_file = $("#stat-file")[0];
    if(!d_file.r) {
      d_file.r = Raphael("stat-file");
      d_file.r.text(160, 10, "Disk read time for last hour in % from duration").attr(txtattr);
      d_file.r.text(600, 10, "Disk read time for last minute in % from duration").attr(txtattr);
    }
    var times = [];
    var traf = [];
    for(j = 0; j < message.file.hour.length; j++) {
      times.push(message.file.hour[j].time);
      traf.push(message.file.hour[j].disk);
    }
    if(d_file.hour_graph) d_file.hour_graph.remove();
    d_file.hour_graph = d_file.r.linechart(40, 10, 400, 220, [times], [traf], lineattr);

    var labels = d_file.hour_graph.axis[0].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : Erlyvideo.format_pulse_minutes(labels[k].attr('text'))});
    }

    var labels = d_file.hour_graph.axis[1].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : labels[k].attr('text') + "%"});
    }


    var times = [];
    var traf = [];
    for(j = 0; j < message.file.minute.length; j++) {
      times.push(message.file.minute[j].time);
      traf.push(message.file.minute[j].disk);
    }
    if(d_file.minute_graph) d_file.minute_graph.remove();
    d_file.minute_graph = d_file.r.linechart(490, 10, 400, 220, [times], [traf], lineattr);

    var labels = d_file.minute_graph.axis[0].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : Erlyvideo.format_pulse_seconds(labels[k].attr('text'))});
    }

    var labels = d_file.minute_graph.axis[1].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : labels[k].attr('text') + "%"});
    }





    if($("#stat-segment").length == 0) {
      $("#pulse-stats").append("<div id='stat-segment' style='height: 280px; width: 900px; margin-bottom: 30px'></div>");      
    }
    var d_seg = $("#stat-segment")[0];
    if(!d_seg.r) {
      d_seg.r = Raphael("stat-segment");
      d_seg.r.text(160, 10, "Segment read time for last hour in % from duration").attr(txtattr);
      d_seg.r.text(600, 10, "Segment read time for last minute in % from duration").attr(txtattr);
    }
    var times = [];
    var traf = [];
    for(j = 0; j < message.file.hour.length; j++) {
      times.push(message.file.hour[j].time);
      traf.push(message.file.hour[j].segment);
    }
    if(d_seg.hour_graph) d_seg.hour_graph.remove();
    d_seg.hour_graph = d_seg.r.linechart(40, 10, 400, 220, [times], [traf], lineattr);

    var labels = d_seg.hour_graph.axis[0].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : Erlyvideo.format_pulse_minutes(labels[k].attr('text'))});
    }

    var labels = d_seg.hour_graph.axis[1].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : labels[k].attr('text') + "%"});
    }



    var times = [];
    var traf = [];
    for(j = 0; j < message.file.minute.length; j++) {
      times.push(message.file.minute[j].time);
      traf.push(message.file.minute[j].segment);
    }
    if(d_seg.minute_graph) d_seg.minute_graph.remove();
    d_seg.minute_graph = d_seg.r.linechart(490, 10, 400, 220, [times], [traf], lineattr);

    var labels = d_seg.minute_graph.axis[0].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : Erlyvideo.format_pulse_seconds(labels[k].attr('text'))});
    }

    var labels = d_seg.minute_graph.axis[1].text.items;
    for(k = 0; k < labels.length; k++) {
      labels[k].attr({'text' : labels[k].attr('text') + "%"});
    }





  },

  stop_periodic_pulse_loader: function() {
    if(Erlyvideo.pulse_load_timer) clearTimeout(Erlyvideo.pulse_load_timer);
    Erlyvideo.pulse_load_timer = undefined;
  },

  last_function: function() {}

};

$.mustache = function(template, view, partials) {
  return Mustache.to_html(template, view, partials);
};




$(function() {
  var params = parseQueryString();
  // Erlyvideo.comet = $.bullet("ws://"+window.location.host + "/comet");
  // Erlyvideo.comet.onmessage = Erlyvideo.on_message;
  
  Erlyvideo.enable_tabs();
  // Erlyvideo.enable_licenses();
  if(window.location.hash != "" && window.location.hash != "support") {
    Erlyvideo.activate_tab(window.location.hash.substring(1));
  } else if(params["file"] && params["file"].length > 0) {
    Erlyvideo.activate_tab("play");
    $("#requested-stream-name").val(params["file"]);
  } else {
    Erlyvideo.activate_tab("streams");
  }
  Erlyvideo.enable_play_tab();
  Erlyvideo.request("server");

  if(params["player"] == "grind") Erlyvideo.hds_player = "GrindPlayer";


  // $('#traffic-stats').visualize({type: 'line', width: '800px'});
	
  $("#block-login").dialog({autoOpen:false, title : "Play Stream", width: 840, height: 700});
  $("#block-login").bind("dialogclose", function() {
    $("#block-login .content").html('<div id="player-embed"></div>');
  });
  
})

})(jQuery);