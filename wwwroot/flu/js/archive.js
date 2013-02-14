(function($) {
  
  var draw_hour_thumbnails = function(div, hour, camera, opts) {
    var i;
    var minutes = {};
    var path = camera + "/" + hour;
    var count = 60;
    
    for(i = 0; i < 60; i++) {
    	$.getJSON("/erlyvideo/api/dvr_previews/"+hour +"/" + i + "/"+camera, {}, function(reply) {
    	  var s = '<div class="line">'+reply.minute;
    	  var j;
    	  
    	  if(reply.keyframes.length > 10) {
    	    var prev = 0;
    	    var kf = [];
    	    for(j = 0; j < reply.keyframes.length; j++) {
    	      if(reply.keyframes[j].dts - prev > 5000) {
    	        prev = reply.keyframes[j].dts;
    	        kf[kf.length] = reply.keyframes[j];
    	      }
    	    }
    	    reply.keyframes = kf;
    	  }
    	  for(j = 0; j < reply.keyframes.length; j++) {
          s += '<img title="' + reply.keyframes[j].path+'" src="'+reply.keyframes[j].path +'"/>'
    	  }
  	    s += "</div>";
    		minutes[reply.minute] = s;
    		count--;
    		if(count == 0) {
    		  var h = '<a href="#" onclick="$(\'.dvr_archive\').toggle(); $(\'.hour_thumbnails\').html(\'\'); return false;">Close</a><br>\n';
    		  for(j = 0; j < 60; j++) {
    		    h += minutes[j];
    		  }
    		  $(div).find('.dvr_archive').toggle();
      	  $(div).find('.hour_thumbnails').html(h);
    		}
    	});
      
    }
  }
  
  var draw_dvr_status = function(div, year, month, day, camera, reply, opts) {
  	var i,j;
  	
  	var minutes = {};
  	for(j = 0; j < 24; j++) {
  		minutes[j] = {};
  	}
  	for(i = 0; i < reply.length; i++) {
  		var matches = reply[i].path.match(/\d{4}\/\d+\/\d+\/(\d+)\/(\d+)/);
  		var h = parseInt(matches[1], 10);
  		var m = parseInt(matches[2], 10);
  		minutes[h][m] = true;
  	}


  	s = "";
  	s += '<div class="line"><div class="first-col">&nbsp;</div>';
  	for(i = 0; i < 60; i++) {
  		klass = "";
  		if((i + 1) % 10 == 0) klass += " ten";
  		else if((i + 1) % 5 == 0) klass += " five";
  		s += '<div class="'+klass+'">'+i+'</div>';
  	}
  	s += "</div>";
  	for(j = 0; j < 24; j++) {
  		s += '<div class="line"><div class="first-col hour">'+ 
  		  '<a href="#" path="'+year+"/"+month+"/"+day+"/"+ j+'" class="hour-click">'+	j+'</a></div>';
  		for(i = 0; i < 60; i++) {
  			var klass = minutes[j][i] ? "ok" : "fail";
  			if((i + 1) % 10 == 0) klass += " ten";
  			else if((i + 1) % 5 == 0) klass += " five";
  			var d = new Date(year, month - 1, day, j, i, 0);
  			s += "<div class=\"minute "+klass+"\" time='"+(d.getTime() / 1000 - d.getTimezoneOffset()*60)+"'>&nbsp;</div>";
  		}
  		s += "</div>";
  	}
  	$(div).find(".dvr_archive").html(s);
  	var selection_start = null;
  	$(div).find("div.ok").mousedown(function() {
  		selection_start = parseInt($(this).attr('time'), 10);
  	});
  	$(div).find(".hour-click").click(function() {
  	  // draw_hour_thumbnails(div, $(this).attr('path'), camera, opts);
  	  return false;
  	});
  	$(div).find("div.minute").mouseup(function() {
  		if(selection_start > 0) {
  		  var play_name = opts.play_name || camera;
  			var selection_stop = parseInt($(this).attr('time'), 10);
  			$(div).find(".dvr-export-url").attr("href", "/"+play_name+"/archive-"+selection_start+"-"+(selection_stop - selection_start)+".mp4").html("Export camera mp4");
  			$(div).find(".dvr-view-url").attr("href", "/"+play_name+"/archive/"+selection_start+"/"+(selection_stop - selection_start)).html("View archive");
  			$(div).find(".dvr-hls-url").attr("href", "/"+play_name+"/index-"+selection_start+"-"+(selection_stop - selection_start)+".m3u8").html("HLS");
  			$(div).find(".dvr-info-line").show();
  			selection_start = null;
  		}
  	});
  }


  var request_dvr_status = function(id, year, month, day, camera, opts) {
  	$.getJSON("/erlyvideo/api/dvr_status/"+year+"/"+month+"/"+day+"/"+camera, {}, function(reply) {
  		draw_dvr_status(id, year, month, day, camera, reply, opts);
  	});
  }

  var load_dvr = function(id, name, date, opts) {
    var year, month, day;
  	var p = date.split("/");
  	year = p[0];
  	month = p[1];
  	day = p[2];
	
    request_dvr_status(id, year, month, day, name, opts);
  }

  
  $.fn.showDVR = function(camera, options) {
    var defaults = {
      server : window.location.host,
      dvr_template: "<div class='dvr_archive_block' class='inner'> \
      <div class='dvr-info-line'> \
        <a href='#' onclick=\"$(this).parent().parent().remove(); return false\">Close</a>\
      	<a class='dvr-export-url' href=''>Export archive</a> \
      	<a class='dvr-view-url' href='' onclick='Erlyvideo.play_stream($(this).attr(\"href\"), \"hds\"); return false;'>View archive</a> \
      	<a class='dvr-hls-url' href='' onclick='Erlyvideo.play_stream($(this).attr(\"href\"), \"hls\"); return false;'>HLS</a> \
      	<input type='text' class='dvr-view-date' /> \
      </div> \
      <div class='dvr_archive'></div> \
      <div class='hour_thumbnails'></div> \
      </div>",
      play_function: function() {
        return function(text, render) {return "";}
      },
      handler: {}
    };
    var opts = $.extend(defaults, options);
  
    var $this = $(this);
    
    var show_dvr_status = function(div, name) {
      div.html(Mustache.to_html(opts.dvr_template, {}));
      var day = opts.day;
      // var day = params["day"];
    	if(!day) {
    		var t = new Date();
    		day = "" + (t.getYear() + 1900) +"/" +(t.getMonth() + 1) + "/" + t.getDate();
    	}
    	div.find(".dvr-view-date").val(day).simpleDatepicker().bind('dateChanged', function() {
    	  load_dvr(div, name, $(this).val(), opts);
    	});
    	load_dvr(div, name, day, opts);
    }
    opts.camera = camera;
  
    show_dvr_status($this, camera);

  }
})(jQuery);