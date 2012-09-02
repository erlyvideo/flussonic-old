backend www {
	.host = "127.0.0.1";
	.port = "8080";
}

sub vcl_recv {
	set req.backend = www;
	
	if (req.url ~ "^/flu" || req.url ~ "\.ts$" || req.url ~ "Seg(\d+)-Frag(\d+)$") {
		unset req.http.cookie;
	}
	
	if (req.url ~ "(\.m3u8|\.f4m|bootstrap)$") {
		set req.hash_always_miss = true;
	}
	
	return (lookup);
}


sub vcl_fetch {
	if (req.url ~ "^/flu" || req.url ~ "\.ts$" || req.url ~ "Seg(\d+)-Frag(\d+)$") {
		unset beresp.http.set-cookie;
	}
	
}