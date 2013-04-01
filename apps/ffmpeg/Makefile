VERSION := $(shell ./priv/version.erl)

all:
	ERL_LIBS=..:../../deps ./rebar compile

test:
	ERL_LIBS=..:../../deps ./rebar -v compile eunit

clean:
	ERL_LIBS=..:../../deps ./rebar clean

package:
	rm -rf tmproot
	mkdir -p tmproot/opt/flussonic/apps/ffmpeg/priv
	ldd priv/thumbnailer |grep opt/flussonic | awk '{print $$3}' | while read p; do cp $$p tmproot/opt/flussonic/apps/ffmpeg/priv/ ; done
	cp -a /opt/flussonic/apps/ffmpeg/share tmproot/opt/flussonic/apps/ffmpeg/
	git archive master | (cd tmproot/opt/flussonic/apps/ffmpeg; tar x)
	ROOT=`pwd`/../.. && (cd tmproot/opt/flussonic/apps/ffmpeg; ERL_LIBS=$$ROOT/apps:$$ROOT/deps ./rebar compile)
	cd tmproot && \
	fpm -s dir -t deb -n flussonic-ffmpeg -v $(VERSION) --category net \
	 -m "Max Lapshin <max@maxidoors.ru>" -a amd64 opt 
	mv tmproot/*.deb .
	# rm -rf tmproot


.PHONY: test

