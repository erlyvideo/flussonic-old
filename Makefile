# include version.mk
VERSION := $(shell ./contrib/version.erl)
NODENAME ?= flussonic

all: app


install:
	mkdir -p $(DESTDIR)/usr/share/flussonic/deps $(DESTDIR)/etc/init.d/ $(DESTDIR)/etc/default
	cp -r apps deps wwwroot $(DESTDIR)/usr/share/flussonic
	cp priv/flussonic $(DESTDIR)/etc/init.d/
	echo "FLUDIR=/usr/share/flussonic" >> $(DESTDIR)/etc/default/flussonic
	mkdir -p $(DESTDIR)/usr/share/doc/flussonic/ $(DESTDIR)/etc/flussonic/
	cp COPYING $(DESTDIR)/usr/share/doc/flussonic/copyright
	cp priv/sample/flussonic.conf $(DESTDIR)/etc/flussonic/flussonic.conf



test:
	./rebar eunit skip_deps=true

app: deps/cowboy deps/lager/ebin/lager_transform.beam
	./rebar compile skip_deps=true

deps/cowboy:
	./rebar get-deps compile

deps/lager/ebin/lager_transform.beam:
	./rebar compile

clean:
	./rebar clean
	rm -f erl_crash.dump

compile_public:
	vagrant destroy -f compile_public
	vagrant up compile_public

run:
	ERL_LIBS=apps:deps erl +K true +A 16 +a 2048 -name flussonic@127.0.0.1 -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -s flussonic -sasl errlog_type error

shell:
	erl -name debug@127.0.0.1 -remsh flussonic@127.0.0.1


vagrant:
	vagrant destroy -f
	vagrant up
	vagrant ssh -c "sudo -s /etc/init.d/flussonic start"


.PHONY: test

# check_public:
# 	vagrant destroy -f
	

start:
	mkdir -p log/pipe log/console
	export RUN_ERL_LOG_GENERATIONS=5
	export RUN_ERL_LOG_MAXSIZE=100000
	run_erl -daemon log/pipe/ log/console/ "exec make run"
	while [ ! -e log/flussonic.pid ] ; do sleep 1; echo "."; done

attach:
	to_erl log/pipe/

stop:
	echo -e "init:stop().\n" > log/pipe/erlang.pipe.1.w
	kill `cat log/flussonic.pid`


dist-clean: clean

tgz:
	rm -rf flussonic-$(VERSION)
	git archive --prefix=flussonic-$(VERSION)/ master | tar x
	mkdir -p flussonic-$(VERSION)/deps
	[ -d deps ] && for d in deps/* ; do git clone $$d flussonic-$(VERSION)/deps/`basename $$d`; done || true
	cd flussonic-$(VERSION) && ./rebar get-deps
	cp -f contrib/Makefile.debian flussonic-$(VERSION)/Makefile
	perl -pi -e s,vsn_subst,$(VERSION),g flussonic-$(VERSION)/Makefile
	rm -f flussonic-$(VERSION)/deps/mimetypes/src/mimetypes_parse.erl
	find flussonic-$(VERSION) -name *.beam -delete
	find flussonic-$(VERSION) -name *.so -delete
	find flussonic-$(VERSION) -name *.o -delete
	find flussonic-$(VERSION) -name *.app.src -exec perl -pi -e s,git,'"v1.0"',g {} \;
	find flussonic-$(VERSION) -name .gitignore -delete
	cat rebar.config |grep -v meck > flussonic-$(VERSION)/rebar.config
	rm -rf flussonic-$(VERSION)/deps/meck
	rm -rf flussonic-$(VERSION)/deps/cowboy/test
	rm -rf flussonic-$(VERSION)/deps/cowboy/examples
	rm -rf flussonic-$(VERSION)/deps/*/.git
	rm -rf flussonic-$(VERSION)/apps/rtsp/priv
	rm -rf flussonic-$(VERSION)/deps/lager/rebar
	rm -rf flussonic-$(VERSION)/apps/flussonic/c_src
	rm -f flussonic-$(VERSION)/apps/mpegts/contrib/build_table.rb
	rm -f flussonic-$(VERSION)/apps/flussonic/mibs-unused/ERLYVIDEO-MIB.mib
	tar zcf flussonic-$(VERSION).tgz flussonic-$(VERSION)
	rm -rf flussonic-$(VERSION)

package:
	rm -rf tmproot
	mkdir -p tmproot/opt/flussonic
	git archive master | (cd tmproot/opt/flussonic; tar x)
	mkdir -p tmproot/opt/flussonic/deps
	rm -rf tmproot/opt/flussonic/priv/mbr*.mp4
	rm -rf tmproot/opt/flussonic/test
	[ -d deps ] && for d in deps/* ; do git clone $$d tmproot/opt/flussonic/deps/`basename $$d`; done || true
	(cd tmproot/opt/flussonic/ && ./rebar get-deps && ./rebar compile)
	mkdir -p tmproot/opt/flussonic/apps/flussonic/priv/
	cp -f priv/mmap-squeeze64.so tmproot/opt/flussonic/apps/flussonic/priv/mmap.so
	mkdir -p tmproot/opt/flussonic/apps/mpegts/priv/
	cp -f priv/mpegts_udp-squeeze64.so tmproot/opt/flussonic/apps/mpegts/priv/mpegts_udp.so
	rm -rf tmproot/opt/flussonic/deps/proper*
	rm -rf tmproot/opt/flussonic/apps/ffmpeg
	rm -rf tmproot/opt/flussonic/apps/mpegts/contrib/build_table.rb tmproot/opt/flussonic/apps/rtsp/priv/* tmproot/opt/flussonic/deps/*/test
	rm -rf tmproot/opt/flussonic/deps/*/.git tmproot/opt/flussonic/.git
	mkdir -p tmproot/usr/share/doc/flussonic
	find tmproot -name .gitignore -delete
	cp COPYING tmproot/usr/share/doc/flussonic/copyright
	mkdir -p tmproot/etc/init.d/
	cp priv/flussonic tmproot/etc/init.d/
	mkdir -p tmproot/etc/flussonic
	cp priv/sample/*.conf tmproot/etc/flussonic/
	cd tmproot && \
	fpm -s dir -t deb -n flussonic -v $(VERSION) --category net \
	--post-install ../debian/postinst --pre-uninstall ../debian/prerm --post-uninstall ../debian/postrm \
	--config-files /etc/flussonic/flussonic.conf --config-files /etc/flussonic/streams.conf --config-files '/etc/flussonic/*.conf' \
	-d 'esl-erlang (>= 15) | esl-erlang-nox (>= 15) | erlang-nox (>= 1:15)' -m "Max Lapshin <max@maxidoors.ru>" -a amd64 etc/init.d/flussonic etc/flussonic opt 
	mv tmproot/*.deb .
	rm -rf tmproot


escriptize:
	./contrib/escriptize

upload:
	./contrib/license_pack upload $(VERSION)
	scp flussonic_$(VERSION)_amd64.deb flussonic flussonic-$(VERSION).tgz erlyhub@erlyvideo.org:/apps/erlyvideo/debian/public/binary
	# scp flussonic erlyhub@erlyvideo.org:/apps/erlyvideo/debian/public/binary/flussonic
	ssh erlyhub@erlyvideo.org "cd /apps/erlyvideo/debian ; ./update ; cd public/binary ; ln -sf flussonic-$(VERSION).tgz flussonic-latest.tgz "
	./contrib/send_email.erl erlyvideo-dev@googlegroups.com $(VERSION)
	@#echo "Erlyvideo version ${VERSION} uploaded to debian repo http://debian.erlyvideo.org/ ." | mail -r "Erlybuild <build@erlyvideo.org>" -s "Erlyvideo version ${VERSION}" -v erlyvideo-dev@googlegroups.com

new_version: tgz package escriptize upload

