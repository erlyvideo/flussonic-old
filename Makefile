# include version.mk
VERSION := $(shell grep vsn apps/flussonic/src/flussonic.app.src | ruby -e 'puts STDIN.read[/\"([0-9\.]+)\"/, 1]')
NODENAME ?= flussonic

all: app

app: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

run:
	ERL_LIBS=apps:deps erl +K true +A 16 +a 2048 -name $(NODENAME)@127.0.0.1 -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -s flussonic -sasl errlog_type error

shell:
	erl -name debug@127.0.0.1 -remsh flussonic@127.0.0.1

opensource:
	git archive --prefix=flussonic-$(VERSION)/ master |gzip > flussonic-$(VERSION).tgz

start:
	mkdir -p log/pipe
	run_erl -daemon log/pipe/ log/ "exec make run"
	while [ ! -e log/flussonic.pid ] ; do sleep 1; echo "."; done
	# echo `ps axuww |grep beam.smp| grep "sname flussonic" | head -1 | awk '{print $$2}'` > log/flussonic.pid
	cat log/flussonic.pid

attach:
	to_erl log/pipe/

stop:
	echo -e "init:stop().\n" > log/pipe/erlang.pipe.1.w
	kill `cat log/flussonic.pid`

bench:
	ERL_LIBS=apps:deps erl +K true +A 16 -sname fluss_bench -pa apps/*/ebin -boot start_sasl -s http_bench -sasl errlog_type error

dist-clean: clean

release:
	rm -rf flussonic-$(VERSION)
	mkdir flussonic-$(VERSION)
	./contrib/escriptize
	mv flussonic flussonic-$(VERSION)/
	mkdir flussonic-$(VERSION)/priv/
	cp -r priv/*.conf priv/bunny.mp4 priv/runit priv/*.ru priv/*.rb flussonic-$(VERSION)/priv/
	cp README.md flussonic-$(VERSION)/README.md

package:
	rm -rf tmproot
	mkdir -p tmproot/opt/flussonic
	git archive master | (cd tmproot/opt/flussonic; tar xv)
	(cd tmproot/opt/flussonic/ && make)
	rm -rf tmproot/opt/flussonic/deps/proper*
	find tmproot -type d -name .git -delete 2>/dev/null
	mkdir -p tmproot/etc/init.d/
	cp priv/flussonic tmproot/etc/init.d/
	mkdir -p tmproot/etc/flussonic
	cp priv/sample/*.conf tmproot/etc/flussonic/
	cd tmproot && \
	fpm -s dir -t deb -n flussonic -v $(VERSION) --config-files /etc/flussonic/flussonic.conf --config-files /etc/flussonic/streams.conf --config-files '/etc/flussonic/*.conf' -d 'esl-erlang (>= 15) | erlang (>= 1:15)' --post-install opt/flussonic/priv/postinst -m "Max Lapshin <max@maxidoors.ru>" -a amd64 etc/init.d/flussonic etc/flussonic opt 
	mv tmproot/*.deb .
	rm -rf tmproot


archive: release
	tar zcvf flussonic-$(VERSION).tar.gz flussonic-$(VERSION)
	rm -rf flussonic-$(VERSION)

upload:
	./contrib/license_pack $(VERSION)
	scp *$(VERSION)* root@erlyvideo.tv:/vz/erlyhub/apps/erlyvideo/debian/public/binary
	ssh erlyhub@erlyvideo.org "cd /apps/erlyvideo/debian ; ./update ; cd public/binary ; ln -sf flussonic-$(VERSION).tgz flussonic-latest.tgz "
	@#echo "Erlyvideo version ${VERSION} uploaded to debian repo http://debian.erlyvideo.org/ ." | mail -r "Erlybuild <build@erlyvideo.org>" -s "Erlyvideo version ${VERSION}" -v erlyvideo-dev@googlegroups.com

new_version: opensource package upload

