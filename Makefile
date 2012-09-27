# include version.mk
VERSION := $(shell grep vsn apps/flussonic/src/flussonic.app.src | ruby -e 'puts STDIN.read[/\"([0-9\.]+)\"/, 1]')
NODENAME ?= flussonic
DESTDIR ?= /opt/flussonic

all: app


install: all
	mkdir -p $(DESTDIR)
	cp -r Emakefile Makefile apps contrib deps priv rebar wwwroot $(DESTDIR)
	


app: deps/cowboy
	@ERL_LIBS=apps:deps erl -make

deps/cowboy:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

run:
	ERL_LIBS=apps:deps erl +K true +A 16 +a 2048 -name $(NODENAME)@127.0.0.1 -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -s flussonic -sasl errlog_type error

shell:
	erl -name debug@127.0.0.1 -remsh flussonic@127.0.0.1


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


dist-clean: clean

opensource:
	rm -rf flussonic-$(VERSION)
	git archive --prefix=flussonic-$(VERSION)/ master | tar x
	cd flussonic-$(VERSION) && ./rebar get-deps && ./rebar compile && find . -name *.beam -delete && find . -name *.so -delete
	tar zcf flussonic-$(VERSION).tgz flussonic-$(VERSION)
	rm -rf flussonic-$(VERSION)

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


upload:
	./contrib/license_pack $(VERSION)
	scp *$(VERSION)* root@erlyvideo.tv:/vz/erlyhub/apps/erlyvideo/debian/public/binary
	ssh erlyhub@erlyvideo.org "cd /apps/erlyvideo/debian ; ./update ; cd public/binary ; ln -sf flussonic-$(VERSION).tgz flussonic-latest.tgz "
	@#echo "Erlyvideo version ${VERSION} uploaded to debian repo http://debian.erlyvideo.org/ ." | mail -r "Erlybuild <build@erlyvideo.org>" -s "Erlyvideo version ${VERSION}" -v erlyvideo-dev@googlegroups.com

new_version: opensource package upload

