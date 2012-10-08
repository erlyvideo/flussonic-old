#!/bin/sh


echo "deb http://debian.erlyvideo.org/ binary/" > /etc/apt/sources.list.d/erlang.list


wget -q -O - http://debian.erlyvideo.org/binary/gpg.key |apt-key add -

apt-get update >/dev/null
apt-get -y install esl-erlang-nox git varnish nginx apache2-utils > /dev/null

mkdir -p /etc/varnish /opt /etc/flussonic /storage
cp /vagrant/priv/sample/varnish.vcl /etc/varnish/default.vcl
/etc/init.d/varnish restart


rm /etc/nginx/sites-enabled/*
cp /vagrant/priv/sample/nginx.conf /etc/nginx/sites-enabled/flussonic.conf
mkdir -p /var/log/flussonic
/etc/init.d/nginx restart

htpasswd -bc /etc/flussonic/htpasswd admin pass0


cp /vagrant/priv/bunny.mp4 /storage/
echo "/storage/bunny.mp4" > /storage/playlist.txt

cat > /etc/flussonic/flussonic.conf <<-EOF
{http,8080}.
{stream, "channel", "playlist:///storage/playlist.txt", [{sessions, "http://127.0.0.1:8080/"}]}.
{file, "vod", "/storage", [{sessions, "http://127.0.0.1:8080/"}]}.
api.
{root, "wwwroot"}.
EOF



git clone /vagrant /opt/flussonic
cd /opt/flussonic
./rebar get-deps compile >/dev/null

ln -sf /opt/flussonic/priv/flussonic /etc/init.d/flussonic
