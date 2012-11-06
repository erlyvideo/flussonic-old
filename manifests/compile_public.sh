#!/bin/sh



if [ ! -f /etc/apt/sources.list.d/backports.list ] ; then
  echo "deb http://backports.debian.org/debian-backports squeeze-backports main contrib non-free" > /etc/apt/sources.list.d/backports.list
fi


if [ ! -f /usr/bin/erl ] ; then
  apt-get update
  apt-get -y install erlang-nox git build-essential
fi


if [ ! -e /root/flu-cache ] ; then
  git clone git://github.com/erlyvideo/flussonic /root/flu-cache
else
  cd /root/flu-cache
  git pull
fi
cd /root/flu-cache
./rebar get-deps update-deps


rm -rf /root/flussonic
git clone /root/flu-cache /root/flussonic
cd /root/flussonic
cp -r /root/flu-cache/deps deps
make






