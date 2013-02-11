#!/bin/sh


# echo "deb http://debian.erlyvideo.org/ binary/" > /etc/apt/sources.list.d/erlang.list


# wget -q -O - http://debian.erlyvideo.org/binary/gpg.key |apt-key add -

# apt-get update >/dev/null
# apt-get -y install esl-erlang-nox make git checkinstall > /dev/null

# SOURCE=`ls /vagrant/*.tgz | sort -r |head -1`

# tar zxvf $SOURCE
# cd flussonic-*
# make
# sudo checkinstall -y --maintainer="info@erlyvideo.org" --pkgarch=all --requires='erlang-nox (>= 1:15) | esl-erlang (>= 15) | esl-erlang-nox (>= 15)'

# mv flussonic_*.deb /vagrant/
