Flussonic
=========

Flussonic is a HTTP streaming video solution, former erlyvideo.


Installation
------------

* install erlang from https://www.erlang-solutions.com/downloads/download-erlang-otp

```
wget -q -O - http://debian.erlyvideo.org/binary/gpg.key | apt-key add -
echo "deb http://debian.erlyvideo.org binary/" >> /etc/apt/sources.list
apt-get update
apt-get -y install flussonic
```

* edit /etc/flussonic/flussonic.conf for your needs
* read manual in config how to work with it
* open http://localhost:8080/index.html


Environment options
-------------------


set `NODENAME` to anything else to be able to launch second flussonic on the same machine
set `FLU_CONFIG` to point at proper config file


Extended usage
--------------

Write on info@erlyvideo.org for trial license key for additional modules.

