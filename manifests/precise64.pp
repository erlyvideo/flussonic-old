class erlang {
  file { "erlang.list":
    path => "/etc/apt/sources.list.d/esl-erlang.list",
    ensure => present,
    owner => root,
    # content => 'deb http://binaries.erlang-solutions.com/debian precise contrib';
    content => 'deb http://debian.erlyvideo.org/ binary/';
  }
 exec { "import-key":
   # command     => "/usr/bin/wget -q -O - http://binaries.erlang-solutions.com/debian/erlang_solutions.asc |/usr/bin/apt-key add -",
   # unless      => "/usr/bin/apt-key list|/bin/grep -c erlang-solutions";
   command     => "/usr/bin/wget -q -O - http://debian.erlyvideo.org/binary/gpg.key |/usr/bin/apt-key add -",
   unless      => "/usr/bin/apt-key list|/bin/grep -c maxidoors";
 }
 exec { "apt-update":
   command     => "/usr/bin/apt-get update",
   #refreshonly => true,
   require => [ Exec['import-key'], File["erlang.list"] ];
 }
 package {  "erlang":
  name => "esl-erlang-nox",
# name => "esl-erlang",
  ensure => installed,
  require => Exec['apt-update'];
 }
}

class varnish {
  exec {"varnish_conf":
    command => "/bin/mkdir -p /etc/varnish"
  }
  file {"varnish.vcl":
    path => "/etc/varnish/default.vcl",
    ensure => present,
    owner => root,
    source => "/vagrant/priv/sample/varnish.vcl",
    require => Exec["varnish_conf"]
  }

  package { "varnish":
    name => "varnish",
    ensure => installed,
    require => File["varnish.vcl"]
  }
}


class flussonic_conf {
  exec {"storage":
    command => "/bin/mkdir -p /storage"
  }
  file {"movie":
    path => "/storage/bunny.mp4",
    source => "/vagrant/priv/bunny.mp4",
    ensure => present,
    require => Exec["storage"]
  }

  file {"playlist":
    path => "/storage/playlist.txt",
    ensure => present,
    content => "/storage/bunny.mp4
    ",
    require => Exec["storage"]
  }

  exec {"flu_conf":
    command => "/bin/mkdir -p /etc/flussonic"
  }

  file {"config":
    path => "/etc/flussonic/flussonic1.conf",
    ensure => present,
    content => "{http,8080}.
    {stream, \"channel\", \"playlist:///storage/playlist.txt\", [{sessions, \"http://127.0.0.1:8080/\"}]}.
    {file, \"vod\", \"/storage\", [{sessions, \"http://127.0.0.1:8080/\"}]}.
    api.
    {root, \"wwwroot\"}.
    ",
    require => [ File["movie"], File["playlist"], Exec["flu_conf"] ]
  }
}

include erlang
include varnish
include flussonic_conf
