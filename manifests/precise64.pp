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

include erlang
