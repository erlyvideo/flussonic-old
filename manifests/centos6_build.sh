#!/bin/sh

yum install -y http://mirror.yandex.ru/epel/6/x86_64/epel-release-6-7.noarch.rpm

yum install -y mock rpm-build


rpmbuild
mkdir -p /root/rpmbuild/SPECS /root/rpmbuild/SOURCES
cp /vagrant/flussonic.spec /root/rpmbuild/SPECS
cp /vagrant/flussonic-*.tgz /root/rpmbuild/SOURCES
cd /root
rpmbuild -bs rpmbuild/SPECS/flussonic.spec
usermod  -a -G mock root
usermod  -a -G mock vagrant
/usr/bin/mock -v /root/rpmbuild/SRPMS/flussonic-*.src.rpm