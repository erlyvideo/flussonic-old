#!/bin/sh


rpmbuild
mkdir -p /root/rpmbuild/SPECS /root/rpmbuild/SOURCES
cp /vagrant/manifests/flussonic.spec /root/rpmbuild/SPECS
cp /vagrant/flussonic-*.tgz /root/rpmbuild/SOURCES
cd /root
rpmbuild -bs rpmbuild/SPECS/flussonic.spec
usermod  -a -G mock root
usermod  -a -G mock vagrant
/usr/bin/mock -v /root/rpmbuild/SRPMS/flussonic-*.src.rpm