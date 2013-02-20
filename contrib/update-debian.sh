#!/bin/bash


Sign() {
cd $1
echo "Origin: Erlyvideo repositories
Label: Erlyvideo
Archive: stable
Architectures: i386 armel amd64 all source
Components: net
Suite: $1
Version: 9.10
Description: Erlyvideo streaming server http://erlyvideo.org/
MD5Sum:" > Release

#ls Packages* Sources* Release | while read ln
ls $2* Release | while read ln
do
md=`md5sum $ln |awk {' print $1 '}`
sz=`du -sb $ln`
echo " $md $sz" >> Release.tmp
done
cat Release.tmp >> Release
rm -f Release.tmp

rm -f Release.gpg
gpg -sbao Release.gpg Release
cd ..
}

repo=binary
echo $repo
cd /apps/erlyvideo/debian/public
dpkg-scanpackages -m $repo > $repo/Packages
gzip -9c $repo/Packages > $repo/Packages.gz
Sign $repo Packages

