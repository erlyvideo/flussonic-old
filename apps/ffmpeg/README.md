Compile
=======

Must have installed ffmpeg libraries


Run tests
=========

Launch test flussonic_ffmpeg first: 

    ./priv/flussonic_ffmpeg -d 9000 -f

then 

    make test




Building
========


wget http://www.tortall.net/projects/yasm/releases/yasm-1.2.0.tar.gz
tar zxvf yasm-1.2.0.tar.gz
cd yasm-1.2.0
./configure --prefix=/usr/local
make
sudo make install
cd ..
git clone git://git.videolan.org/x264.git
cd x264
git checkout bc13772e21d0e61dea6ba81d0d387604b5b360df
./configure  --enable-shared --prefix=/opt/flussonic/apps/ffmpeg --libdir=/opt/flussonic/apps/ffmpeg/priv --bindir==/opt/flussonic/apps/ffmpeg/priv --includedir=/opt/flussonic/apps/ffmpeg/priv
make
sudo make install
cd ..
git clone git://source.ffmpeg.org/ffmpeg.git
git checkout b3b456b2588f67a152f3035cbc80d349898534c1
./configure --enable-debug  --prefix=/opt/flussonic/apps/ffmpeg --datadir=/opt/flussonic/apps/ffmpeg/priv --mandir=/opt/flussonic/apps/ffmpeg/priv --libdir=/opt/flussonic/apps/ffmpeg/priv --shlibdir=/opt/flussonic/apps/ffmpeg/priv --bindir==/opt/flussonic/apps/ffmpeg/priv --incdir=/opt/flussonic/apps/ffmpeg/priv --enable-shared --enable-gpl --enable-version3 --enable-nonfree --enable-hardcoded-tables  --enable-libx264 --extra-cflags="-I/opt/flussonic/apps/ffmpeg/priv" --extra-ldflags="-L/opt/flussonic/apps/ffmpeg/priv"
make
sudo make install
cd ..
sudo apt-get install rubygems
sudo gem install --no-rdoc --no-ri fpm