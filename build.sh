# this is just for fdkaac
#!/bin/bash
set -e

# need root for installs
sudo -s <<EOF

# for fdk-aac
sed -i "/^# deb.*multiverse/ s/^# //" /etc/apt/sources.list
echo 'deb http://us.archive.ubuntu.com/ubuntu/ xenial multiverse' >> /etc/apt/sources.list
echo 'deb http://us.archive.ubuntu.com/ubuntu/ xenial-updates multiverse' >> /etc/apt/sources.list

# deps
apt-get update && apt-get install -y libgmp-dev git yasm npm libcrack2-dev gcc g++ autoconf automake zlib1g-dev \
		   libmp3lame-dev libx264-dev libfdk-aac-dev libavformat-dev libswscale-dev libavcodec-dev libavutil-dev curl \
		   pkg-config vim ssmtp postgresql-client libgmp3-dev libz-dev

# nonstandard node binary
ln -s /usr/bin/nodejs /usr/bin/node

# compile ffmpeg from source
cd /usr/src
git clone git://source.ffmpeg.org/ffmpeg.git
cd ffmpeg
git remote update origin
ffmpeg=2.8.x && ffmpeg=`git describe --abbrev=0 origin/release/${ffmpeg%.x}` && ffmpeg=${ffmpeg#u} && git checkout $ffmpeg
./configure --enable-gpl --enable-version3 --enable-nonfree --enable-libx264 --enable-libfdk-aac --enable-libmp3lame # --extra-ldexeflags='-lcr_-u cr_run_link_me'
make && make install

#get docker
wget -qO- https://get.docker.com/ | sh
exit

sudo usermod -aG docker $USER
su - $USER <<EOF

cd ~

mkdir src
cd src

# ghc
wget https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3b-x86_64-deb8-linux.tar.xz && tar -xvf ghc-7.10.3b-x86_64-deb8-linux.tar.xz
cd ghc-7.10.3 && ./configure && sudo make install

cd ~/src

# cabal
wget https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/cabal-install-1.24.0.2.tar.gz && tar -xvf cabal-install-1.24.0.2.tar.gz
cd cabal-install-1.24.0.2 && EXTRA_CONFIGURE_OPTS= sudo ./bootstrap.sh --global
export PATH=$PATH:/usr/local/bin
cabal update

cd ~/src

# get databrary
git clone git://github.com/databrary/databrary

# make postgres and solr containers
cd src/Docker
docker volume create --name databrary_postgres_store
docker build -t databrary_postgres postgres/
docker run -d -v databrary_postgres_store:/var/lib/postgresql/data -p 5432:5432 --rm --name databrary_postgres databrary_postgres
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"

docker volume create --name databrary_solr_store
docker build -t databrary_solr solr/
docker run -d -v databrary_solr_store:/opt/solr -p 8983:8983 --rm --name databrary_solr databrary_solr
./wait-for-solr.sh localhost "docker exec databrary_solr solr create -c databrary_core -d /databrary_conf"

cd ~/src/databrary

# cabal sandbox
cabal sandbox init
cabal install happy --force-reinstalls
cabal install --only-dependencies --force-reinstalls
cabal configure 
cp example.conf databrary.conf
yes | cabal install 
