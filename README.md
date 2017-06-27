# Databrary 2.0


Table of Contents
=================

  * [Dependencies](#dependencies)
     * [git](#git)
     * [mercurial](#mercurial)
     * [golang](#golang)
     * [Docker](#docker)
     * [Docker Compose](#docker-compose)
     * [PSQL](#psql)
     * [Dev Tools](#dev-tools)
  * [Instructions](#instructions)

## Dependencies

### git

get it from somewhere

### mercurial

get it from somewhere

### golang

```
wget https://storage.googleapis.com/golang/go$VERSION.$OS-$ARCH.tar.gz
sudo tar -C /usr/local -xzf go$VERSION.$OS-$ARCH.tar.gz

export PATH=$PATH:/usr/local/go/bin:~/go/bin
echo "export PATH=$PATH:/usr/local/go/bin:~/go/bin" >> ~/.bashrc

go get -u github.com/golang/dep/cmd/dep
```

### Docker

```
wget -qO- https://get.docker.com/ | sh
sudo usermod -aG docker $USER
```
you will need to login/logout

### Docker Compose

```
sudo wget -O /usr/local/bin/docker-compose https://github.com/docker/compose/releases/download/1.14.0/docker-compose-`uname -s`-`uname -m`

sudo chmod +x /usr/local/bin/docker-compose
```

### PSQL

```
sudo apt-get install postgresql-client
```

### Dev Tools

I highly highly recommend [Gogland](https://www.jetbrains.com/go/) as a go ide.

## Instructions

```
go get -u github.com/databrary/databrary
cd ~/go/src/github.com/databrary/databrary
git checkout go_master
dep ensure
go build

cd Docker
./docker_build.sh
docker-compose up
```
postgres pw: mysecretpassword

docker will start solr, postgres, and redis instance in that terminal. open a new terminal

```
cd ~/go/src/github.com/databrary/databrary
psql -f db/schema/master_sql -h localhost -U postgres -d databrary
```
 
replace $USER with your name in `config/databrary_dev.toml`. finally

```
GMAILPASSWORD={{password}} ./databrary -c config/databrary_dev.toml
```

will serve databrary on port 3444 over https. the https will be reported as insecure because the certs are self-signed but oh well.




