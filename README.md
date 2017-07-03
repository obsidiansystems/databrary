# Databrary 2.0

Table of Contents
=================

  * [Dependencies](#dependencies)
     * [git](#git)
     * [golang](#golang)
        * [ubuntu](#ubuntu)
        * [macOS](#macos)
     * [Docker](#docker)
        * [ubuntu](#ubuntu-1)
        * [macOS](#macos-1)
     * [Docker Compose](#docker-compose)
        * [ubuntu](#ubuntu-2)
        * [macOS](#macos-2)
     * [PSQL](#psql)
        * [ubuntu](#ubuntu-3)
        * [macOS](#macos-3)
     * [Dev Tools](#dev-tools)
  * [To start the app](#to-start-the-app)
  * [API](#api)
  * [Secrets](#secrets)


## Dependencies

### git

get it from somewhere

### golang

#### ubuntu
```
wget https://storage.googleapis.com/golang/go$VERSION.$OS-$ARCH.tar.gz
sudo tar -C /usr/local -xzf go$VERSION.$OS-$ARCH.tar.gz

export PATH=$PATH:/usr/local/go/bin:~/go/bin
echo "export PATH=$PATH:/usr/local/go/bin:~/go/bin" >> ~/.bashrc

go get -u github.com/golang/dep/cmd/dep
```

#### macOS

explanation: http://www.golangbootcamp.com/book/get_setup

```
brew install go
mkdir $HOME/go
export GOPATH=$HOME/go
```

Open .bash_profile and add the following:

```
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
```

And then 
```
source ~/.bash_profile
```

Get dep
```
go get -u github.com/golang/dep/cmd/dep
```

### Docker

#### ubuntu

```
wget -qO- https://get.docker.com/ | sh
sudo usermod -aG docker $USER
su - $USER
```

#### macOS

Download and install docker from https://www.docker.com/docker-mac. This will configure the env for you, ready to use.

### Docker Compose

#### ubuntu

```
sudo wget -O /usr/local/bin/docker-compose https://github.com/docker/compose/releases/download/1.14.0/docker-compose-`uname -s`-`uname -m`

sudo chmod +x /usr/local/bin/docker-compose
```

#### macOS

Docker includes Docker Compose for macOS, so no need to do anything here.

### PSQL

#### ubuntu
```
sudo apt-get install postgresql-client
```

#### macOS

```
brew install postgresql
```

### Dev Tools

I highly highly recommend [Gogland](https://www.jetbrains.com/go/) as a go ide.

## To start the app

First start the docker

```
go get -u github.com/databrary/databrary
cd ~/go/src/github.com/databrary/databrary
git checkout go_master
dep ensure
go build

cd Docker
./docker_build.sh
docker-compose up -d
```


**Replace `$USER` with your name in `config/databrary_dev.toml`**.

**macOS**: Change /home/$USER to /Users/username (replace username with your Mac username: e.g., /Users/jack.)

The app uses gmail to send service emails so to start the app you need to set the GMAILPASSWORD env var

```
GMAILPASSWORD=<password> ./databrary -c config/databrary_dev.toml
```

This will serve databrary on [https://localhost:3444](https://localhost:3444)  over https and set the password for the email sending functionality. The https will be reported as insecure because the certs are self-signed. Tell the browser to trust the site.

## API

[API](api.md)

## Secrets

[sqlboiler config db pw](../go_master/config/sqlboiler/sqlboiler.toml#L12)

[config db pw](../go_master/config/databrary_dev.toml#L10)

[config redis pw](../go_master/config/databrary_dev.toml#L20)

[config redis secret](../go_master/config/databrary_dev.toml#L20)

[config cookie hash key](../go_master/config/databrary_dev.toml#L25)

[config cookie block key](../go_master/config/databrary_dev.toml#L26)

[docker pg pw](../go_master/Docker/postgres/Dockerfile#L2)

[docker script pg pw](../go_master/Docker/docker_build.sh#L20)

[docker script pg pw](/go_master/Docker/wait-for-postgres.sh#L10)

[docker pg pw](../go_master/Docker/postgres/init-user-db.sh#L3)

[redis config pw](../go_master/Docker/redis/redis.conf#L1)
