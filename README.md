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

macOS: 

```brew install mercurial```

### golang

```
wget https://storage.googleapis.com/golang/go$VERSION.$OS-$ARCH.tar.gz
sudo tar -C /usr/local -xzf go$VERSION.$OS-$ARCH.tar.gz

export PATH=$PATH:/usr/local/go/bin:~/go/bin
echo "export PATH=$PATH:/usr/local/go/bin:~/go/bin" >> ~/.bashrc

go get -u github.com/golang/dep/cmd/dep
```

macOS: 

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

```
wget -qO- https://get.docker.com/ | sh
sudo usermod -aG docker $USER
```
you will need to login/logout

macOS:

Download and install docker from https://www.docker.com/docker-mac. This will configure the env for you, ready to use.

### Docker Compose

```
sudo wget -O /usr/local/bin/docker-compose https://github.com/docker/compose/releases/download/1.14.0/docker-compose-`uname -s`-`uname -m`

sudo chmod +x /usr/local/bin/docker-compose
```

macOS:

Docker includes Docker Compose for macOS, so no need to do anything here.

### PSQL

```
sudo apt-get install postgresql-client
```

macOS:

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
docker-compose up
```
postgres pw: mysecretpassword

Docker will start solr, postgres, and redis instance in that terminal. 

Second, open a new terminal and do this only once. This will build tables anew from the existing schema, replacing your local database. Once you add data to your local database, you should not rebuild it again. 

```
cd ~/go/src/github.com/databrary/databrary
psql -f db/schema/master_sql -h localhost -U postgres -d databrary
```
 
Third, replace $USER with your name in `config/databrary_dev.toml`. 
macOS: Change /home/$USER to /Users/username (replace username with your Mac username: e.g., /Users/jack.)

Fourth, finally do the following to start the app.

```
GMAILPASSWORD={{password}} ./databrary -c config/databrary_dev.toml
```

Thsi will serve databrary on port 3444 over https and set the password for the email sending functionality. The https will be reported as insecure because the certs are self-signed. Tell the browser to trust the site.




