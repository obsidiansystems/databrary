# Databrary 2.0

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
cd ..
```
postgres pw: mysecretpassword

```
psql -f db/schema/master_sql -h localhost -U postgres
docker-compose up
```

docker will start solr, postgres, and redis instance in that terminal. open a new terminal

```
cd ~/go/src/github.com/databrary/databrary
```
 
replace $USER with your name in `config/databrary_dev.toml`. finally

```
./databrary -c config/databrary_dev.toml
```

will serve databrary on port 3444 over https. the https will be reported as insecure because the certs are self-signed but oh well.



