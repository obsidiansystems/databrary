#!/bin/bash

docker rm $(docker ps -a -q) -f
docker rmi $(docker images -a -q) -f
docker volume rm $(docker volume list -q) -f

docker volume create --name databrary_postgres_store
docker build -t databrary_postgres postgres/
docker run -d -v databrary_postgres_store:/var/lib/postgresql/data -p 5432:5432 --rm --name databrary_postgres databrary_postgres
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"

docker volume create --name databrary_log_store
docker build -t databrary_databrary:base -f databrary/Dockerfile_base ./databrary
docker build -t databrary_databrary:config -f databrary/Dockerfile_config ./databrary
docker build -t databrary_databrary:final -f databrary/Dockerfile_final ./databrary
docker stop databrary_postgres

docker volume create --name databrary_solr_store
docker build -t databrary_solr solr/
docker run -d -v databrary_solr_store:/opt/solr -p 8983:8983 --rm --name databrary_solr databrary_solr
./wait-for-solr.sh localhost "docker exec databrary_solr solr create -c databrary_core -d /databrary_conf"
docker stop databrary_solr
