#!/bin/bash

docker build -t databrary_postgres postgres/
docker build -t databrary_databrary:base -f databrary/Dockerfile_base ./databrary

docker create -v /var/lib/postgresql/data --name databrary_dbstore databrary_postgres /bin/true
docker run -d --volumes-from databrary_dbstore -p 5432:5432 --rm --name databrary_postgres databrary_postgres
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"
docker build -t databrary_databrary:config -f databrary/Dockerfile_config ./databrary

docker build -t databrary_databrary:final -f databrary/Dockerfile_final ./databrary
docker stop databrary_postgres

docker build -t databrary_solr solr/
docker create -v /opt/solr/ --name databrary_solrstore databrary_solr /bin/true

docker run -d --volumes-from databrary_solrstore -p 8983:8983 --rm --name databrary_solr databrary_solr
docker exec databrary_solr solr create -c databrary_core -d /opt/solr/databrary_conf
#docker exec databrary_solr solr create -c databrary_core -d /databrary_conf
docker stop databrary_solr
