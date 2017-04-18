#!/bin/bash

#docker rm $(docker ps -a -q) -f
#docker rmi $(docker images -a -q) -f
#docker volume rm $(docker volume list -q) -f

docker volume create --name databrary_postgres_store
docker build -t databrary_postgres postgres/
docker run -d -v databrary_postgres_store:/var/lib/postgresql/data -p 5433:5433 --rm --name databrary_postgres databrary_postgres
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"

./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/remove-superuser-db.sh"
docker stop databrary_postgres
