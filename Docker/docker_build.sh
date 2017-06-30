#!/bin/bash
containers=`docker ps -a -q`
if [ -n "$containers" ]; then
  docker rm $containers -f
fi
images=`docker images -a -q`
if [ -n "$images" ]; then
  docker rmi $images -f
fi
volumes=`docker volume list -q`
if [ -n "$volumes" ]; then 
  docker volume rm $volumes -f
fi
docker volume create --name databrary_postgres_store
docker build -t databrary_postgres postgres/
docker run -d -v databrary_postgres_store:/var/lib/postgresql/data -p 5432:5432 --rm --name databrary_postgres databrary_postgres
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"

./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/remove-superuser-db.sh"
PGPASSWORD=mysecretpassword psql -f ../db/schema/master_sql -h localhost -U postgres -d databrary

docker stop databrary_postgres

docker volume create --name databrary_solr_store
docker volume create --name databrary_logs_store

