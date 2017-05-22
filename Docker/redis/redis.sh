#!/bin/bash
#docker build . -t databrary_redis
docker run --name databrary_redis --rm -d -p 6379:6379 databrary_redis
