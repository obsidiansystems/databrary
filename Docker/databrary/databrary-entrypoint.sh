#!/bin/bash
service postfix restart
su databrary -c 'git pull origin build_system'
##stack init --force --solver
su databrary -c 'stack build'
su databrary -c 'stack exec databrary -- -c/home/databrary/src/databrary.conf'

