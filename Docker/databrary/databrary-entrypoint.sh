#!/bin/bash
git pull origin build_system 
##stack init --force --solver
stack build
stack exec databrary -- -c/home/databrary/src/databrary.conf

