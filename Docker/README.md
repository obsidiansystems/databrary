The Dockerfile for postgres installs some dependencies to make pgranges installation
work - pgranges is some C code for unioning/intersecting more than
2 ranges etc. The init-user scripts creates the databrary user with
superuser privileges. The remove-superuser script removes those privileges.
The privileges are necessary for installation of the pgranges functions
interface ([here](https://github.com/databrary/databrary/blob/go_models/db/schema/master_sql#L357)).
After creating those interface functions superuser privileges aren't necessary
and should be removed.

`wait-for-postgres.sh` is just a script that pings the postgres
instance to see whether it's ready to accept connections.

`docker_build.sh` just automates creating the volume store for the
postgres instance and creating the `databrary` user and then removing
superuser privileges.

