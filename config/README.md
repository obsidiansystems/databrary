We use [viper](https://github.com/spf13/viper) for config.

In here we have the actual config files for different environments
and the configuration artifacts for [sqlboiler](https://github.com/databrary/sqlboiler)
and [xo](https://github.com/knq/xo).

Note that you can't change `sqlboiler.toml` to any other name
because the `sqlboiler` executable expects that to be the name
of its config file. Also the `sqlboiler/templates/templates...` directory structure
is necessary because sqlboiler expects a base directory where
all of the templates are kept.

The version of sqlboiler though has been heavily modified to enable use of "custom types".

Here are the commands to generate the models

```bash
go sqlboiler github.com/databrary/sqlboiler
sqlboiler postgres --basedir templates/ -o models/sqlboiler_models/public -p public
rsync -avpP models/sqlboiler_models/ ../../db/models/sqlboiler_models/

```

