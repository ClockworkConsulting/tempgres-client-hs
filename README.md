# pg-harness

`pg-harness` is a REST service for conveniently creating temporary
PostgreSQL databases. It is intended for use from tests.

## Usage

Once the service is set up and running (see below), you can do a HTTP
POST to it to create a temporary database. For example,

'''
    $ curl -d '' http://localhost:8900
    pg-harness-test:pg-harness-pass@db:5432/temp_ba36rk6rqvs92wbofk55sz5k3pcl0u74x
'''

The response indicates that the temporary database
`temp_ba36rk6rqvs92wbofk55sz5k3pcl0u74x` has been created on the
database server `db` (port 5432) and made available to the user
`pg-harness-test` using the password `pg-harness-pass`.

The database will automatically be destroyed after a configurable
duration, though any temporary databases that have not been destroyed
when the service is stopped will stay around. All temporary databases
will be named `temp_...`.

## Security Notice

Since `pg-harness` must be able to forcibly drop connections to the
temporary databases it creates, you'll need to set up a superuser
account on the database server. I would recommend using a separate
account from the normal database superuser, and I would also *highly*
recommend using a *non-production* PostgreSQL instance.

Furthermore, you should definitely **NOT** run this on any network
facing the public Internet since no attempt has been made to prevent
DoS attacks and the like. The `pg-harness` REST service is only meant
for development LANs which are firewalled off.

## Installing the service

The recommended installation option is to use a Cabal sandbox for
installation, for example

```
    $ mkdir ~/opt/pg-harness
    $ cabal sandbox init
    $ cabal install pg-harness
    $ ln -s
```

When the installation is done, update the `pg-harness.ini` file to
suit your setup (see below).

You can now run `pg-harness` manually from
`./.cabal-sandbox/bin/pg-harness`, or you could configure it run as a
system service (e.g. via systemd, upstart or similar).

## Database Setup

The user names in this section are just examples that'll minimize the
number of changes you'll need to do to the `pg-harness.ini` that's
shipped with `pg-harness`. You can change the user names here to
anything you like, just make sure the configuration file reflects any
changes you make.

To create the administrator user, use the command

```
    $ createuser -d -E -i -l -P -s pg-harness
```

as the PostgreSQL superuser. Make sure you enter a password that is
**not** used for any other critical infrastructure since you'll need
to put the password in the `pg-harness.ini` configuration file.

To provide client programs with access to the temporary databases,
you'll also need an unprivileged user. This user will only have access
to the temporary databases that are created by the harness. To create
the user, use the command

```
    $ createuser -D -E -i -l -P -S pg-harness-test
```

Enter a password and put that password in the configuration file. Note
that only the user name is used during normal operation of the
`pg-harness` REST service, so any problems with the password will only
become apparent once your tests actually try to connect.
