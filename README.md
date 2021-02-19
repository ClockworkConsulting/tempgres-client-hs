# pg-harness

`pg-harness` is a REST service for conveniently creating temporary
PostgreSQL databases. It is intended for use from tests.

## Usage

Once the service is set up and running (see below), you can do a HTTP
POST to it to create a temporary database. For example,

```
$ curl -d '' http://localhost:15432
pg-harness-client
pg-harness-cpass
localhost
15432
temp_jm1sufkdkdyn1ekifld1nyblj8vuomjz9
```

The response indicates that the temporary database
`temp_jm1sufkd...` has been created on the
database server `localhost` (port 5432) and made available to the user
`pg-harness-test` using the password `pg-harness-cpass`.

The database will automatically be destroyed after a configurable
duration, though any temporary databases that have not been destroyed
when the service is stopped will stay around. All temporary databases
will be named `temp_...`.

## Usage (Haskell)

An easy-to-use client library for Haskell is included; see
the [Hackage entry](http://hackage.haskell.org/package/pg-harness-client)
for documentation.

## Security Notice

This service is strictly meant for use on one's own computer or on development
LANs which are firewalled off from the public Internet.

## Installing the service

The recommended installation option is to use 'Docker' to build a container,
e.g.

```sh
$ ./docker/build.sh
```

This runs the build, and should produce some output akin to:

``` text
 ...
 => exporting to image
 => => exporting layers
 => => writing image sha256:f47ee2bb5dd2d1df6577439a421fe00383eb599193d395a96178fb2d2360cfba
 => => naming to docker.io/library/pg-harness:latest
```

To run the container "in the background", run

```sh
$ docker run -p 15431:8080 -p 15432:5432 -d --name=pg-harness sha256:f47ee2bb5dd2d1df657743...
```

To adjust the ports (15431, 15432) you will need to set the `HTTP_PORT` and
`PG_HARNESS_PUBLISHED_ADDRESS_PORT` environment variables when starting the
container.

You can also set other environment variables to control user names, passwords,
etc. See the `Dockerfile` and `Configuration.hs` for details.
