# Nix-Based Kadena Devnet

This repo is a Nix flake for running a chainweb devnet environment locally. The flake uses [the devenv project](https://devenv.sh/) for configuring the set of processes and their interdependencies to be managed by the [process-compose](https://github.com/F1bonacc1/process-compose) process manager, a project that aims to be a docker-compose replacement for native processes.

## Setting up the Nix cache

In order to speed up the build time of the Nix-based Devnet, you can add the following entries to your [`nix.conf`](https://nixos.org/manual/nix/stable/command-ref/conf-file) (typically at `/etc/nix/nix.conf`, but make sure a user-specific `nix.conf` doesn't override these lines):

```
substituters = https://nixcache.chainweb.com https://cache.nixos.org/
trusted-public-keys = nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

I.e. in particular, you're adding the `https://nixcache.chainweb.com` substituter with its public key: `nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY=`

## Running devnet natively via Nix

The default app of this flake runs devnet, so it's possible to run it by executing the following in this repo:
``` bash
nix run --impure
```

Since this repo is a flake, it can also be run via a remote flake reference without checking out the repo first:
``` bash
nix run github:kadena-community/nix-kda-cli --impure
```

Instead of running the devnet environment, it's also possible to access the script that runs the devnet environment. The following command will print the path to a shell script that runs the devnet environment.
``` bash
nix build --no-link --print-out-paths --impure
```

Note that the `--impure` flag is needed because [devenv](https://devenv.sh/) (used for setting up the processes) needs to learn the current directory in order to keep the devnet state under `./.devenv`.

## Building a devnet Docker image

The default app runs all the devnet processes as child processes of process-compose. This flake also exposes a package that builds a docker image whose entry point is the devnet script, so it's possible to run the whole environment under a single docker container. The following command builds a container and prints the `/nix/store` path to it:

``` bash
nix build .#container --no-link --print-out-paths
```

Or with the following command without checking out the repo:
``` bash
nix build github:kadena-community/nix-kda-cli#container --no-link --print-out-paths
```

The following is a convenient way to build the image and load it into docker in one go:
``` bash
docker load < $(nix build github:kadena-community/nix-kda-cli#container --no-link --print-out-paths)
```

Note that the `--impure` option is not needed for building docker images, because that operation doesn't need to know the current directory.

# Running the devnet Docker image

The docker image serves an HTTP API at port 8080. This API serves as a convenient entry-point into the system. You can start the devnet with local access to the HTTP API as follows:
``` bash
docker run -it -p 8080:8080 <devnet-image-name>
```

Once the image is running, visit http://localhost:8080 in your browser for more information about your devnet environment. For example, you can learn in [the Container API section](http://localhost:8080/#container-api) that the system state is stored in the `/data` folder, so you can keep the devnet state outside the container by mounting that folder as a docker volume:

``` bash
docker volume create l1
docker run -it -p 8080:8080 -v l1:/data <devnet-image-name>
```