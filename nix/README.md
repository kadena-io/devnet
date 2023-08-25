This repo is a Nix flake for running a chainweb devnet environment locally. The flake uses [the devenv project](https://devenv.sh/) for configuring the set of processes and their interdependencies to be managed by the [process-compose](https://github.com/F1bonacc1/process-compose) process manager, a project that aims to be a docker-compose replacement for native processes.

# Setting up the Nix cache

TODO

# Running devnet natively via Nix

The default app of this flake runs devnet, so it's possible to run it by executing the following in this repo:
``` bash
nix run
```

Since this repo is a flake, it can also be run via a remote flake reference without checking out the repo first:
``` bash
nix run github:kadena-community/nix-kda-cli
```

Instead of running the devnet environment, it's also possible to access the script that runs the devnet environment. The following command will print the path to a shell script that runs the devnet environment.
``` bash
nix build --no-link --print-out-paths
```

# Building a devnet Docker image

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
cat $(nix build github:kadena-community/nix-kda-cli#container --no-link --print-out-paths) | docker load
```

# Running the devnet Docker image

The docker image serves an HTTP API at the port 1337. The image also stores its environment state under the /root/.devenv folder. The following docker invocation will run the environment while exposing its HTTP port at 8080 and keeping the environment state under the user's `$HOME/l1`:
``` bash
docker run -it -p 8080:1337 -v $HOME/l1:/devnet/.devenv <devnet-image-name>
```
