This repo is a Nix flake for running a chainweb devnet environment locally. The flake uses [the devenv project](https://devenv.sh/) for configuring the set of processes and their interdependencies to be managed by the [process-compose](https://github.com/F1bonacc1/process-compose) process manager, a project that aims to be a docker-compose replacement for native processes.

# Setting up the Nix cache

TODO

# Running devnet natively via Nix

The default app of this flake runs devnet, so it's possible to run it by executing the following in this repo:

```bash
nix run
```

Since this repo is a flake, it can also be run via a remote flake reference without checking out the repo first:

```bash
nix run github:kadena-community/nix-kda-cli
```

Instead of running the devnet environment, it's also possible to access the script that runs the devnet environment. The following command will print the path to a shell script that runs the devnet environment.

```bash
nix build --no-link --print-out-paths
```

# Building a devnet Docker image

The default app runs all the devnet processes as child processes of process-compose. This flake also exposes a package that builds a docker image whose entry point is the devnet script, so it's possible to run the whole environment under a single docker container. The following command builds a container and prints the `/nix/store` path to it:

```bash
nix build .#container --no-link --print-out-paths
```

Or with the following command without checking out the repo:

```bash
nix build github:kadena-community/nix-kda-cli#container --no-link --print-out-paths
```

The following is a convenient way to build the image and load it into docker in one go:

```bash
cat $(nix build github:kadena-community/nix-kda-cli#container --no-link --print-out-paths) | docker load
```

# Running the devnet Docker image

To setup easy to access aliases you could add the following aliases to your `~/.zshrc` or `~/.bashrc`:

```sh
alias l1='docker run -it -p 8080:8080 -v l1:/data kadena/devnet:latest'
# optional: only add if you intend to work on L2
alias l2='docker run -it -p 8081:8080 -v l2:/data kadena/devnet:latest'
```

Before running the aliases, make sure to run the following commands:

```sh
docker volume create l1
# optional: only run if you intend to work on L2
docker volume create l2
```

Now you can start l1 or l2 by running:

```sh
l1
# or
l2
```
