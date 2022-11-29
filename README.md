# Docker Compose Deployment of Kadena Devnet

A collection of [docker compose](https://docs.docker.com/compose)
configurations for running a full Kadena blockchain network on your development
machine.


## Prerequisites

Before you begin, check your docker settings to make sure that docker has access
to at least 8GB of RAM and 4 CPUs.

## Run the network

Start network:

```sh
docker compose pull
docker compose up -d
```

Show that the network is making progress:

```sh
docker compose run --rm curl -sk https://api-node:1789/chainweb/0.0/development/cut
```

Stop network:

```sh
docker compose down
```

Services are exposed on the host on port 8080 via HTTP.

## Settings

Default values for some settings are defined in the `.env` file and can be
overwritten with environment variables:

```.env
# Images
CHAINWEB_NODE_IMAGE=ghcr.io/kadena-io/chainweb-node/ubuntu:latest
MINING_CLIENT_IMAGE=ghcr.io/kadena-io/chainweb-mining-client:latest

# Ports that are exposed on the host system
HOST_SERVICE_PORT=8080
HOST_STRATUM_PORT=1917

# Miner keys
MINER_PUBLIC_KEY=f89ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f
MINER_PRIVATE_KEY=da81490c7efd5a95398a3846fa57fd17339bdf1b941d102f2d3217ad29785ff0
```

## chainweb-node Docker Images

The fastest way to build for development and incremental builds is to build
chainweb-node docker images locally. The docker compose file expects the
chainweb-node binary on the image in the folder /chainweb/chainweb-node`.

The Docker images for officially released chainweb versions are stored in the
packages
[chainweb-node/ubuntu](https://github.com/orgs/kadena-io/packages/container/package/chainweb-node%2Fubuntu)
and
[chainweb-node/alpine](https://github.com/orgs/kadena-io/packages/container/package/chainweb-node%2Falpine)

Docker images of development versions of chainweb-node are created by [Build
Applications
Workflow](https://github.com/kadena-io/chainweb-node/actions/workflows/applications.yml)
and stored in [this private github
package](https://github.com/orgs/kadena-io/packages/container/package/chainweb-node)
in the [Github Container Registry](https://ghcr.io). By default only images for
revisions in the master branch are created. Images for other revisions can be
[triggered on
demand](https://github.com/kadena-io/chainweb-node/actions/workflows/applications.yml)
by clicking on `Run Workflow` selecting the respective branch.

The docker image URLs are:

*   development image for git rev SHORT_REV: `ghcr.io/kadena-io/chainweb-node:sha-SHORT_REV` (private)
*   latest released ubuntu image: `ghcr.io/kadena-io/chainweb-node/ubuntu:latest` (public)
*   latest released alpine image: `ghcr.io/kadena-io/chainweb-node/alpine:latest` (public)

Use of the private images requires to log into the ghcr.io docker registry:

```
docker login ghcr.io
```

While it might be possible to use the Github password for login, it is recommend
to create a [PAT token for that purpose](https://docs.github.com/en/packages/working-with-a-github-packages-registry/migrating-to-the-container-registry-from-the-docker-registry#authenticating-to-the-container-registry).

## Environment Variables

Override docker image (chainweb-node revision):

```sh
export CHAINWEB_NODE_IMAGE=ghcr.io/kadena-io/chainweb-node:sha-SHORT_REV
docker compose up -d
```

Override port on which services are exposed on the host:

```sh
export HOST_SERVICE_PORT=8000
docker compose up -d
```

## Stratum Server

Note that stratum server support is yet experimental. It can be enabled via the
docker compose `--profile stratum` flag. The stratum port is exposed on the host
at port number 1917.

## Logging To Elasticsearch

In order to send telemetry and log messages to a local Elasticsearch stack
run devnet as follows:

```
docker compose -f docker-compose.yaml -f elasticsearch.yaml up -d
```

It will take up to a minute until Elasticsearch and Kibana are initialized. In
order to capture all logs right from the start it is recommended to first only
start Kibana and Elasticsearch and start the other services only some time
(~1min) later:

```
docker compose -f docker-compose.yaml -f elasticsearch.yaml up -d kibana
sleep 60
docker compose -f docker-compose.yaml -f elasticsearch.yaml up -d
```

Kibana is served on the host at [http://localhost:5601]().
Elasticsearch is exposed on port 9200.

All data saved in Elasticsearch is deleted when the service is shut down or
restarted.

Preinitialized Kibana definitions are available as saved objects in the
file `./config/kibana.saved-objets.ndjson`. Changes and additions can be made
permanent by exporting all Kibana objects and saving them to this file.

## Running Tests

The `test` service, which is disabled by default, can be used to run test
scripts on a container within devnet. The service runs an ubuntu image with
curl, jq, rsync, and nodejs pre-installed. The `test` sub-directory is
mounted into that container. The default entry point is `/bin/bash`.

The following command provides an interactive shell prompt on the test
container.

```
docker compose run --rm test
```

The following is an example for a test script that prints the cuts for
all nodes in the cluster:

```
docker compose run --rm -T test cuts.sh
```

## Debugging with Curl

In order to make it easier to query individual nodes from the host there is a
service definition that provide an alpine curl image. The service has the
`debug` profile and is thus disabled by default.

The entrypoint is just a curl binary which can be used to run curl in the
context of the devnet network environment:

```
docker compose run -- curl -sk "https://devnet_api-node_1:1789/chainweb/0.0/development/cut"
```

The domaines of individual containers in the devnet network are the same as the
names of the respective containers and are of the form:

```
devnet_{{SERVICE_NAME}}_{{REPLICA_NUMBER}}
```

It is also possible to use just the service name `{{SERVICE_NAME}}`, which case
a random container for that service is choosen.

```
docker compose run -- curl -sk "https://common-node:1789/chainweb/0.0/development/cut"
```

## Caveats

Restarting nodes via `docker compose up` does not preserve
databases. Therefore nodes have to perform a complete catchup after restart.

Node restarts without deleting the database can be performed by defining nodes
in devnet.yaml that store the database on a named value or on the host.

# Minimal Devnet

In instances where only a single devnet node is required, such as doing transaction tests and running or having limited system resources, you can use a minimal devnet installation availible in docker-compose.minimal.yaml.

This should be started from the root of the devnet folder just like the main docker-copose.yaml, as it references local files.
```
docker compose -f docker-compose.minimal.yaml up --remove-orphans -d
# use --remove-orphans if you no longer need the full devnet containers and they're causing problems 
```

This instance features a single node, simulation mining client, and nginix proxy.  It does not have a stratum profile or others from the main docker-compose.yaml at this time (profiles 'test' and 'pact' are present, but have not been tested with this compose file).  It does utilize the the same environment variables (except the unused stratum port or the node replica counts).

## Database bind mount
The docker-compose.minimal.yaml devnet instance references a folder called 'db' in the devnet directory.  If this folder does not exist, it will create one when you first start a minimal node.  The chainweb databases will be stored as such:
```
db/0/rocksDb
db/0/sqlite
```
* You can delete this folder to reset your devnet blockchain, or back it up to make a savestate
    * _Turn off the node container before doing stuff to this folder_
    * The structure must match the above with a 0/ folder
    * You do not *need* the sqlite folder if you want to save backup space, as chainweb-node will rebuild one from the rocksDb if present
* If you are starting from a fresh database, you may wish to wait for the block height of each chain to pass the latest feature fork for devnet
    * As of 2.15, this is a height of 165.  It takes an hour or so to reach this from scratch.
    * The rest api should be exposed on your localhost, so you can use the command below to check the height of chain 0.
    
        ```curl -s http://localhost:8080/chainweb/0.0/development/cut | jq '.hashes."'0'".height'```
        
        You can also use the commands listed elsewhere in this document to query into the node container directly if you reference the right container name: devnet-bootstrap-node-1

# Setup Machine

### Ubuntu:

Install docker

```sh
sudo apt-get update
sudo apt-get install ca-certificates curl gnupg lsb-release
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo \
 "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
 $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
```

Install docker compose V2

```sh
mkdir -p ~/.docker/cli-plugins/
curl -SL https://github.com/docker/compose/releases/download/v2.2.3/docker-compose-linux-x86_64 -o ~/.docker/cli-plugins/docker-compose
chmod +x ~/.docker/cli-plugins/docker-compose
```

Give current user access to docker

```sh
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
```

Install other tools

```sh
sudo apt-get install git jq curl vim net-tools
```

Clone devnet repository

```sh
git clone https://github.com/kadena-io/devnet
```

### Docker:

Devnet requires that Docker have access to the following resources in order to perform as expected:
- At least 8 GB of RAM memory
- At least 4 CPU cores

**NOTE:**
Running Devnet with less than 8 GB RAM memory could result in some containers exiting with code 137.


**NOTE:**
The resources Docker has access to can be changed in the Docker Desktop.

