# Chainweb Node

This repository contains the Chainweb node implementation and related tools.
It provides a braided Proof-of-Work blockchain runtime, node APIs, miner
coordination endpoints, and operator tooling.

## Table of Contents

- [Installing Chainweb](#installing-chainweb)
- [Bootstrap Nodes](#bootstrap-nodes)
- [Configuring, running, and monitoring the health of a Chainweb Node](#configuring-running-and-monitoring-the-health-of-a-chainweb-node)
- [Mining for a Chainweb Network](#mine-for-a-chainweb-network)
- [Chainweb Design](#chainweb-design)
  - [Component Structure Details](#component-structure)
  - [Architecture Overview](#architecture-overview)

## Installing Chainweb

Minimal recommended hardware requirements for nodes are:

    * 2 CPU cores
    * 4 GB of RAM
    * 250 GB SSD or fast HDD
    * Public IP address

If the node is also used as API server for Pact, mining, or chainweb-data: 4 CPU cores and 8GB of RAM.

### Ubuntu Linux

The following packages must be installed on the host system:

*   ubuntu-20.04:
    ```bash
    apt-get install ca-certificates libmpfr6 libgmp10 libssl1.1 libsnappy1v5 zlib1g liblz4-1 libbz2-1.0 libgflags2.2 zstd
    ```

*   ubuntu-22.04:
    ```bash
    apt-get install ca-certificates libmpfr6 libgmp10 libssl1.1 libsnappy1v5 zlib1g liblz4-1 libbz2-1.0 libgflags2.2 zstd
    ```

Chainweb-node binaries for ubuntu-20.04 and ubuntu-22.04 can be found
[here](https://github.com/vootaa/chainweb-node/releases).

Download the archive for your system and extract the binaries and place them
into a directory from where they can be executed.

At this point, you are ready to [run a Chainweb node](#configuring-running-and-monitoring-the-health-of-a-chainweb-node)

## Building from Source

*IMPORTANT NOTE: We recommend the use of officially released chainweb-node
binaries or docker images, which can be found in the
[release section of this
repository](https://github.com/vootaa/chainweb-node/releases).
If you decide to build your own binaries, please make sure to only use
officially released and tagged versions of the code. Those versions are
extensively tested to ensure that they are compatible with all other nodes in
the chainweb network. It is generally not safe to run arbitrary builds of the
master branch in production networks.*

Chainweb is a [Haskell](https://www.haskell.org/) project. After cloning the
code with git from this GitHub repository the chainweb-node application can be
built as follows.

### Building with Cabal

In order to build with `cabal` you have to install `ghc-9.12.2` (Haskell compiler)
and `cabal >= 3.16` (Haskell build-tool)

[Linux / Mac](https://www.haskell.org/ghcup/)

You need to install the development versions of the following libraries:
`gflags`, `snappy`, `zlib`, `lz4`, `bz2`, `zstd`.

On apt based distribution these can be installed as follows:

```text
apt-get install ca-certificates libssl-dev libmpfr-dev libgmp-dev libsnappy-dev zlib1g-dev liblz4-dev libbz2-dev libgflags-dev libzstd-dev
```

To build a `chainweb-node` binary:

```bash
# Only necessary if you haven't done this recently.
cabal update

# Build the project.
#
# After this, a runnable binary can be found by running `cabal list-bin chainweb-node`.
cabal build
```

## Bootstrap Nodes

Bootstrap nodes are used by chainweb-nodes on startup in order to discover other
nodes in the network. At least one of the bootstrap nodes must be trusted.

Chainweb node operators can configure additional bootstrap nodes by using the
`--known-peer-info` command line option or in a configuration file. It is also
possible to ignore the builtin bootstrap nodes by using the
`--enable-ignore-bootstrap-nodes` option or the respective configuration file
setting.

Bootstrap nodes must have public DNS names and a corresponding TLS certificate
that is issued by a widely accepted CA (a minimum requirement is acceptance by
the OpenSSL library).

Operators of bootstrap nodes are expected to guarantee long-term availability of
the nodes. The list of builtin bootstrap nodes should be kept up-to-date and
concise for each chainweb-node release.

If you like to have your node included as a bootstrap node please make a pull
request that adds your node to [P2P.BootstrapNodes module](src/P2P/BootstrapNodes.hs).

## Configuring, running, and monitoring the health of a Chainweb Node

**This section assumes you've installed the `chainweb-node` binary** somewhere
sensible, or otherwise have a simple way to refer to it.

**Note:** Your node needs to be reachable from the public internet. You will
have to perform Port Forwarding if your machine is behind a router (by default
port 1789 is used by the node).

**NOTE**: When you start chainweb-node for the first time it creates a new
empty database and starts to synchronize and catch up with other nodes in the
target Chainweb network. This process takes a long time -- several days. It is much
faster (depending on hardware one to a few hours) to just synchronize the chain
database or get a snapshot of it and only rebuild the pact databases from the
chain-database. Please, consult the documentation of the docker images for
chainweb-node about details on how to obtain an initial chain database.

Run your node:

```bash
chainweb-node
```

The node will communicate with other nodes in a P2P network. By default it uses
port 1789 for the P2P communication.

Node services are exposed via the service API, by default on port 1848. The
service API includes `/info`, `/health-check`, Pact endpoints, the mining API
endpoints, GET endpoints for on-chain data (headers, payloads, cuts), and an
HTTP event stream of block header updates. Some of these are disabled by default
(e.g. mining API, and header updates).

While the P2P endpoint must be directly available from the public internet, it
is highly recommended to expose the service API only on a private network. When
service API endpoints are made available publicly it is recommended to use a
reverse proxy setup things like rate limiting, authentication, and CORS.

### Configuration

No particular configuration is needed for running Chainweb node with the
default production chain profile.

Use `chainweb-node --help` to show a help message that includes a brief
description of all available command line options.

A complete configuration file with the default settings can be created with

```sh
chainweb-node --print-config > config.yaml
```

This file can then be edited in order to change configuration values.

The command `chainweb-node --help` also provides descriptions of these
configuration values.

Given a configuration file or a set of command line options it is possible to
print out only those configuration values that are different from their
respective default:

```
chainweb-node --config-file=config.yaml --some-command-line-options --print-config-as=minimal
```

### Monitoring the health of a Chainweb Node

The following outlines how you can check that your `chainweb-node` is healthy

`chainweb-node` should be running from the public IP address and a port that is open to the other Chainweb nodes.

If you're behind a NAT, it is **VERY IMPORTANT** that your network allows
external nodes to connect to the node you are running.

```text
$ chainweb-node --log-level <desired-log-level>
```

For production scenarios we recommend that you use log-level `warn` or `error`.
For troubleshooting or improved monitoring you can also use `info`.

Once your node is running, go through the following checks to verify that you have a healthy node:

    *   run the command in your terminal:
        ```
        $ curl -sk "https://<public-ip>:<port>/chainweb/0.0/mono/cut"
        ```
    *   navigate to this website on your browser: [https://yourPublicIp:port/chainweb/0.0/mono/cut](https://yourPublicIp:port/chainweb/0.0/mono/cut)
    *   check logs for whether services are started
    *   check if the node is receiving cuts
    *   look for errors in the logs
    *   look for warnings in the logs

Usually, when a node is receiving and publishing cuts (i.e. block heights at every chain), it's working correctly.

The `/cut` endpoint will return the latest cut that your node has. It's possible that your node is falling behind, so make sure to compare its cut height with the cut heights of the bootstrap nodes. It's also possible that you are mining to a node that is catching up to the rest of the network. Before you start mining to a node, you SHOULD verify that this node has the most up-to-date cut.

You can get the cut height of any node by running the following:

```text
$ curl -sk https://<bootstrap-node-url>/chainweb/0.0/mono/cut | jq '.height'
```

## Mine for a Chainweb Network

Successful mining on a production profile requires specialized hardware (ASIC). The setup for solo mining involves running a chainweb-node with a configuration that enables mining and a [chainweb-mining-client](https://github.com/vootaa/chainweb-mining-client/) that connects to the mining API of a chainweb-node and provides a Stratum API for the mining hardware (ASIC).

Detailed instructions for setting up all the infrastructure needed to start
mining using `docker compose` can be found in the documentation of [docker-compose-chainweb-node/mining-node](https://github.com/vootaa/docker-compose-chainweb-node/tree/main/mining-node).

For example, to set up a chainweb node for mining, see [this](https://github.com/vootaa/docker-compose-chainweb-node/blob/main/mining-node/docker-compose.yaml#L126) section of the docker-compose file.

Detailed mining client instructions can be found in the documentation of
[chainweb-mining-client](https://github.com/vootaa/chainweb-mining-client/)

## Chainweb Design

### Component Structure

The Chainweb package contains the following buildable components:

    *   `chainweb` library: It provides the implementation for the different
        components of a chainweb-node.

    *   `chainweb-node`: An application that runs a Chainweb node. It maintains copies
        of a number of chains from a given Chainweb instance. It provides interfaces
        (command-line and RPC) for directly interacting with the Chainweb or for
        implementing applications such as miners and transaction management tools.

    *   `chainweb-tests`: A test suite for the Chainweb library and chainweb-node.

    *   `cwtool`: A collection of tools that are helpful for maintaining, testing,
        and debugging Chainweb.

    *   `bench`: a collection of benchmarks

### Architecture Overview

![Architecture Overview](docs/Overview.png)
