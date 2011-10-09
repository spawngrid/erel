# Erel
## Better way to handle your Erlang releases

Erel will be a better way to handle Erlang releases in small to medium sized clusters.

It will use AMQP (RabbitMQ) to communicate membership, handle configuration, push out updates, configure hosts and so on as well as facilitate automatic updates.

## Building blocks
Erel a collection of three components; erel, which manages the application, the erel\_master, which manages a host and RabbitMQ which connects together many erel\_masters to form a cluster.

### erel_master
Each host (a server) within an erel cluster setup has a erel_master application running. This application does not contain the releases but has a manages a number of erel-enabled releases.
Each erel_master has a an identifier which is used to identify which releases it should manage setup and manage. They can be group together.

### erel
For an release to work in an erel cluster it needs to include the erel application. The erel application is responsible for starting the contents of it's release.
The erel and erel_master communicate over Erlang RPC.

### RabbitMQ (AMQP)
RabbitMQ is used to connect erel_masters together and is used to distribute configuration.

## Configuration management
As an erel-enable cluster does not have a central point for configurations they need to distributed to all erel_masters.
These configurations define how a group of erel_master behaves, which releases it should start etc.

### Publishing configurations
1. A configurationis sent to RabbitMQ that fans the data out to every erel_master.
2. If the configurationis relevant to the erel_master it will be executed, e.g. updating a running erel.
3. The configurations are written to disk.

### Adding a new host
1. The new erel_master connects to RabbitMQ.
2. When a new erel_master is added it requests the configurations from the cluster.
3. Once it has received the configurations it will discover which group it belongs to and start setting up the erels.
4. If it does not belong to any group, it will wait until a configuration is pushed to the cluster that matches it.

## erel_master API
The erel_master has an API to interact with the erels;
* Start
* Restart
* Stop
* Status
* Delete (?)