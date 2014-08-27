# Development Setup

## Prerequisites

While you could certainly perform the entire development cycle within your favorite environment, it makes some sense to use the OpenIndiana VM, to ensure everything works for the target system.

* [VirtualBox](https://www.virtualbox.org)
* [Vagrant](http://www.vagrantup.com)
* [OpenIndiana](http://openindiana.org) 151a9 Vagrant box
    * Install OpenIndiana 151a9 in VirtualBox
    * Set up according to the Vagrant instructions
        * vagrant user with insecure password
        * vagrant insecure ssh key
        * VirtualBox guest additions
    * `vagrant package ...`
* [Fabric](http://www.fabfile.org)

## Setup

```
$ cd vagrant
$ vagrant up
$ ./runfab build_all
```

## Development Cycle

As mentioned above, it is probably best to use the OpenIndiana VM for development, to ensure everything works for the target system. You could, of course, use any system on which Erlang and CouchDB are supported.

* Perform the setup for the VM as described above.
* Edit the code using your favorite editor on the host system
* From the OpenIndiana VM, run the following commands:
```
$ cd /vagrant_data
$ rebar compile
$ ./_rel/tanuki/bin/tanuki
```
* Visit [http://localhost:8000](http://localhost:8000) to see the web interface
    * Thanks to port-forwarding, you can use your favorite browser on the host system

## Prototype Incoming Processor

### Python 2.7

* Install Python 2.7.x.
* Install https://github.com/djc/couchdb-python
* Install https://github.com/ianare/exif-py
