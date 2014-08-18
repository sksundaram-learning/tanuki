# Development Setup

## Prerequisites

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
$ ./runfab install_erlang
$ ./runfab install_couchdb
```

## Prototype Incoming Processor

### Python 2.7

* Install Python 2.7.x.
* Install https://github.com/djc/couchdb-python
* Install https://github.com/ianare/exif-py
