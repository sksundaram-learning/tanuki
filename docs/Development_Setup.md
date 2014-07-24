# Development Setup

## CouchDB

Install CouchDB, which for Mac OS X is really easy as it is simply a native application that you manage via a graphical interface. Works just fine for development.

## Incoming Processor

### Python 2.7

* Install Python 2.7.x.
* Install https://github.com/djc/couchdb-python
* Install https://github.com/ianare/exif-py

## Web Interface

### Nitrogen

Install the [Nitrogen](http://nitrogenproject.com/) Erlang web framework.

```
$ git clone git://github.com/nitrogen/nitrogen.git
$ mkdir foobar
$ cd foobar
$ ../nitrogen/embed

*******************************************************************************
****                          Installation Complete                        ****
*******************************************************************************
There are a few manual steps you must take before you're completely set up:

1) You must make sure to start Nitrogen.  The easiest way is by
    adding `nitrogen_sup:start_link()` to your application.

2) You must make sure that the config files in etc/ are properly
   loaded. This can be done with a vm.args file if you're using a reltool
   generated release, or it can be done by adding additional -config calls to
   the commandline call that launches your app.

3) You may need to add the new packages (nitrogen_core, simple_bridge,
   nprocreg, etc) to your app's code path (the easy way is with the -pa flag
   in the `erl` call.)
```
