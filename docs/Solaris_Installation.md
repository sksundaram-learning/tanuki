# Solaris Installation

## Python 2.7

The prototype incoming processor script requires Python 2.7, one because I like `argparse`, and two because the EXIF library does not work with Python 3.x (see [issue #35](https://github.com/ianare/exif-py/issues/35) for details). To install, get the latest 2.7.x tarball from the Python [website](https://www.python.org) and install as follows.

```
$ ./configure
$ make
$ pfexec make install
$ pfexec rm /usr/local/bin/python
$ pfexec rm /usr/local/bin/python-config
```

To prevent the new, unversioned `python` executables from causing problems with system packages (e.g. time-slider), remove them completely from the system. Our scripts explicitly request `python2.7` anyway. Yes, yes, I understand how execution environments work, but this is my surefire way to prevent any problems.

## CouchDB

### Configuration

* Have the database files live in `/zeniba/shared/couchdb`
    * Add the following to `/usr/local/etc/couchdb/local.ini`

```
[couchdb]
database_dir = /zeniba/shared/couchdb
```

## Incoming Processor

### Dependencies

```
$ wget --no-check-certificate https://pypi.python.org/packages/source/C/CouchDB/CouchDB-0.10.tar.gz
$ tar zxf CouchDB-0.10.tar.gz
$ cd CouchDB-0.10
$ pfexec python2.7 setup.py install
$ cd ..
$ pfexec rm -rf CouchDB-0.10*
$ wget --no-check-certificate https://bootstrap.pypa.io/get-pip.py
$ pfexec python2.7 get-pip.py
$ rm get-pip.py
$ pfexec pip2.7 install exifread
```

### Cron job

* Set up a crontab entry to run the incoming.py script at midnight (_note: server uses UTC_).

```
8 0 * * * /usr/local/bin/incoming.py -p /zeniba/shared/incoming -d /zeniba/shared/tanuki
```
