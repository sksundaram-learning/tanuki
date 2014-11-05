# TODO

## Overall

* Use [concrete](https://github.com/opscode/concrete) for dev-only dependencies
* Use [PropEr](http://proper.softlab.ntua.gr) for property-based testing
    * Also evaluate QuickCheck

## Web UI

### Getting Started

1. Change `Vagrantfile` and `fabfile.py` to use Linux VM instead of OpenIndiana.
1. Check for updated dependencies (couchbeam and hackney for certain)
1. Start writing some basic unit tests
    * Write a `tanuki_backend` module that does the send/receive to the other processes
        * gen_server uses registered names to avoid holding Pids
        * e.g. functions for querying data from database
    * Query documents in database
    * Collect unique set of tags in database documents
    * Find documents by tag
1. Code up a simple prototype backend for tanuki assets (see basic operations below).
1. Code up a front page for an overview of what is stored in tanuki.
1. Sync scanner is having issues; probably not compatible with relx
    * Probably need a 'dev' mode that does not use the release build
1. Use ETS to cache the tags and such in memory of `tanuki_backend` process
    * Cache invalidation via couchbeam change listeners
1. Look at https://github.com/sylane/erod rel/reltool.config
    * Use of overlay to copy scripts into release directory may be useful

### Prototype

1. Design application for querying tanuki data store
    * Query tags
    * Query dates
    * Document details (e.g. path to asset)
1. Connect web front-end to the backend service
    * Display available tags
    * Display available dates (year, then months, then days?)
    * Display assets by tag
    * Display assets by date (with pagination?)
    * Display a single asset

### Implementation Details

* Starting tanuki backend on file server
    * Use SMF
    * Use erl flags: -detached

## Incoming Processor

### Features

* Send a daily email report of everything that was imported
    * Include the names of files and their checksums
    * Organize by tags

### Implementation Details

* Be sure to write thorough unit tests to guard against accidental data loss. The data loader is the most fragile in the system because it adds records to the database and moves files in the file system. These need to be performed as an atomic transaction.
    * Attempt to move the asset into place first; if that fails stop immediately.
    * If the attempt to insert the document into the database fails, revert the asset move.

### Installation and Configuration

* Having a `tanuki` user is a good idea for file ownership and permissions.
* The incoming processor and web stack should run as the tanuki user.

## Backend

### Configuration

Use application environment (defined with `{env [{Key, Val}]}` in `.app.src` file) to indicate the default location of a configuration file.

- Log everything to a file
- Configure logwatch to generate a daily log summary
    - Check on couchdb logs as well
    - Check on Nitrogen logs as well
- Use Fabric to automate building a testing environment
