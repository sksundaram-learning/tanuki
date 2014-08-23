# TODO

## Overall

* Use [concrete](https://github.com/opscode/concrete) for dev-only dependencies
* Use [PropEr](http://proper.softlab.ntua.gr) for property-based testing

## Web UI

### Getting Started

1. Nitrogen has trouble finding the templates in `priv/templates`
1. Get this running in the Solaris VM
1. Document the procedure, possibly update the `fabfile.py` script
1. Code up a simple prototype backend for tanuki assets (see basic operations below).
1. Code up a front page for an overview of what is stored in tanuki.

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
