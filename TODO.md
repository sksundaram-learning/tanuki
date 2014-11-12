# TODO

## Overall

* Use [concrete](https://github.com/opscode/concrete) for dev-only dependencies
* Use [PropEr](http://proper.softlab.ntua.gr) for property-based testing
* Evaluate [QuickCheck](https://github.com/krestenkrab/triq) vs PropEr

## Web UI

### Getting Started

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

* Starting tanuki backend on the server
    * Use Ubuntu Upstart
    * Use erl flags: -detached

## Incoming Processor

### Features

* Runs in the same process as the backend, kicked off by a timer (c.f. timer:send_after/2)
* Log actions during import
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
