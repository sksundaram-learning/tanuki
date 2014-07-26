# TODO

## Web UI

### Getting Started

1. Figure out where a Nitrogen app fits within the rebar application framework.
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

## Incoming Processor

### Implementation Details

* Be sure to write thorough unit tests to guard against accidental data loss. The data loader is the most fragile in the system because it adds records to the database and moves files in the file system. These need to be performed as an atomic transaction.
    * Attempt to move the asset into place first; if that fails stop immediately.
    * If the attempt to insert the document into the database fails, revert the asset move.

### Installation and Configuration

* Having a `tanuki` user is a good idea for file ownership and permissions.
* The incoming processor and web stack should run as the tanuki user.
