# TODO

## Web UI

### Getting Started

1. Learn more about rebar and layout of project consisting of multiple applications.
    * Look at [howistart.org](http://howistart.org/posts/erlang/1) example
    * Move incoming.py to `scripts` directory
    * Create a stub incoming app in Erlang
    * Set up rebar and relx configuration
1. Figure out where Nitrogen app fits within that framework.
1. Code up a simple prototype backend for tanuki assets.
1. Code up a front page for an overview of what is stored in tanuki.

### Prototype

1. Design application for querying tanuki data store
    * Query tags
    * Query dates
    * Document details (e.g. path to asset)
1. Wire web app front-end to the backend
    * Display available tags
    * Display available dates (year, then months, then days?)
    * Display assets by tag
    * Display assets by date (with pagination?)
    * Display a single asset

## Incoming Processor

### Installation and Configuration

* Having a `tanuki` user is a good idea for file ownership and permissions.
* The incoming processor and web stack should run as the tanuki user.
