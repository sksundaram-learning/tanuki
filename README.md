# Tanuki

## Design

* Store the binary files like Git does, using checksums for names on disk
* Files are therefore content-addressable and automatically deduplicating
* Let CouchDB handle the metadata, indexing, and querying
* Of course, if CouchDB can store binaries in a reasonable way, let it

## TODO

1. Install CouchDB on my Mac and set up a proof-of-concept
    * Using curl and the HTTP interface, store images with tags
1. Evaluate CouchDB as a photo and video storage system
    * How does it fair in terms of storing large binaries?
    * Camlistore is a good fit, but does not build on Solaris
1. Test installation of CouchDB on Solaris
1. Use rebar to configure a single project with multiple apps
    * incoming: processes files dumped in an "incoming" folder
    * viewing: web interface that displays and filters files
1. Convert `devscripts/camlistore/incoming.py` into an Erlang app

## CouchDB Evaluation

* Would need to configure `couchdb.max_document_size` to handle files
  larger than 4GB
