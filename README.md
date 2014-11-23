# Tanuki

A system for importing, storing, categorizing, browsing, displaying, and searching files, primarily images and videos. Attributes regarding the files are stored in a schema-less, document-oriented database. Designed to store millions of files. Primary interface is a web front-end designed for use by a web browser.

Yes, another one of these. I am a programmer, it's what I do. I want to learn Erlang and this is a good project with which to learn Erlang and OTP. See the Requirements wiki for additional technical reasons.

## Current Status

The incoming processor is written in Python, and will eventually be rewritten in Erlang.

The backend application and web interface are being written in Erlang and built using [rebar](https://github.com/rebar/rebar/) and [relx](http://relx.org/).

## Building and Testing

Install Erlang/OTP R17 and CouchDB, then build the project like so:

```
$ rebar -r prepare-deps
$ make dev
$ ./_rel/tanuki/bin/tanuki-dev
```

The web server will be listening on port 8000. Be sure to have a CouchDB instance running.

## TODO

A lot. See the issues and `TODO.md` for pending tasks.

## Implementation Details

* Everything written in Erlang because I want to learn it thoroughly.
* Use `file:read_file_info()` to read file mtime information.
* Use `crypto:hash(sha256, Data)` to generate the SHA256 checksum of the assets.
* Use [spawngrid/mimetypes](https://github.com/spawngrid/mimetypes) for detecting mime types.
* Use [erlang-exif](https://github.com/andrenth/erlang-exif) for reading EXIF data.
* Use [couchbeam](https://github.com/benoitc/couchbeam) for CouchDB interface.
