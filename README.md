# Tanuki

A system for importing, storing, categorizing, browsing, displaying, and searching files, primarily images and videos. Attributes regarding the files are stored in a schema-less database. Designed to store millions of files. Primary interface is a web front-end designed for use by a web browser.

Yes, another one of these. I am a programmer, it's what I do. I want to learn Erlang and this is a good project with which to learn Erlang and OTP. See the Requirements wiki for additional technical reasons.

## Python Prototype

Currently a proof-of-concept system has been written appears to be working. The incoming processor, currently implemented in Python, will eventually be rewritten in Erlang. Some boilerplate Erlang applications are in place, using [rebar](https://github.com/rebar/rebar/) and [relx](http://relx.org/) to build the system. As I go through [Learn You Some Erlang](http://learnyousomeerlang.com), I will flesh out the applications.

## TODO

A lot. See the issues and `TODO.md` for pending tasks.

## Implementation Details

* Everything written in Erlang because I want to learn it thoroughly.
* Use `file:read_file_info()` to read file mtime information.
* Use [spawngrid/mimetypes](https://github.com/spawngrid/mimetypes) for detecting mime types.
* Use [erlang-exif](https://github.com/andrenth/erlang-exif) for reading EXIF data.
* Use [couchbeam](https://github.com/benoitc/couchbeam) for CouchDB interface.
