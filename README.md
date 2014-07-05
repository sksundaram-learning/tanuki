# Tanuki

A system for importing, storing, categorizing, browsing, displaying, and
searching files, primarily photos and videos. Attributes regarding the files are
stored in a schema-less database. Designed to store millions of files. Primary
interface is a web front-end designed for use by a web browser.

## Requirements

* _Should_ run on any operating system
* _Must_ run on Solaris derivatives
* Data liberty: _must_ not use proprietary, cryptic data formats
    * Eschew anything that will be unreadable in 40 years
    * Loss of meta-data should not result in loss of file data
        * i.e. should be able to take raw files and make sense of them
    * Database corruption should not result in the loss of everything
* _Should_ prevent duplication of assets
* _Should_ use efficient blob storage
    * Store blobs on file system as most of these files are images and videos
        * Anything over 1mb is simply unreasonable for a database
        * See [Large Object Storage in a Database or a Filesystem?](http://research.microsoft.com/pubs/64525/tr-2006-45.pdf)
    * Path and filename based on checksum for even distribution
    * Scheme should work well with ZFS
* _Must_ include sufficient details regarding each document:
    * created_at: UTC datetime of file ctime
        * use file:read_file_info()
    * tags: comma-separated labels (e.g. photos,joseph,birthday,party)
    * mimetype: string (e.g. image/jpeg)
    * filename: at time of import
* _Should_ incorporate EXIF information from incoming photos into document fields
    * Original datetime
    * Use [erlang-exif](https://github.com/andrenth/erlang-exif)
* _Should_ incorporate MPEG-7 information, if available
* Removal of assets _must_ be an explicit operation; no garbage collection
    * Unlike Camlistore, nothing needs to be "kept" in order to persist indefinitely
* Web interface _must_ show tag cloud as well as browse files by date

## Design

* Store the binary files like Git does, using checksums for names on disk.
    * Split the sha256 into directory path and filename
    * For 1b files, divide into 256 folders within 256 folders for ~15k per leaf folder
    * e.g. 59/9e/263cd69343fd822ee51539d7aacbe54b4f0482de8d6e4ea473b1d8adbad0
* Files are therefore content-addressable and automatically de-duplicated.
* Let CouchDB handle the metadata storage, indexing, and querying
    * CouchDB does not handle large binary data efficiently;
    * base64 encodes attachments and stores directly in the db file.

### Incoming Processor

* Scan a configured directory for files
* Files must reside in directories
* Directory names form the blob tags: e.g. joseph_birthday_photos -> "joseph,birthday,photos"
* Directory name to tag conversion: allow underscore, comma, period, hash (others?)
* Stores blobs on file system in sharded directories
* Store metadata in database (i.e. CouchDB)
    * Use [couchbeam](https://github.com/benoitc/couchbeam)
* Write metadata to an append-only log in JSON format as a precaution against data loss
    * At certain size threshold, can compress and archive log file
    * At ~300 bytes of metadata per blob, 1m entries is 300mb

### Importer

* One-off app that traverses directory structure and imports files
* Use file path to generate tags
    * Use patterns from a config file to avoid hard-coding
    * e.g. pictures/kids/... -> "photos,kids"
    * e.g. movies/events/2014/birthday -> "videos,birthday"

### Browser

* Create views for blobs based on tags and dates; these are stored in CouchDB
    * See the [view docs](http://docs.couchdb.org/en/latest/couchapp/views/intro.html)

## TODO

1. Convert the requirements into a wiki page
1. Write design documents in the form of wiki pages
1. Use rebar to configure a single project with multiple apps
    * incoming: processes files dumped in an "incoming" folder
    * browser: web interface that displays and filters files
    * importer: run once to load files from existing directory structure
1. Convert `devscripts/camlistore/incoming.py` into an Erlang app
