#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# -------------------------------------------------------------------
#
# Copyright (c) 2014 Nathan Fiedler
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#
# -------------------------------------------------------------------
"""Process files in an 'incoming' folder by adding them to CouchDB.

This script, which is intended to be run via a cron job, examines the
contents of a particular directory, looking for content to add to CouchDB.
The parent directories of any files found are used to assign tags to the
blobs once the files are stored in CouchDB.

See the project home page for details: https://github.com/nlfiedler/tanuki

Prerequisites:
* Python 2.7 -- the EXIF libraries all seem to fail miserably on Python 3.x
* couchdb -- https://github.com/djc/couchdb-python
* exifread -- https://github.com/ianare/exif-py

"""

import argparse
from datetime import datetime
import hashlib
import mimetypes
import os
import pwd
import re
import socket
import sys

import couchdb
import exifread

_DB_NAME = 'tanuki'
_EXIF_DATETIME = 'EXIF DateTimeOriginal'
_DATETIME_FORMAT = '%Y-%m-%d %H:%M'
_EXTRANEOUS_FILES = ['.DS_Store', '.localized']
_DATE_REGEX = re.compile(r'(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})')


def _connect_couch():
    """Connect to CouchDB or die trying.

    Returns the couchdb.Server instance.

    """
    couch = couchdb.Server()
    try:
        couch.version()
    except socket.error:
        sys.stderr.write("Unable to connect to CouchDB at default location.")
        sys.exit(1)
    return couch


def _create_database(couch):
    """Create the database for our assets if not already present.

    :param couch: couchdb.Server instance.

    Returns the couchdb.Database instance.

    """
    if _DB_NAME not in couch:
        db = couch.create(_DB_NAME)
    else:
        db = couch[_DB_NAME]
    return db


def _process_path(dirpath, db, destpath):
    """Import the assets found with the named path.

    :param dirpath: path to incoming assets.
    :param db: instance of couchdb.Database.
    :param destpath: path to asset storage area.

    """
    utcnow = datetime.utcnow()
    importdate = _date_string_to_ints_list(utcnow.strftime(_DATETIME_FORMAT))
    asset_folder = os.path.basename(os.path.normpath(dirpath))
    tags = asset_folder.lower().split('_')
    for entry in os.listdir(dirpath):
        filepath = os.path.join(dirpath, entry)
        if entry in _EXTRANEOUS_FILES:
            # Remove the superfluous files that Mac likes to create.
            os.unlink(filepath)
        elif entry[0] == '.':
            # Ignore all other hidden entries.
            continue
        elif os.path.isfile(filepath):
            checksum = _compute_checksum(filepath)
            doc = dict()
            doc['exif_date'] = _date_string_to_ints_list(_get_original_date(filepath))
            doc['file_date'] = _date_string_to_ints_list(_file_date(filepath))
            doc['file_name'] = entry
            doc['file_owner'] = _file_owner(filepath)
            doc['file_size'] = os.stat(filepath).st_size
            doc['import_date'] = importdate
            mimetype = mimetypes.guess_type(filepath)
            doc['mimetype'] = mimetype[0] if mimetype else None
            doc['sha256'] = checksum
            doc['tags'] = tags
            doc_id, doc_rev = db.save(doc)
            print("{} => id={}, rev={}".format(entry, doc_id, doc_rev))
            _store_asset(filepath, checksum, destpath)
        else:
            print("Ignoring non-file {}".format(entry))
    if len(os.listdir(dirpath)) == 0:
        os.rmdir(dirpath)
    else:
        print("Unable to remove non-empty directory: {}".format(asset_folder))


def _compute_checksum(filepath):
    """Compute the SHA256 for the named file.

    :param filepath: path to file to be read.

    Returns the hex string of the computed checksum.

    """
    h = hashlib.new('sha256')
    with open(filepath, 'rb') as fobj:
        while True:
            data = fobj.read(4096)
            if not data:
                break
            h.update(data)
    return h.hexdigest()


def _file_owner(filepath):
    """Return the username of the file owner.

    :param filepath: path to file to be examined.

    Returns the username of the file owner, of None if not available.

    """
    s = os.stat(filepath)
    p = pwd.getpwuid(s.st_uid)
    return p[0] if p else None


def _file_date(filepath):
    """Return the mtime of the file, which is typically when it was created.

    :param filepath: path to file to be examined.

    """
    s = os.stat(filepath)
    d = datetime.utcfromtimestamp(s.st_mtime)
    return d.strftime(_DATETIME_FORMAT)


def _get_original_date(filepath):
    """Attempt to read the original datetime from the EXIF tags.

    :param filepath: path to file to be read.

    Returns a datetime string (e.g. '2005-08-21 19:19') or None.

    """
    value = None
    with open(filepath, 'rb') as fobj:
        tags = exifread.process_file(fobj)
        if _EXIF_DATETIME in tags:
            value = tags[_EXIF_DATETIME].values
            date = datetime.strptime(value, "%Y:%m:%d %H:%M:%S")
            value = datetime.strftime(date, _DATETIME_FORMAT)
    return value


def _store_asset(filepath, checksum, destpath):
    """Move the named asset to its sharded location.

    :param filepath: path to file to be read.
    :param checksum: the checksum of the file, becomes its new name and path.
    :param destpath: path to the storage area.

    If an existing asset with the same checksum already exists, the new
    asset will be ignored.

    """
    dirname = os.path.join(destpath, checksum[0:2], checksum[2:4])
    if not os.path.exists(dirname):
        os.makedirs(dirname)
    newpath = os.path.join(dirname, checksum[4:])
    if not os.path.exists(newpath):
        os.rename(filepath, newpath)


def _date_string_to_ints_list(value):
    """Convert the date string into a list of ints.

    Example: "2014-07-04 12:01" -> [2014, 7, 4, 12, 1]

    :type value: str
    :param value: date string to convert

    :return: list of ints, or None if value does not match date format.

    """
    match = _DATE_REGEX.match(value)
    if match is None:
        return None
    result = []
    for part in match.groups():
        result.append(int(part))
    return result


def _revert_all(couch, frompath, topath):
    """Put everything back the way it was.

    :param couch: instance of couchdb.Server.
    :param frompath: path of stored assets.
    :param topath: where to put the files with their original names.

    Does not handle filename conflicts. This is only intended for reversing
    an import that was performed while testing. Presumably the database
    contains only a small number of documents and thus file names would not
    be in conflict.

    """
    db = couch[_DB_NAME]
    for row in db.view('_all_docs'):
        doc = db.get(row.id)
        filename = doc['file_name']
        sha256 = doc['sha256']
        fp = os.path.join(frompath, sha256[0:2], sha256[2:4], sha256[4:])
        tp = os.path.join(topath, filename)
        os.rename(fp, tp)
        print("{} => {}".format(filename, topath))
        db.delete(doc)


def main():
    """Process incoming assets according to provided arguments."""
    parser = argparse.ArgumentParser(description="Import assets to tanuki.")
    parser.add_argument("-p", "--path", required=True,
                        help="base path of incoming assets")
    parser.add_argument("-d", "--dest", required=True,
                        help="base path of asset storage area")
    parser.add_argument("-r", "--revert", action='store_true',
                        help="extract everything from CouchDB (oops!)")
    args = parser.parse_args()
    if not os.path.exists(args.path):
        sys.stderr.write("No such directory: {}".format(args.path))
    couch = _connect_couch()
    if args.revert:
        _revert_all(couch, args.dest, args.path)
    else:
        db = _create_database(couch)
        for entry in os.listdir(args.path):
            fullpath = os.path.join(args.path, entry)
            if os.path.isfile(fullpath):
                if entry in _EXTRANEOUS_FILES:
                    # Remove the superfluous files that Mac likes to create.
                    os.unlink(fullpath)
                else:
                    print("Ignoring file outside of tagged folder: {}".format(entry))
            elif os.path.isdir(fullpath):
                _process_path(fullpath, db, args.dest)
            else:
                print("Ignoring mystery object {}".format(entry))


if __name__ == "__main__":
    main()
