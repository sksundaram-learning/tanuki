#!/usr/bin/env python3
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
"""Change the tags field format from a string to a list of strings.

Initially the incoming processor prototype created CouchDB documents in
which the tags field was a comma-separated string of values. Instead, it
should be an array of strings.

"""

import sys

import couchdb


def _connect_couch():
    """Connect to CouchDB or die trying.

    :return: the couchdb.Server instance.

    """
    couch = couchdb.Server()
    try:
        couch.version()
    except ConnectionRefusedError:
        sys.stderr.write("Unable to connect to CouchDB at default location.")
        sys.exit(1)
    return couch


def _convert_tag_values(couch):
    """Change the format of the tags field in the database.

    :type couch: :class:`couchdb.Server`
    :param couch: API to CouchDB

    """
    db = couch['tanuki']
    # Yes, a view written in JavaScript and run on the database would be
    # more efficient, but this is a one-off script and there are only a few
    # documents right now.
    processed = 0
    observed = 0
    for row in db.view('_all_docs'):
        observed += 1
        doc = db.get(row.id)
        tags = doc['tags']
        if isinstance(tags, str):
            doc['tags'] = tags.split(',')
            db[doc.id] = doc
            processed += 1
            print('Saved document {}'.format(doc.id))
    print("Processed {} of {} documents".format(processed, observed))


def main():
    """Convert all of the documents."""
    couch = _connect_couch()
    _convert_tag_values(couch)


if __name__ == "__main__":
    main()
