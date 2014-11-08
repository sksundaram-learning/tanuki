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
"""Change the date fields format from a string to a list of numbers.

Initially the incoming processor prototype created CouchDB documents in
which the date fields were simply strings. Instead, they should be an array
of numbers, where individual members are part of a timestamp in decreasing
significance. For example, [2014, 11, 8, 14, 40] represents 2:40pm on
November the 8th of 2014.

"""

import re
import sys

import couchdb

DATE_REGEX = re.compile(r'(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})')


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


def _string_to_ints(value):
    """Convert the date string into an array of ints.

    :type value: str
    :param value: date string to convert

    :return: list of ints, or None if value does not match date format.

    """
    match = DATE_REGEX.match(value)
    if match is None:
        return None
    result = []
    for part in match.groups():
        result.append(int(part))
    if not result:
        raise RuntimeError('failed to match date value {}'.format(value))
    return result


def _convert_tag_values(couch):
    """Change the format of the date fields in the database.

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
        dirty = False
        for field_name in ['exif_date', 'file_date', 'import_date']:
            field = doc[field_name]
            if isinstance(field, (str, unicode)):
                doc[field_name] = _string_to_ints(field)
                dirty = True
        if dirty:
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
