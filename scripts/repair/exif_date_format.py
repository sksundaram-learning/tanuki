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
"""Fix the EXIF date format in tanuki documents.

Initially the incoming processor prototype created CouchDB documents
in which the EXIF date looked like "2005:08:21 19:19:49", but what
was desired was something more like "2005-08-21 19:19". This script
converts the EXIF dates in an existing tanuki database.

"""

from datetime import datetime
import sys

import couchdb


def _connect_couch():
    """Connect to CouchDB or die trying.

    Returns the couchdb.Server instance.

    """
    couch = couchdb.Server()
    try:
        couch.version()
    except ConnectionRefusedError:
        sys.stderr.write("Unable to connect to CouchDB at default location.")
        sys.exit(1)
    return couch


def _convert_exif_dates(couch):
    """Change the format of the EXIF date fields in the database.

    :param couch: instance of couchdb.Server.

    """
    db = couch['tanuki']
    # Yes, a view written in JavaScript and run on the database would be
    # more efficient, but this is a one-off script and there are only a few
    # documents right now. And most of those documents have an EXIF date
    # anyway. For anything larger, really should use a JavaScript view to
    # select the desired documents.
    for row in db.view('_all_docs'):
        doc = db.get(row.id)
        exif_date = doc['exif_date']
        if exif_date is not None:
            date = datetime.strptime(exif_date, '%Y:%m:%d %H:%M:%S')
            exif_date = datetime.strftime(date, '%Y-%m-%d %H:%M')
            doc['exif_date'] = exif_date
            db[doc.id] = doc


def main():
    """Process incoming assets according to provided arguments."""
    couch = _connect_couch()
    _convert_exif_dates(couch)


if __name__ == "__main__":
    main()
