#!/usr/bin/evn python3
"""Process files in an 'incoming' folder by adding them to CouchDB.

This script, which is intended to be run via a cron job, examines the
contents of a particular directory, looking for content to add to CouchDB.
The parent directories of any files found are used to assign tags to the
blobs once the files are stored in CouchDB.

For example, at midnight the script finds that the directory
/zeniba/shared/incoming contains these directories and files:

joseph_photos_birthday_party/img1.jpg
joseph_photos_birthday_party/img2.jpg
joseph_photos_birthday_party/img3.jpg
christina_photos_school_fieldtrip/image01.jpg
christina_photos_school_fieldtrip/image02.jpg

The first three would be stored in CouchDB with the tags "joseph",
"photos", "birthday", "party", while the last two would have the tags
"christina", "photos", "school", "fieldtrip".

Also use the original file name as one of the tags, to facilitate searching
for blobs by filename and/or extension.

As each file is processed, remove it from the file system, then delete the
empty parent directory.

Be sure to ignore all dot files (the Mac creates many).

"""

#
# TODO: set up CouchDB on the Mac and file server
# TODO: finish reading http://howistart.org/posts/erlang/1
# TODO: get the https://github.com/benoitc/couchbeam library
# TODO: code the thing to do the stuff
# TODO: delete this silly script once the Erlang version gets off the ground
#


def main():
    """Do the thing."""
    pass


if __name__ == "__main__":
    main()
