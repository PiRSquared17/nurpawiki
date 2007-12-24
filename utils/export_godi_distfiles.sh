#!/bin/sh

set -e

wd=`dirname $0`

# Extract GODI .tar.gz and distinfo from current SVN head

if [ "$VERSION" = "" ]; then
    echo "Must have Nurpawiki VERSION in environment"
    echo "For releases, this should be trunk/VERSION"
    exit 1
fi

svn export trunk nurpawiki-$VERSION

tar cf nurpawiki-$VERSION.tar nurpawiki-$VERSION
gzip nurpawiki-$VERSION.tar

$wd/gen_godi_distinfo.sh
