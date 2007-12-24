#!/bin/sh

set -e

TARBALL="nurpawiki-$VERSION.tar.gz"
if [ ! -f $TARBALL ]; then
    echo "$TARBALL does not exist!"
    exit 1
fi

# Extract GODI .tar.gz and distinfo from current SVN head

if [ "$VERSION" = "" ]; then
    echo "Must have NURPAWIKI VERSION in environment"
    exit 1
fi

sha1=`sha1sum $TARBALL|cut -d " " -f 1`
sz=`cat $TARBALL|wc -c`

echo "\$NetBSD\$" > distinfo
echo "" >> distinfo
echo "SHA1 ($TARBALL) = $sha1" >> distinfo
echo "Size ($TARBALL) = $sz bytes" >> distinfo
