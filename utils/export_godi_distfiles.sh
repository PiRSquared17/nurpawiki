#!/bin/sh

set -e

# Extract GODI .tar.gz and distinfo from current SVN head

if [ "$VERSION" = "" ]; then
    echo "Must have NURPAWIKI VERSION in environment"
    exit 1
fi

svn export trunk nurpawiki-$VERSION

tar cf nurpawiki-$VERSION.tar nurpawiki-$VERSION
gzip nurpawiki-$VERSION.tar

sha1=`sha1sum nurpawiki-$VERSION.tar.gz|cut -d " " -f 1`
sz=`cat nurpawiki-$VERSION.tar.gz|wc -c`

echo "\$NetBSD\$" > distinfo
echo "" >> distinfo
echo "SHA1 (nurpawiki-$VERSION.tar.gz) = $sha1" >> distinfo
echo "Size (nurpawiki-$VERSION.tar.gz) = $sz bytes" >> distinfo
