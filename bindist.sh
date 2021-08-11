#!/bin/bash

set -e
set -x

key='6FEEC2227323EF85A49D54875252C5C863E5E57D'
bindir='bin'
distdir='package'

if [ "$1" == 'fed' ]; then
    mode='fed'
elif [ "$1" == 'dev' ]; then
    mode='dev'
else
    echo "Please specify mode, either 'fed' or 'dev'"
fi

version=`darcs log --last 1 \
       | head --lines 1 \
       | cut --delimiter " " --fields 2 \
       | head --bytes 10`

out="${distdir}/vervis-${mode}-${version}.tar.xz"
outsum="${out}.sha256sum"

touch 'src/Vervis/Widget.hs'

if [ "$mode" == 'fed' ]; then
    stack build --flag vervis:-dev
else
    stack build --flag vervis:dev
fi

mkdir -p "$distdir"
mkdir -p "$bindir"

cp `stack exec which vervis` "$bindir/"
cp `stack exec which vervis-post-apply` "$bindir/"
cp `stack exec which vervis-post-receive` "$bindir/"

tar --create --file - \
    'COPYING'         \
    'COPYING.AGPL3'   \
    'COPYING.CC0'     \
    'INSTALL.md'      \
    'README.md'       \
    'config/'         \
    'data/'           \
    'static/'         \
    "$bindir/"        \
    | xz --compress > "$out"

sha256sum "$out" > "$outsum"

gpg --detach-sign --armor --local-user "$key" "$outsum"

echo "Success"
