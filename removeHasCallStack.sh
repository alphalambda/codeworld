#!/bin/bash

cwd=$(pwd)

cd ${cwd}/web/doc

for f in *.html
do
    mv $f $f.pre
    sed -e 's/HasCallStack =&gt; //g' \
        < $f.pre > $f
    rm $f.pre
done

