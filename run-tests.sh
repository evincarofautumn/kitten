#!/bin/bash

function run_warn {
    set +e +E
    pass=true

    diff -u --strip-trailing-cr -- "./test/$1.warn" "./build/test.warn/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    if ! $pass ; then
	echo "[FAILED] $1"
	exit 1
    fi

    set -e -E
}

function run_test {
    set +e +E
    pass=true

    "./build/test/$1" > "./build/test.out/$1" 2> "./build/test.err/$1"

    diff -u --strip-trailing-cr -- "./test/$1.out" "./build/test.out/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    diff -u --strip-trailing-cr -- "./test/$1.err" "./build/test.err/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    if ! $pass ; then
	echo "[FAILED] $1" >&2
	exit 1
    fi

    set -e -E
}

for file in ./build/test.warn/* ; do
    run_warn $(basename "$file")
done

for file in ./build/test/* ; do
    run_test $(basename "$file")
done

echo 'All tests passed! :)'
