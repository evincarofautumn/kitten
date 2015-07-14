#!/bin/bash

CC="${CC:-cc}"
KITTEN_DIR="$(pwd)"
cd "$(dirname "$0")"
HERE="$(pwd)"

if [ "$#" -lt 1 ]; then
  echo "Usage: run.sh /path/to/kitten TEST" >&2
  exit 1
fi

KITTEN="$1"
shift

function run_kitten {
  "$KITTEN" -L "$HERE" $*
}

CFLAGS+=" kitten.o -std=c99 -Wall -Werror -Wextra -g -I. -DNDEBUG -lm"

BLACKLISTED_C_TESTS=$(cat <<EOF
EOF
)

function contains {
  xs=$1
  shift
  y=$1
  shift
  for x in $xs; do
    if [ "$x" = "$y" ]; then
      return 0
    fi
  done
  return 1
}

function run_test {

  set +e +E

  path=$1
  shift

  mode=$1
  shift

  test_file="test/$path"
  test_name="$(basename "$test_file" ".ktn")"
  test_in="$HERE/$test_name.in"
  actual_out="$HERE/$test_name.out.$mode"
  expect_out="$HERE/$test_name.out.expect"
  actual_err="$HERE/$test_name.err.$mode"
  expect_err="$HERE/$test_name.err.expect"

  if [ ! -e "$test_in" ]; then
    test_in="/dev/null"
  fi

  if [ ! -e "$expect_err" ]; then
    expect_err="/dev/null"
  fi

  pushd "$KITTEN_DIR" > /dev/null
    if [ "$mode" = 'interpreted' ]; then
      run_kitten "$test_file" \
        < "$test_in" \
        > "$actual_out" \
        2> "$actual_err"
    elif [ "$mode" = 'c' ]; then
      if contains "$BLACKLISTED_C_TESTS" "$test_name"; then
        echo "Test '$test_name' ($mode) SKIPPED." >&2
        echo "This test is in the blacklist." >&2
        echo >&2
        return
      fi
      run_kitten -cc "$test_file" > "$test_file.c" 2> "$actual_err"
      if [ $? -ne 0 ]; then
        return
      fi
      if ! $CC "$test_file.c" $CFLAGS -o "$test_file.built" \
        > "$test_file.info" 2>&1; then
        echo "Test '$test_name' ($mode) FAILED." >&2
        echo "The generated program did not compile:" >&2
        echo >&2
        echo "Compiler Output:" >&2
        cat "$test_file.info" >&2
        echo >&2
        exit 1
      fi
      # TODO Perhaps differentiate compiletime from runtime errors?
      "$test_file.built" \
        < "$test_in" \
        > "$actual_out" \
        2> "$actual_err"
    else
      echo "Invalid test mode ($mode)." >&2
      exit 1
    fi
  popd > /dev/null

  if [ ! -e "$expect_out" ]; then
    echo "Test '$test_name' ($mode) BROKEN." >&2
    echo "Expected positive test output ($expect_out) not found." >&2
    exit 1
  fi

  diff -u "$expect_err" "$actual_err"
  if [ $? -ne 0 ]; then
    echo "Test '$test_name' ($mode) FAILED." >&2
    echo "Negative test output does not match expected." >&2
    echo >&2
    echo "Expected:" >&2
    cat "$expect_err" >&2
    echo >&2
    echo "Actual:" >&2
    cat "$actual_err" >&2
    echo >&2
    exit 1
  fi

  diff -u "$expect_out" "$actual_out"
  if [ "$?" != 0 ]; then
    echo "Test '$test_name' ($mode) FAILED." >&2
    echo "Positive test output does not match expected." >&2
    exit 1
  fi

  set -e -E

}

if [ ! -e "$KITTEN" ]; then
  echo "Unable to run tests; missing Kitten executable." >&2
  exit 1
fi

if [ $# -gt 0 ]; then
  for test in "$@"; do
    run_test "$test.ktn" 'interpreted'
    run_test "$test.ktn" 'c'
  done
else
  find . -maxdepth 1 -name '*.ktn' | while read test_file; do
    run_test "$test_file" 'interpreted'
    run_test "$test_file" 'c'
  done
fi
