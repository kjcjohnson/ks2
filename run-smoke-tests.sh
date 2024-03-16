#!/bin/sh
#
# Runs smoke tests as a quick check that the solver is working
#

# Find the build directory. Adapted from SBCL's run-sbcl.sh
this="$0"

build_directory_p(){
    [ -x "$1"/bin/ks2 ];
}

# OSX 10.8 readlink doesn't have -f
while [ -h "$this" ]; do
    # Stop resolving symlinks when $this lives in a build tree.
    # (Build trees might consist of symlinks to something that doesn't
    # follow our repo layout.)
    if build_directory_p "$(dirname "$this")"; then
	break
    fi
    # [ -h should guarantee that readlink output will be non-null
    link="$(readlink -n "$this")"
    # if absolute path
    if expr "$link" : '^/.*' > /dev/null; then
        this="$link"
    else
        this="$(dirname "$this")"/"$link"
    fi
done
root_dir="$(dirname "$this")"
test_dir="${root_dir}/t/smoke-tests"
exe="${root_dir}/bin/ks2"
failure=

#
# Basic solving tests
#
for test in "$test_dir"/*.sem; do
    echo " --> $test"
    $exe solve -s tde "$test"
    if [ $? -ne 0 ]; then
        echo " --- FAIL: $test"
        failure=true
    else
        echo " +++ $test"
    fi
done

#
# Basic benchmarking tests
#
echo " --> Benchmarking test"
$exe benchmark -s 'tde;name=Config A' -s 'tde;name=Config B' -c ';name=Core A' -c ';name=Core B' "${test_dir}/test_suite"
if [ $? -ne 0 ]; then
    echo " --- FAIL: Benchmarking test"
    failure=true
else
    echo " +++ Benchmarking test"
fi

if [ -z $failure ]; then
    echo " "
    echo "Smoke tests passed."
else
    echo " "
    echo "Some tests failed."
    exit 1
fi
