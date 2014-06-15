#!/bin/bash
#
# Run a suite of tests on a CEDET distribution.
#
# 1) Set current directory to wherever a CEDET version from CVS is
#
# 2) ./testdist.sh myemacs
#
# Will test the Make build, and the ebuild build, and all the
# automated tests using the myemacs program.
#
# 3) ./testdist.sh emacs xemacs
#
# Will do the same with emacs and xemacs, and any other number of Emacsen
# you add to the command line.
#


EMACS=emacs
if [ $# -gt 0 ]; then
    EMACS=$*
fi

CEDETVER=`grep "defconst cedet-version" common/cedet.el | cut -d "\"" -f 2`

echo "Distribution Test with \"$EMACS\" of CEDET version $CEDETVER"

CEDETROOT=`pwd`

DIST=$CEDETROOT/cedet-$CEDETVER.tar.gz

if [ ! -f $DIST ]; then
    echo "Cannot find distribution file cedet-$CEDETVER.tar.gz"
    exit 1
fi

cd /tmp
mkdir -p CEDETDISTTEST
cd CEDETDISTTEST

if [ -d cedet-$CEDETVER ]; then
    echo "Deleting old CEDET dist from previous test."
    rm -rf "cedet-$CEDETVER"
fi

for i in $EMACS; do

    echo "Run DIST test with $i using make"
    tar -xvzf $DIST
    cd "cedet-$CEDETVER"
    if make EMACS=$i; then
	echo "DIST build with $i using make PASSED."
    else
	echo "DIST build with $i using make FAILED."
	exit 1
    fi
    if make EMACS=$i utest itest; then
	echo "DIST test with $i using make PASSED."
    else
	echo "DIST test with $i using make FAILED."
	exit 1
    fi
    cd ..
    rm -rf "cedet-$CEDETVER"
    
    echo "Run DIST test with $i using ebuild (via make)"
    tar -xvzf $DIST
    cd "cedet-$CEDETVER"
    if make EMACS=$i ebuild; then
	echo "DIST build with $i using make PASSED."
    else
	echo "DIST build with $i using make FAILED."
	exit 1
    fi
    if make EMACS=$i utest itest; then
	echo "DIST test with $i using make PASSED."
    else
	echo "DIST test with $i using make FAILED."
	exit 1
    fi
    cd ..
    rm -rf "cedet-$CEDETVER"

done

exit 0