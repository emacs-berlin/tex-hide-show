#!/bin/sh

# Author: Andreas Röhler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

# This script tests functions from ar-mode.el.

# Code:

ORT=${ORT:-$1}

echo "\$ORT: $ORT"

if [ $ORT -eq 0 ]; then
    EMACS=emacs
elif [ -s emacs24 ]; then
    EMACS=emacs24
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

FILE1=tex-hide.el


TEST1=test/tex-hide-setup-ert-tests.el
TEST2=test/tex-hide-tests.el

hier () {
    $EMACS -Q --batch \
-load $FILE1 \
-load $TEST1 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
-load $FILE1 \
-load $TEST1 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

if [ $ORT -eq 0 ]; then
    hier
else
    echo "Lade Testumgebung \"entfernt\""
    entfernt
fi

