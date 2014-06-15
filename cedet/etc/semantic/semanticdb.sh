#!/bin/bash
# semanticdb.sh --- Build a semantic cache for input arguments
#
# Copyright (C) 2002, 2004 Eric M. Ludlam
#
# Author: Eric M. Ludlam <zappo@gnu.org>
# Keywords: tags
# X-RCS: $Id: semanticdb.sh,v 1.3 2005-09-30 20:19:32 zappo Exp $
#
# This file is not part of GNU Emacs.
#
# Semanticdb is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.
# 
# Commentary:
#
# Emacs usually builds a semantic cache on the fly.  If you want to use
# a tool that accesses database files without having to visit all the files, 
# however, you should use this script BEFORE starting Emacs.
#
# If you move this script from its original location, you have to set
# CEDET_PATH to the CEDET directory.

if [ -z "$1" ]; then
    echo "Usage: `basename $0` list-of-files+"
    exit 1
fi

if [ -z "$CEDET_PATH" ]; then
    echo CEDET_PATH not set. Guessing path from script source.
    loadpath="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
else
    loadpath="$SEMANTIC_PATH"
fi

echo Using CEDET path $loadpath

if [ -z "$EMACS" ]; then
    emacs="emacs"
else
    emacs="$EMACS"
fi

exec $emacs -batch -l "${loadpath}/lisp/cedet/semantic/db-mk.el" "$@"

#end
