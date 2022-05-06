#!/bin/sh
$@ 2>&1 | grep -Eq ^ERROR: && exit 1
exit 0
