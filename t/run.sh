#!/bin/sh
$@ 2>&1 | grep -E ^ERROR: && exit 1
exit 0
