#!/bin/sh
$@ | grep -E ^ERROR: && exit 1
exit 0
