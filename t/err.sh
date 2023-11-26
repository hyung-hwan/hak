#!/bin/sh

for i in $@; do :; done
script="$i"

expected_errinfo=$(grep -E "##[[:space:]]+ERROR:" "$script" 2>/dev/null)
[ -z "$expected_errinfo" ] && {
	echo "INVALID TESTER - $script contains no ERROR information"
	exit 1
}

expected_errline=$(echo $expected_errinfo | cut -d: -f1)
xlen=$(echo $expected_errline | wc -c)
xlen=$(expr $xlen + 2)
expected_errmsg=$(echo $expected_errinfo | cut -c${xlen}-)

output=$($@ 2>&1) 
echo "$output" | grep -E "ERROR:.+${script}.+${expected_errmsg}" || {
	echo "$script - $output"
	exit 1
}
exit 0
