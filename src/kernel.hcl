class Apex {
	fun :: basicNew(size) {
		return (core.basic_new self size)
	}
}

class Object :: Apex {
}

class Collection :: Object {
}

class IndexedCollection :: Collection {
}

class FixedSizedCollection :: IndexedCollection {
}

class Array :: FixedSizedCollection {
	fun :: new(size) {
		return (core.basic_new self size)
	}	
}

class String :: FixedSizedCollection {
}

fun Collection:length() {
	return (core.length self)
}

fun Collection:slice(index count) {
	return (core.slice self index count)
}

fun Collection:at(index) {
	return (core.get self index)
}

fun Collection:atPut(index value) {
	return (core.put self index value)
}

fun Class:name() {
	return (core.class_name self)
}

##class String:: Array [a b c] {
##}

##class String:: Array [
##	monaco
##	duncan
##	falcon
##	deuce
##	canival
##	pebble
##	godzilla
##] {
##	fun Collection:slice(index count) {
##		return (arr.slice self index count)
##	}
##}


k := "abcdefghijklmn"
printf "string length %d\n" ("aaaa":length)
printf "substring [%s]\n" (k:slice 5 6)

try {
	printf "substring [%c]\n" (k:at 13)
	k:atPut 14 'A'
	printf "[%s]\n" k
} catch (e) {
	printf "EXCEPTION %O\n" e
}

k := #[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]

try {
	k:atPut 2 'A'
	printf "%O\n" k
} catch (e) {
	printf "EXCEPTION %O\n" e
}

k := #b[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
try {
	k:atPut 2 -10
	printf "%O\n" k
} catch (e) {
	printf "EXCEPTION %O\n" e
}


k := (Array:new 10)
k:atPut 3 "hello"
printf "%O\n" k
