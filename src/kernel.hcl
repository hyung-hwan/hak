class Apex {
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


printf "string length %d\n" ("aaaa":length)
printf "substring [%s]\n" ("abcdefghijklmn":slice 5 6)
printf "substring [%c]\n" ("abcdefghijklmn":at 14)
