class Object {
}

class Collection : Object {
}

class IndexedCollection: Collection {
}

class FixedSizedCollection: Collection {
}

class Array: FixedSizedCollection {
}

class String: Array {
}

fun Collection:length() {
	return (arr.length self)
}

fun Collection:slice(index count) {
	return (arr.slice self index count)
}



class String: Array (a b c) {
}

class String: Array (
	monaco
	duncan
	falcon
	:deuce
	:canival
	:pebble
	:godzilla
) {
	fun Collection:slice(index count) {
		return (arr.slice self index count)
	}
}


printf "string length %d\n" ("aaaa":length)
