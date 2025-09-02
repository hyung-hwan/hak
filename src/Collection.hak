
class Collection: Object {
	fun length() {
		return (core.basicSize self)
	}
}

class IndexedCollection: Collection {
	fun slice(index count) {
		return (core.slice self index count)
	}

	fun at(index) {
		return (core.basicAt self index)
	}

	fun atPut(index value) {
		return (core.basicAtPut self index value)
	}
}

class FixedSizedCollection: IndexedCollection {
	fun(#class) new(size) {
		| obj iv |
		obj := (core.basicNew self size)
		if (self:respondsTo "initValue") { ## TODO: change "initValue" to a symbol once supported
			i := 0
			iv := (self:initValue)
			while (< i size) { ## TODO: change to i < size after having implemented these methods on integer/smallintger
				core.basicAtPut obj i iv
				i := (+ i 1) ## TODO: change to i + 1 ## TODO: change to i < size after having implemented these methods on integer/smallintger
			}
		}
		return obj
	}

	##fun ::initValue() {
	##	return nil
	##}
}

class(#varying) Array: FixedSizedCollection {
}

class(#char #varying) String: FixedSizedCollection {
	fun(#class) initValue() {
		##return '\0'
		return ' '
	}
}

class SequenceableCollection: Collection {
	fun iterate (action) {
		| i x |
		i := 0; x := (self:size)
		while (i < x) {
			action:value (self:at i)
			i := (+ i 1)
		}
	}
}
