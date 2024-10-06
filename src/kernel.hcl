class Apex {
	fun(#class) basicNew(size) {
		return (core.basicNew self size)
	}

	fun(#class) respondsTo(mth) {
		return (core.classRespondsTo self mth)
	}

	fun respondsTo(mth) {
		return (core.instRespondsTo self mth)
	}

	fun primAt(pos) {
		return (core.primAt self pos)
	}

	fun primtAtPut(pos value) {
		return (core.primAtPut self pos value)
	}

	fun basicAt(pos) {
		return (core.basicAt self pos)
	}

	fun basicAtPut(pos value) {
		return (core.basicAtPut self pos value)
	}

	fun basicSize() {
		return (core.basicSize self)
	}
}

class Object: Apex {
}

class(#uncopyable #varying #limited) Class: Apex [
	_name
	_mdic
	_spec
	_selfspec
	_superclass
	_nivars_super
	_ibrand
	_ivarnames
	_cvarnames
] {
	fun name() {
		##return (core.className self)
		return _class
	}

	fun instanceVariableNames() {
		## TODO: this still returns nil as the acutal manipulation of the field has not been implemented
		return _ivarnames
	}

	fun classVariableNames() {
		## TODO: this still returns nil as the acutal manipulation of the field has not been implemented
		return _cvarnames
	}
}

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
			while (i < size) {
				core.basicAtPut obj i iv
				i := (i + 1)
			}
		}
		return obj
	}

	##fun ::initValue() {
	##	return nil
	##}
}

class Array: FixedSizedCollection {
}

class String: FixedSizedCollection {
	fun(#class) initValue() {
		##return '\0'
		return ' '
	}
}


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

printf "[%O]\n" (String:new 5)
printf "[%O]\n" (String:basicNew 5)

printf "[%O]\n" (String:respondsTo "new")
printf "[%O]\n" (String:respondsTo "newx")
printf "[%O]\n" (" ":respondsTo "new")
printf "[%O]\n" (" ":respondsTo "length")

##printf "[%O]\n" (String:classVariableNames)
##printf "[%O]\n" (String:instanceVariableNames)

##printf "%O\n" #"abcdefg"


printf "----------------------------------------\n"

k := #[1 2 3]
printf "%O\n" (k:basicAt 2)

class Z: Object [ a b c ] {
	fun(#classinst) new() {
		self.a := 10
		self.b := 20
		self.c := 30
	}
}
##k := (Z:basicNew 0)
k := (Z:new)
printf "%O\n" (k:basicAt 2)



