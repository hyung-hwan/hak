class Apex {
	fun isNil?() { return false }
	fun notNil?() { return true }

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

	## TODO: fun perform(name ...) {}
}

class(#uncopyable #varying #limited #final) Class: Apex [
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

class UndefinedObject: Apex {
	fun isNil?() { return true }
	fun notNil?() { return false }
}

class Object: Apex {
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


## ---------------------------------------------------------------------------------


## // TODO: consider if System can replace Apex itself.
## //       System, being the top class, seems to give very natural way of
## //       offering global system-level functions and interfaces.
## //
## //           class System { ... }
## //           class Object: System { .... }
## //           System at:  #
## //           System logNl: 'xxxxx'.
## //           System getUint8(ptr,offset)
##
class System: Apex {
	var(#class) asyncsg           ## async semaphore group
	var(#class) gcfin_sem         ## gc finalization semaphore
	var(#class) gcfin_should_exit
	var(#class) ossig_pid
	var(#class) shr               ## signal handler registry

	gcfin_should_exit := false


## ##	pooldic Log
## ##	{
## ##		// -----------------------------------------------------------
## ##		// defines log levels
## ##		// these items must follow defintions in moo.h
## ##		// -----------------------------------------------------------
## ##
## ##		DEBUG := 1,
## ##		INFO  := 2,
## ##		WARN  := 4,
## ##		ERROR := 8,
## ##		FATAL := 16
## ##	}
##
## 	## initialize class variables
## 	shr := (OrderedCollection:new)
## 	asyncsg := (SemaphoreGroup:new)
##
## 	fun(#class) _initialize {
## 		self.shr := OrderedCollection new.
## 		self.asyncsg := SemaphoreGroup new.
## 	}
##
## 	fun(#class) addAsyncSemaphore: sem {
## 		return (self.asyncsg addSemaphore: sem)
## 	}
##
## 	fun(#class) removeAsyncSemaphore: sem {
## 		return (self.asyncsg removeSemaphore: sem)
## 	}
##
## 	fun(#class) handleAsyncEvent {
## 		return (self.asyncsg wait).
## 	}
##
## 	fun(#class) installSignalHandler: block {
## 		return (self.shr addLast: block)
## 	}
##
## 	fun(#class) uninstallSignalHandler: block {
## 		self.shr remove: block.
## 	}
##
## 	fun(#class) startup(class_name, fun_name) {
## 		| class ret gcfin_proc ossig_proc |
##
## 		System gc.
##
## 		class := self at: class_name. // System at: class_name.
## 		if (class isError)
## 		{
## 			self error: ("Unable to find the class - " & class_name).
## 		}.
##
## 		self _initialize.
##
## 		// start the gc finalizer process and os signal handler process
## 		//[ self __gc_finalizer ] fork.
## 		//[ self __os_sig_handler ] fork.
## 		gcfin_proc := [ self __gc_finalizer ] newSystemProcess.
## 		ossig_proc := [ :caller | self __os_sig_handler: caller ] newSystemProcess(thisProcess).
##
## 		self.ossig_pid := ossig_proc id.
##
## 		gcfin_proc resume.
## 		ossig_proc resume.
##
## 		[
## 			// TODO: change the fun signature to variadic and pass extra arguments to perform???
## 			ret := class perform: fun_name.
## 		]
## 		ensure: [
## 			// signal end of the main process.
## 			// __os_sig_handler will terminate all running subprocesses.
## 			self _setSig: 16rFF.
## 		].
##
## 		^ret.
## 	}
##
## 	fun(#class) __gc_finalizer
## 	{
## 		| tmp gc |
##
## 		gc := false.
##
## 		self.gcfin_should_exit := false.
## 		self.gcfin_sem := Semaphore new.
## 		self.gcfin_sem signalOnGCFin. // tell VM to signal this semaphore when it schedules gc finalization.
##
## 		[
## 			while (true)
## 			{
## 				while ((tmp := self _popCollectable) notError)
## 				{
## 					if (tmp respondsTo: #finalize)
## 					{
## 						// finalize is protected with an exception handler.
## 						// the exception is ignored except it is logged.
## 						[ tmp finalize ] on: Exception do: [:ex | System longNl: "Exception in finalize - " & ex messageText ]
## 					}.
## 				}.
##
## 				//if (Processor total_count == 1)
## 				//if (Processor gcfin_should_exit)
## 				if (self.gcfin_should_exit)
## 				{
## 					// exit from this loop when there are no other processes running except this finalizer process
## 					if (gc)
## 					{
## 						System logNl: "Exiting the GC finalization process " & (thisProcess id) asString.
## 						break.
## 					}.
##
## 					System logNl: "Forcing garbage collection before termination in " & (thisProcess id) asString.
## 					self collectGarbage.
## 					gc := true.
## 				}
## 				else
## 				{
## 					gc := false.
## 				}.
##
## 				self.gcfin_sem wait.
## 			}
## 		] ensure: [
## 			self.gcfin_sem signal. // in case the process is stuck in wait.
## 			self.gcfin_sem unsignal.
## 			System logNl: "End of GC finalization process " & (thisProcess id) asString.
## 		].
## 	}
##
## 	fun(#class) __os_sig_handler: caller {
## 		| os_intr_sem signo sh |
##
## 		os_intr_sem := Semaphore new.
## 		os_intr_sem signalOnInput: System _getSigfd.
##
## 		[
## 			while (true) {
## 				until ((signo := self _getSig) isError) {
## 					// TODO: Do i have to protected this in an exception handler???
## 					//TODO: Execute Handler for signo.
##
## 					System logNl: "Interrupt detected - signal no - " & signo asString.
##
## //System logNl: "WWWWWWWWWWWWWWWWWWWWWWWWW ".
## 					// user-defined signal handler is not allowed for 16rFF
## 					if (signo == 16rFF) { goto done }.
## //System logNl: "OHHHHHHHHHHHHHH ".
##
## 					ifnot (self.shr isEmpty)
## 					{
## //System logNl: "About to execute handler for the signal detected - " & signo asString.
## 						self.shr do: [ :handler | handler value: signo ].
## 					}
## 					else
## 					{
## //System logNl: "Jumping to done detected - signal no - " & signo asString.
## 						if (signo == 2) { goto done }.
## 					}.
## 				}.
## //System logNl: "Waiting for signal on os_intr_sem...".
## 				os_intr_sem wait.
## 			}.
## 		done:
## //System logNl: "Jumped to done detected - signal no - " & signo asString.
## 			nil.
## 		]
## 		ensure: [
## 			| pid proc oldps |
##
## //System logNl: "Aborting signal handler......".
## 			// stop subscribing to signals.
## 			os_intr_sem signal.
## 			os_intr_sem unsignal.
##
## 			// the caller must request to terminate all its child processes..
##
## 			// this disables autonomous process switching only.
## 			// TODO: check if the ensure block code can trigger process switching?
## 			//       what happens if the ensure block creates new processes? this is likely to affect the termination loop below.
## 			//       even the id of the terminated process may get reused....
## 			oldps := self _toggleProcessSwitching: false.
##
## 			/*
## 			 0 -> startup  <--- this should also be stored in the "caller" variable.
## 			 1 -> __gc_finalizer
## 			 2 -> __os_sig_handler
## 			 3 ..  -> other processes started by application.
##
## 			from the second run onwards, the starting pid may not be 0.
## 			*/
## 			//proc := System _findNextProcess: self.ossig_pid.
## 			proc := System _findFirstProcess.
## 			while (proc notError)
## 			{
## 				pid := proc id.
## 				if (proc isNormal)
## 				{
## 					System logNl: ("Requesting to terminate process of id - " & pid asString).
## 					proc terminate.
## 				}.
## 				proc := System _findNextProcess: pid.
## 			}.
##
## 			System logNl: "Requesting to terminate the caller process of id " & (caller id) asString.
## 			caller terminate.  // terminate the startup process.
## 			self _toggleProcessSwitching: oldps.
##
## 			System logNl: ">>>>End of OS signal handler process " & (thisProcess id) asString.
##
## 			self.gcfin_should_exit := true.
## 			self.gcfin_sem signal. // wake the gcfin process.
##
## 			self _halting. // inform VM that it should get ready for halting.
## 		].
## 	}
##
## 	fun(#class,#primitive) _getSig.
## 	fun(#class,#primitive) _getSigfd.
## 	fun(#class,#primitive) _setSig: signo.
## 	fun(#class,#primitive) _halting.
## 	fun(#class,#primitive) _toggleProcessSwitching: v.
## 	fun(#class,#primitive,#lenient) _findProcessById: id.
## 	fun(#class,#primitive,#lenient) _findFirstProcess.
## 	fun(#class,#primitive,#lenient) _findLastProcess.
## 	fun(#class,#primitive,#lenient) _findPreviousProcess: p. // process id or process object
## 	fun(#class,#primitive,#lenient) _findNextProcess: p. // process id or process object
##
## 	fun(#class,#primitive) _popCollectable.
## 	fun(#class,#primitive) collectGarbage.
## 	fun(#class,#primitive) gc.
## 	fun(#class,#primitive) return: object to: context.
##
## 	// =======================================================================================
##
## 	fun(#class) sleepForSecs: secs {
## 		// -----------------------------------------------------
## 		// put the calling process to sleep for given seconds.
## 		// -----------------------------------------------------
## 		| s |
## 		s := Semaphore new.
## 		s signalAfterSecs: secs.
## 		s wait.
## 	}
##
## 	fun(#class) sleepForSecs: secs nanosecs: nanosecs
## 	{
## 		// -----------------------------------------------------
## 		// put the calling process to sleep for given seconds.
## 		// -----------------------------------------------------
## 		| s |
## 		s := Semaphore new.
## 		s signalAfterSecs: secs nanosecs: nanosecs.
## 		s wait.
## 	}
##
## 	// the following funs may not look suitable to be placed
## 	// inside a system dictionary. but they are here for quick and dirty
## 	// output production from the moo code.
## 	//   System logNl: 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
## 	//
## 	fun(#class,#variadic,#primitive) log(level,msg1).
##
## /*
## TODO: how to pass all variadic arguments to another variadic funs???
## 	fun(#class,#variadic) logInfo (msg1)
## 	{
## 		^self log (System.Log.INFO,msg1)
## 	}
## */
## 	fun(#class) atLevel: level log: message
## 	{
## 		<primitive: #System_log>
## 		// do nothing upon logging failure
## 	}
##
## 	fun(#class) atLevel: level log: message and: message2
## 	{
## 		<primitive: #System_log>
## 		// do nothing upon logging failure
## 	}
##
## 	fun(#class) atLevel: level log: message and: message2 and: message3
## 	{
## 		<primitive: #System_log>
## 		// do nothing upon logging failure
## 	}
##
## 	fun(#class) atLevel: level logNl: message
## 	{
## 		// the #_log primitive accepts an array.
## 		// so the following lines should work also.
## 		// | x |
## 		// x := Array new: 2.
## 		// x at: 0 put: message.
## 		// x at: 1 put: "\n".
## 		// ^self atLevel: level log: x.
##
## 		^self atLevel: level log: message and: "\n".
## 	}
##
## 	fun(#class) atLevel: level logNl: message and: message2
## 	{
## 		^self atLevel: level log: message and: message2 and: "\n".
## 	}
##
## 	fun(#class) log: message
## 	{
## 		^self atLevel: System.Log.INFO log: message.
## 	}
##
## 	fun(#class) log: message and: message2
## 	{
## 		^self atLevel: System.Log.INFO log: message and: message2.
## 	}
##
## 	fun(#class) logNl
## 	{
## 		^self atLevel: System.Log.INFO log: "\n".
## 	}
##
## 	fun(#class) logNl: message
## 	{
## 		^self atLevel: System.Log.INFO logNl: message.
## 	}
##
## 	fun(#class) logNl: message and: message2
## 	{
## 		^self atLevel: System.Log.INFO logNl: message and: message2.
## 	}
##
## 	fun(#class) backtrace
## 	{
## 		| ctx oldps |
## 		// TOOD: IMPROVE THIS EXPERIMENTAL BACKTRACE... MOVE THIS TO  System>>backtrace and skip the first fun context for backtrace itself.
## // TODO: make this fun atomic? no other process should get scheduled while this function is running?
## //  possible imementation funs:
## //    1. disable task switching? ->
## //    2. use a global lock.
## //    3. make this a primitive function. -> natually no callback.
## //    4. introduce a new fun attribute. e.g. #atomic -> vm disables task switching or uses a lock to achieve atomicity.
## // >>>> i think it should not be atomic as a while. only logging output should be produeced at one go.
##
## 		oldps := System _toggleProcessSwitching: false.
## 		System logNl: "== BACKTRACE ==".
##
## 		//ctx := thisContext.
## 		ctx := thisContext sender. // skip the current context. skip to the caller context.
## 		while (ctx notNil)
## 		{
## 			// if (ctx sender isNil) { break }. // to skip the fake top level call context...
##
## 			if (ctx class == MethodContext)
## 			{
## 				System log: " ";
## 				       log: ctx fun owner name;
## 				       log: ">>";
## 				       log: ctx fun name;
## 				       log: " (";
## 				       log: ctx fun sourceFile;
## 				       log: " ";
## 				       log: (ctx fun ipSourceLine: (ctx pc)) asString;
## 				       logNl: ")".
## 				//System logNl: (" " & ctx fun owner name & ">>" & ctx fun name &
## 				//			   " (" & ctx fun sourceFile & " " & (ctx fun ipSourceLine: (ctx pc)) asString & ")").
## 			}.
## 			// TODO: include blockcontext???
## 			ctx := ctx sender.
## 		}.
## 		System logNl: "== END OF BACKTRACE ==".
## 		System _toggleProcessSwitching: oldps.
## 	}
##
## ##	/* nsdic access */
## ##	fun(#class) at: key
## ##	{
## ##		^self nsdic at: key
## ##	}
## ##
## ##	fun(#class) at: key put: value
## ##	{
## ##		^self nsdic at: key put: value
## ##	}
## ##
## ##	/* raw memory allocation */
## ##	fun(#class,#primitive) malloc (size).
## ##	fun(#class,#primitive) calloc (size).
## ##	fun(#class,#primitive) free (rawptr).
## ##
## ##	fun(#class,#primitive) malloc: size.
## ##	fun(#class,#primitive) calloc: size.
## ##	fun(#class,#primitive) free: rawptr.
## ##
## ##	/* raw memory access */
## ##	fun(#class,#primitive) getInt8   (rawptr, offset). // <primitive: #System__getInt8>
## ##	fun(#class,#primitive) getInt16  (rawptr, offset).
## ##	fun(#class,#primitive) getInt32  (rawptr, offset).
## ##	fun(#class,#primitive) getInt64  (rawptr, offset).
## ##	fun(#class,#primitive) getUint8  (rawptr, offset). // <primitive: #System__getUint8>
## ##	fun(#class,#primitive) getUint16 (rawptr, offset).
## ##	fun(#class,#primitive) getUint32 (rawptr, offset).
## ##	fun(#class,#primitive) getUint64 (rawptr, offset).
## ##
## ##	fun(#class,#primitive) putInt8   (rawptr, offset, value).
## ##	fun(#class,#primitive) putInt16  (rawptr, offset, value).
## ##	fun(#class,#primitive) putInt32  (rawptr, offset, value).
## ##	fun(#class,#primitive) putInt64  (rawptr, offset, value).
## ##	fun(#class,#primitive) putUint8  (rawptr, offset, value).
## ##	fun(#class,#primitive) putUint16 (rawptr, offset, value).
## ##	fun(#class,#primitive) putUint32 (rawptr, offset, value).
## ##	fun(#class,#primitive) putUint64 (rawptr, offset, value).
## ##
## ##	fun(#class,#primitive) getBytes (rawptr, offset, byte_array, offset_in_buffer, len_in_buffer).
## ##	fun(#class,#primitive) putBytes (rawptr, offset, byte_array, offset_in_buffer, len_in_buffer).
}

## ---------------------------------------------------------------------------------

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


