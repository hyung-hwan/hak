unit HCL;

{$mode objfpc}{$H+}
{$linklib hcl}
{$linklib c}
{$linklib gcc}
{$linklib quadmath}

interface

type 
	Interp = class
	public
		handle: pointer;
	private
		handle1: integer;
		handle2: integer;
		handle3: integer;
		handle4: integer;
		handle5: integer;
		handle6: integer;
	public
		constructor Create (x: integer);
		destructor Destroy; override;
		procedure Ignite(heapsize: sizeint);
		procedure AddBuiltinPrims();
		procedure Compile(text: pansichar);
		procedure Compile(text: pansichar; len: sizeint);
		procedure Compile(text: pwidechar);
		procedure Compile(text: pwidechar; len: sizeint);
		procedure Execute();

	protected
		function FetchErrorMsg(): string;
	end;

	InterpPtr = ^Interp;

(*----- external hcl function -----*)
function hcl_errnum_to_errbcstr(errnum: integer; errbuf: pointer; errbufsz: sizeint): pointer; cdecl; external;
function hcl_openstd(xtnsize: sizeint; errnum: pointer): pointer; cdecl; external;
procedure hcl_close(handle: pointer); cdecl; external;
function hcl_geterrbmsg(handle: pointer): pansichar; cdecl; external;
function hcl_ignite(handle: pointer; heapsize: sizeint): integer; cdecl; external;
function hcl_addbuiltinprims(handle: pointer): integer; cdecl; external;
function hcl_beginfeed(handle: pointer; on_cnode: pointer): integer; cdecl; external;
function hcl_feedbchars(handle: pointer; data: pansichar; len: sizeint): integer; cdecl; external;
function hcl_feeduchars(handle: pointer; data: pwidechar; len: sizeint): integer; cdecl; external; (* this is wrong in deed *)
function hcl_endfeed(handle: pointer): integer; cdecl; external;

function hcl_attachcciostdwithbcstr(handle: pointer; cci: pansichar): integer; cdecl; external;
procedure hcl_detachccio(handle: pointer); cdecl; external;
function hcl_attachudiostdwithbcstr(handle: pointer; udi: pansichar; udo: pansichar): integer; cdecl; external;
procedure hcl_detachudio(handle: pointer); cdecl; external;
function hcl_compile(handle: pointer; cnode: pointer; flags: integer): integer; cdecl; external;
function hcl_execute(handle: pointer): integer; cdecl; external;
procedure hcl_abort(handle: pointer) cdecl; external;

function hcl_count_ucstr(ptr: pwidechar): sizeint; cdecl; external;
(*----- end external hcl function -----*)

implementation

uses sysutils;

constructor Interp.Create (x: integer);
var 
	h: pointer;
	errnum: integer;
	errmsg: array[0..255] of AnsiChar;
begin
	h := hcl_openstd(0, @errnum);
	if h = nil then begin
		hcl_errnum_to_errbcstr (errnum, @errmsg, length(errmsg));
		raise Exception.Create(errmsg);
	end;
	self.handle := h;
end;

destructor Interp.Destroy;
begin
	if self.handle <> nil then
	begin
		hcl_close(self.handle);
		self.handle := nil;
	end;
	inherited;
end;

function Interp.FetchErrorMsg(): string;
var
	bmsg: PAnsiChar;
begin
	(* TODO: if the errocode is syntax error. use the systax error message and locations info *)
	bmsg := hcl_geterrbmsg(self.handle);
	exit(string(bmsg))
end;

procedure Interp.Ignite(heapsize: sizeint);
begin
	if hcl_ignite(self.handle, heapsize) <= -1 then
	begin
		(* TODO: proper error message *)
		raise Exception.Create('failed to ignite - ' + self.FetchErrorMsg())
	end;
end;

procedure Interp.AddBuiltinPrims();
begin
	(* TODO: proper error message *)
	if hcl_addbuiltinprims(self.handle) <= -1 then
	begin
		raise Exception.Create('failed to add builtin primitives - ' + self.FetchErrorMsg())
	end;
end;

procedure Interp.Compile(text: pansichar);
begin
	self.Compile(text, sysutils.strlen(text));
end;

procedure Interp.Compile(text: pansichar; len: sizeint);
var
	errmsg: string;
begin
	if hcl_attachcciostdwithbcstr(self.handle, nil) <= -1 then begin
		raise Exception.Create('failed to attach ccio handlers - ' + self.FetchErrorMsg())
	end;

	if hcl_beginfeed(self.handle, nil) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to begin feeding - ' + errmsg)
	end;
	
	if hcl_feedbchars(self.handle, text, len) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_endfeed(self.handle);
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to feed text - ' + errmsg)
	end;

	if hcl_endfeed(self.handle) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to end feeding - ' + errmsg)
	end;

	hcl_detachccio(self.handle);
end;

procedure Interp.Compile(text: pwidechar);
begin
	self.Compile(text, sysutils.strlen(text));
end;

procedure Interp.Compile(text: pwidechar; len: sizeint);
var
	errmsg: string;
begin
	if hcl_attachcciostdwithbcstr(self.handle, nil) <= -1 then begin
		raise Exception.Create('failed to attach ccio handlers - ' + self.FetchErrorMsg())
	end;

	if hcl_beginfeed(self.handle, nil) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to begin feeding - ' + errmsg)
	end;

	if hcl_feeduchars(self.handle, text, len) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_endfeed(self.handle);
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to feed text - ' + errmsg)
	end;

	if hcl_endfeed(self.handle) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to end feeding - ' + errmsg)
	end;

	hcl_detachccio(self.handle);
end;


procedure Interp.Execute();
var
	n: integer;
begin
	if hcl_attachudiostdwithbcstr(self.handle, nil, nil) <= -1 then begin
		raise Exception.Create('failed to attach udio handlers - ' + self.FetchErrorMsg())
	end;
	n := hcl_execute(self.handle);
	hcl_detachudio(self.handle);
	if n <= -1 then
	begin	
		raise Exception.Create('failed to execute - ' + self.FetchErrorMsg())
	end;
end;

end. (* unit *)

