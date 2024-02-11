unit HCL;

{$mode objfpc}{$H+}
{$linklib hcl}
{$linklib c}
{$linklib gcc}
{$linklib quadmath}

interface

type
	BitMask = longword; (* this must match hcl_bitmask_t in hcl.h *)

(*const
	TRAIT_LANG_ENABLE_EOF = (BitMask(1) shl 14);
	TRAIT_LANG_ENABLE_BLOCK = (BitMask(1) shl 15);*)

type
	TraitBit = (
		LANG_ENABLE_EOF = (BitMask(1) shl 14),
		LANG_ENABLE_BLOCK = (BitMask(1) shl 15)
	);

	Option = (
		TRAIT,
		LOG_MASK,
		LOG_MAXCAPA
	);

	Interp = class
	protected
		handle: pointer;

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

	Location = record
		line: sizeint;
		colm: sizeint;
		filp: pwidechar;
	end;
	Synerr = record
		num: integer;
		loc: Location;
		tgt: record
			val: array[0..255] of widechar;
			len: sizeint;
		end;
	end;

	SynerrPtr = ^Synerr;

(*----- external hcl function -----*)
function hcl_errnum_to_errbcstr(errnum: integer; errbuf: pointer; errbufsz: sizeint): pointer; cdecl; external;
function hcl_errnum_is_synerr(errnum: integer): boolean; cdecl; external;

function hcl_openstd(xtnsize: sizeint; errnum: pointer): pointer; cdecl; external;
procedure hcl_close(handle: pointer); cdecl; external;

function hcl_setoption(handle: pointer; option: Option; value: pointer): integer; cdecl; external;
function hcl_getoption(handle: pointer; option: Option; value: pointer): integer; cdecl; external;

function hcl_geterrnum(handle: pointer): integer; cdecl; external;
function hcl_geterrbmsg(handle: pointer): pansichar; cdecl; external;
function hcl_ignite(handle: pointer; heapsize: sizeint): integer; cdecl; external;
function hcl_addbuiltinprims(handle: pointer): integer; cdecl; external;
function hcl_beginfeed(handle: pointer; on_cnode: pointer): integer; cdecl; external;
function hcl_feedbchars(handle: pointer; data: pansichar; len: sizeint): integer; cdecl; external;
function hcl_feeduchars(handle: pointer; data: pwidechar; len: sizeint): integer; cdecl; external; (* this is wrong in deed - hcl_uchar_t may not been widechar ..*)
function hcl_endfeed(handle: pointer): integer; cdecl; external;

function hcl_attachcciostdwithbcstr(handle: pointer; cci: pansichar): integer; cdecl; external;
procedure hcl_detachccio(handle: pointer); cdecl; external;
function hcl_attachudiostdwithbcstr(handle: pointer; udi: pansichar; udo: pansichar): integer; cdecl; external;
procedure hcl_detachudio(handle: pointer); cdecl; external;
function hcl_compile(handle: pointer; cnode: pointer; flags: integer): integer; cdecl; external;
function hcl_execute(handle: pointer): pointer; cdecl; external;
procedure hcl_abort(handle: pointer) cdecl; external;

procedure hcl_getsynerr(handle: pointer; synerr: SynerrPtr) cdecl; external;
function hcl_count_ucstr(ptr: pwidechar): sizeint; cdecl; external;
(*----- end external hcl function -----*)

implementation

uses sysutils, math;

constructor Interp.Create (x: integer);
var
	h: pointer;
	errnum: integer;
	errmsg: array[0..255] of AnsiChar;
	tb: BitMask;
begin

	h := hcl_openstd(0, @errnum);
	if h = nil then begin
		hcl_errnum_to_errbcstr(errnum, @errmsg, length(errmsg));
		raise Exception.Create(errmsg);
	end;

	tb := BitMask(TraitBit.LANG_ENABLE_EOF) or BitMask(TraitBit.LANG_ENABLE_BLOCK);
	if hcl_setoption(h, Option.TRAIT, @tb) <= -1 then begin
		hcl_errnum_to_errbcstr(errnum, @errmsg, length(errmsg));
		hcl_close(h);
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
	num: integer;
	bmsg: pansichar;
	serr: Synerr;
	filp: pwidechar;
	tgt: array[0..255] of widechar;
begin
	num := hcl_geterrnum(self.handle);
	if hcl_errnum_is_synerr(num) then begin
		hcl_getsynerr(self.handle, @serr);
		bmsg := hcl_geterrbmsg(self.handle);
		filp := pwidechar(widestring(''));
		if serr.loc.filp <> nil then filp := serr.loc.filp;
		if serr.tgt.len > 0 then begin
			sysutils.strlcopy(@tgt, serr.tgt.val, math.min(serr.tgt.len, length(tgt) - 1));
			exit(format('%s at %s[%u:%u] - %s', [string(bmsg), string(filp), serr.loc.line, serr.loc.colm, string(tgt)]));
		end
		else begin
			exit(format('%s at %s[%u:%u]', [string(bmsg), string(filp), serr.loc.line, serr.loc.colm]));
		end;
	end
	else begin
		bmsg := hcl_geterrbmsg(self.handle);
		exit(string(bmsg))
	end;
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
	errnum: integer;
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
		errnum := hcl_geterrnum(self.handle);
		errmsg := self.FetchErrorMsg();
		hcl_endfeed(self.handle);
		hcl_detachccio(self.handle);
		if hcl_errnum_is_synerr(errnum) then
			raise Exception.Create(errmsg)
		else
			raise Exception.Create('failed to feed text - ' + errmsg);
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
	errnum: integer;
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
		errnum := hcl_geterrnum(self.handle);
		errmsg := self.FetchErrorMsg();
		hcl_endfeed(self.handle);
		hcl_detachccio(self.handle);
		if hcl_errnum_is_synerr(errnum) then
			raise Exception.Create(errmsg)
		else
			raise Exception.Create('failed to feed text - ' + errmsg);
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
	errmsg: string;
begin
	if hcl_attachudiostdwithbcstr(self.handle, nil, nil) <= -1 then begin
		raise Exception.Create('failed to attach udio handlers - ' + self.FetchErrorMsg())
	end;
	if hcl_execute(self.handle) = nil then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachudio(self.handle);
		raise Exception.Create('failed to execute - ' + errmsg)
	end;

	hcl_detachudio(self.handle);
end;

end. (* unit *)

