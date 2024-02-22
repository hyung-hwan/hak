unit HCL;

{$mode objfpc}{$H+}
{$linklib hcl}
{$linklib c}
{$linklib dl}
{$linklib gcc}

{$if defined(HCL_LIB_QUADMATH_REQUIRED)}
{$linklib quadmath}
{$endif}

interface

type
	BitMask = longword; (* this must match hcl_bitmask_t in hcl.h *)

(*const
	TRAIT_LANG_ENABLE_EOF = (BitMask(1) shl 14);
	TRAIT_LANG_ENABLE_BLOCK = (BitMask(1) shl 15);*)

type
	TraitBit = ( (* this enum must follow hcl_trait_t in hcl.h *)
		LANG_ENABLE_EOF = (BitMask(1) shl 14),
		LANG_ENABLE_BLOCK = (BitMask(1) shl 15)
	);

	Option = ( (* this enum must follow hcl_option_t in hcl.h *)
		TRAIT,
		LOG_MASK,
		LOG_MAXCAPA
	);

	IoCmd = ( (* this enum must follow hcl_io_cmd_t in hcl.h *)
		IO_OPEN,
		IO_CLOSE,
		IO_READ,
		IO_READ_BYTES,
		IO_WRITE,
		IO_WRITE_BYTES,
		IO_FLUSH
	);

{$ifndef HCL_CCI_BUF_LEN}
{$define HCL_CCI_BUF_LEN := 2048}
{$endif}

//{$packrecords c}
	CciArgPtr = ^CciArg;
	CciArg = record (* this record must follow the public part of hcl_io_cciarg_t in hcl.h *)
		name: pwidechar;
		handle: pointer;
		is_bytes: integer;
		buf: array[0..(HCL_CCI_BUF_LEN - 1)] of widechar;
		xlen: System.SizeUint;
		includer: CciArgPtr;
	end;
//{$packrecords normal}

	Interp = class
	protected
		handle: pointer;
		basefile: string;

	public
		constructor Create(x: integer);
		destructor Destroy(); override;
		procedure Ignite(heapsize: System.SizeUint);
		procedure AddBuiltinPrims();
		procedure CompileFile(filename: pansichar);
		procedure CompileText(text: pansichar);
		procedure CompileText(text: pansichar; len: System.SizeUint);
		procedure CompileText(text: pwidechar);
		procedure CompileText(text: pwidechar; len: System.SizeUint);
		procedure Execute();

	protected
		function FetchErrorMsg(): string;
	end;

	InterpExt = record
		self: Interp;
	end;

	InterpExtPtr = ^InterpExt;

	IO = class
	public
		procedure Open(); virtual; abstract;
		procedure Close(); virtual; abstract;
		function Read(): System.SizeUint; virtual; abstract;
	end;

	Location = record
		line: System.SizeUint;
		colm: System.SizeUint;
		filp: pwidechar;
	end;
	Synerr = record
		num: integer;
		loc: Location;
		tgt: record
			val: array[0..255] of widechar;
			len: System.SizeUint;
		end;
	end;

	SynerrPtr = ^Synerr;

(*----- external hcl function -----*)
function hcl_errnum_to_errbcstr(errnum: integer; errbuf: pointer; errbufsz: System.SizeUint): pointer; cdecl; external;
function hcl_errnum_is_synerr(errnum: integer): boolean; cdecl; external;

function hcl_openstd(xtnsize: System.SizeUint; errnum: pointer): pointer; cdecl; external;
procedure hcl_close(handle: pointer); cdecl; external;
function hcl_getxtn(handle: pointer): InterpExtPtr; cdecl; external;

function hcl_setoption(handle: pointer; option: Option; value: pointer): integer; cdecl; external;
function hcl_getoption(handle: pointer; option: Option; value: pointer): integer; cdecl; external;

procedure hcl_seterrnum (handle: pointer; errnum: integer); cdecl; external;
function hcl_geterrnum(handle: pointer): integer; cdecl; external;

procedure hcl_seterrbmsg (handle: pointer; errnum: integer; errmsg: pansichar); cdecl; external;
function hcl_geterrbmsg(handle: pointer): pansichar; cdecl; external;

function hcl_ignite(handle: pointer; heapsize: System.SizeUint): integer; cdecl; external;
function hcl_addbuiltinprims(handle: pointer): integer; cdecl; external;
function hcl_beginfeed(handle: pointer; on_cnode: pointer): integer; cdecl; external;
function hcl_feedbchars(handle: pointer; data: pansichar; len: System.SizeUint): integer; cdecl; external;
function hcl_feeduchars(handle: pointer; data: pwidechar; len: System.SizeUint): integer; cdecl; external; (* this is wrong in deed - hcl_uchar_t may not been widechar ..*)
function hcl_endfeed(handle: pointer): integer; cdecl; external;

function hcl_attachccio(handle: pointer; cci: pointer): integer; cdecl; external;
function hcl_attachcciostdwithbcstr(handle: pointer; cci: pansichar): integer; cdecl; external;
procedure hcl_detachccio(handle: pointer); cdecl; external;
function hcl_attachudiostdwithbcstr(handle: pointer; udi: pansichar; udo: pansichar): integer; cdecl; external;
procedure hcl_detachudio(handle: pointer); cdecl; external;
function hcl_compile(handle: pointer; cnode: pointer; flags: integer): integer; cdecl; external;
function hcl_execute(handle: pointer): pointer; cdecl; external;
procedure hcl_abort(handle: pointer) cdecl; external;

procedure hcl_getsynerr(handle: pointer; synerr: SynerrPtr) cdecl; external;
function hcl_syserrstrb(handle: pointer; syserr_type: integer; syserr_code: integer; buf: pansichar; len: System.SizeUint): integer; cdecl; external;
function hcl_count_ucstr(ptr: pwidechar): System.SizeUint; cdecl; external;
(*----- end external hcl function -----*)

implementation

uses SysUtils, Math, Classes;

type
	NamedHandle = record
		handle: THandle;
		name: System.RawByteString;
	end;

	NamedHandlePtr = ^NamedHandle;

constructor Interp.Create (x: integer);
var
	h: pointer;
	errnum: integer;
	errmsg: array[0..255] of AnsiChar;
	tb: BitMask;
	ext: InterpExtPtr;
begin
	h := hcl_openstd(System.SizeOf(Interp), @errnum);
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

	ext := hcl_getxtn(h);
	ext^.self := self;
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
			SysUtils.Strlcopy(@tgt, serr.tgt.val, Math.Min(serr.tgt.len, length(tgt) - 1));
			exit(SysUtils.Format('%s at %s[%u:%u] - %s', [string(bmsg), string(filp), serr.loc.line, serr.loc.colm, string(tgt)]));
		end
		else begin
			exit(SysUtils.Format('%s at %s[%u:%u]', [string(bmsg), string(filp), serr.loc.line, serr.loc.colm]));
		end;
	end
	else begin
		bmsg := hcl_geterrbmsg(self.handle);
		exit(string(bmsg))
	end;
end;

procedure Interp.Ignite(heapsize: System.SizeUint);
begin
	if hcl_ignite(self.handle, heapsize) <= -1 then
	begin
		raise Exception.Create('failed to ignite - ' + self.FetchErrorMsg())
	end;
end;

procedure Interp.AddBuiltinPrims();
begin
	if hcl_addbuiltinprims(self.handle) <= -1 then
	begin
		raise Exception.Create('failed to add builtin primitives - ' + self.FetchErrorMsg())
	end;
end;

function handle_to_self(handle: pointer): Interp;
var
	ext: InterpExtPtr;
begin
	ext := hcl_getxtn(handle);
	exit(ext^.self);
end;

function cci_handler(handle: pointer; cmd: IoCmd; arg: CciArgPtr): integer; cdecl;
var
	nf: NamedHandlePtr;
	len: System.LongInt;
	err: System.Integer;
	name: System.RawByteString;
	basedir: System.RawByteString;
	self: Interp;
begin
	case cmd of
		IO_OPEN: begin
			self := handle_to_self(handle);

			if arg^.includer = nil then begin
				(* main stream *)
				name := self.basefile;
			end
			else begin
				(* included file *)
				nf := NamedHandlePtr(arg^.includer^.handle);
				basedir := SysUtils.ExtractFilePath(nf^.name);
				name := UTF8Encode(arg^.name);
				if SysUtils.CompareStr(basedir, '') <> 0 then
					name := SysUtils.ConcatPaths([basedir, name]);
			end;

			System.New(nf);
			if nf = nil then begin
				err := SysUtils.GetLastOSError();
				hcl_seterrbmsg(handle, hcl_syserrstrb(handle, 0, err, nil, 0), pansichar(SysUtils.SysErrorMessage(err)));
				exit(-1);
			end;

			if arg^.includer <> nil then begin
				(* included file *)
				nf^.handle := SysUtils.FileOpen(name, SysUtils.fmOpenRead);
				if nf^.handle = System.THandle(-1) then begin
					err := SysUtils.GetLastOSError();
					hcl_seterrbmsg(handle, hcl_syserrstrb(handle, 0, err, nil, 0), pansichar(SysUtils.SysErrorMessage(err)));
					System.Dispose(nf);
					exit(-1);
				end;
			end
			else begin
				nf^.handle := THandle(-1);
			end;

			nf^.name := name;
			arg^.handle := pointer(nf);
			arg^.is_bytes := 1;
		end;

		IO_CLOSE: begin
			nf := NamedHandlePtr(arg^.handle);
			if nf^.handle <> System.THandle(-1) then SysUtils.FileClose(nf^.handle);
			System.Dispose(nf);
		end;

		IO_READ_BYTES: begin
			nf := NamedHandlePtr(arg^.handle);
			len := SysUtils.FileRead(nf^.handle, arg^.buf, System.SizeOf(arg^.buf)); (* use SizeOf a widechar buffer as it needs to fill it with bytes *)
			if len <= -1 then begin
				hcl_seterrbmsg(handle, hcl_syserrstrb(handle, 0, err, nil, 0), pansichar(SysUtils.SysErrorMessage(err)));
				exit(-1);
			end;
			arg^.xlen := len;
		end;

		IO_FLUSH:
			(* no effect on an input stream *)
			;

		(* the following operations are prohibited on the code input stream:
		IO_READ:
		IO_WRITE:
		IO_WRITE_BYTES:
		*)
		else begin
			hcl_seterrnum(handle, 999); (* TODO: change error code *)
			exit(-1);
		end;
	end;

	exit(0);
end;

procedure Interp.CompileFile(filename: pansichar);
var
	f: System.THandle = -1;
	attached: boolean = false;
	feed_ongoing: boolean = false;
	errnum: System.Integer;
	errmsg: string;
	buf: array[0..1023] of ansichar;
	len: System.LongInt;
label
	oops;
begin
	f := SysUtils.FileOpen(filename, SysUtils.fmOpenRead);
	if f = System.THandle(-1) then begin
		errmsg := 'failed to open ' + filename + ' - ' +  SysUtils.SysErrorMessage(SysUtils.GetLastOSError());
		goto oops;
	end;

	self.basefile := filename;
	if hcl_attachccio(self.handle, @cci_handler) <= -1 then begin
		errmsg := 'failed to attach ccio handler - ' + self.FetchErrorMsg();
		goto oops;
	end;
	attached := true;

	if hcl_beginfeed(self.handle, nil) <= -1 then begin
		errmsg := 'failed to begin feeding - ' + self.FetchErrorMsg();
		goto oops;
	end;
	feed_ongoing := true;

	while true do begin
		len := SysUtils.FileRead(f, buf, System.SizeOf(buf));
		if len <= -1 then begin
			errmsg := 'failed to read ' + filename + ' - ' +  SysUtils.SysErrorMessage(SysUtils.GetLastOSError());
			goto oops;
		end;
		if len = 0 then break;

		if hcl_feedbchars(self.handle, buf, len) <= -1 then begin
			errnum := hcl_geterrnum(self.handle);
			errmsg := self.FetchErrorMsg();
			if not hcl_errnum_is_synerr(errnum) then errmsg := 'failed to feed text - ' + errmsg;
			goto oops;
		end;
	end;

	if hcl_endfeed(self.handle) <= -1 then begin
		errmsg := 'failed to end feeding - ' + self.FetchErrorMsg();
		goto oops;
	end;
	feed_ongoing := false;

	hcl_detachccio(self.handle);
	self.basefile := '';
	SysUtils.FileClose(f);
	exit();

oops:
	if feed_ongoing then hcl_endfeed(self.handle);
	if attached then hcl_detachccio(self.handle);
	self.basefile := '';
	if f <> System.THandle(-1) then SysUtils.FileClose(f);
	raise Exception.Create(errmsg);
end;

procedure Interp.CompileText(text: pansichar);
begin
	self.CompileText(text, SysUtils.Strlen(text));
end;

procedure Interp.CompileText(text: pansichar; len: System.SizeUint);
var
	errnum: integer;
	errmsg: string;
begin
	if hcl_attachcciostdwithbcstr(self.handle, nil) <= -1 then
		raise Exception.Create('failed to attach ccio handler - ' + self.FetchErrorMsg());

	if hcl_beginfeed(self.handle, nil) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to begin feeding - ' + errmsg);
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

procedure Interp.CompileText(text: pwidechar);
begin
	self.CompileText(text, SysUtils.Strlen(text));
end;

procedure Interp.CompileText(text: pwidechar; len: System.SizeUint);
var
	errnum: integer;
	errmsg: string;
begin
	if hcl_attachcciostdwithbcstr(self.handle, nil) <= -1 then
		raise Exception.Create('failed to attach ccio handler - ' + self.FetchErrorMsg());

	if hcl_beginfeed(self.handle, nil) <= -1 then begin
		errmsg := self.FetchErrorMsg();
		hcl_detachccio(self.handle);
		raise Exception.Create('failed to begin feeding - ' + errmsg);
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

