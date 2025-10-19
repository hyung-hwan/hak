unit Hak;

{$mode objfpc}{$H+}
{$macro on}
{$linklib hak}
{$linklib c}
{$linklib dl}
{$linklib gcc}

{$if defined(HAK_LIB_QUADMATH_REQUIRED)}
{$linklib quadmath}
{$endif}

interface

uses SysUtils;

type
	BitMask = longword; (* this must match hak_bitmask_t in hak.h *)

(*const
	TRAIT_LANG_ENABLE_EOL = (BitMask(1) shl 14); *)

type
	Bchar = System.AnsiChar;
	PBchar = System.PAnsiChar;

{$if defined(HAK_WIDE_CHAR_SIZE_IS_4)}
	Uchar = System.UCS4Char;
	PUchar = System.PUCS4Char;
{$else}
	Uchar = System.WideChar;
	PUchar = System.PWideChar;
{$endif}

	TraitBit = ( (* this enum must follow hak_trait_t in hak.h *)
		LANG_ENABLE_EOL = (BitMask(1) shl 14)
	);

	Option = ( (* this enum must follow hak_option_t in hak.h *)
		TRAIT,
		LOG_MASK,
		LOG_MAXCAPA
	);

	IoCmd = ( (* this enum must follow hak_io_cmd_t in hak.h *)
		IO_OPEN,
		IO_CLOSE,
		IO_READ,
		IO_READ_BYTES,
		IO_WRITE,
		IO_WRITE_BYTES,
		IO_FLUSH
	);

	LocationB = record
		line: System.SizeUint;
		colm: System.SizeUint;
		filp: PBchar;
	end;

	LocationU = record
		line: System.SizeUint;
		colm: System.SizeUint;
		filp: PUchar;
	end;

	ErrorException = class(SysUtils.Exception)
	private
		error_code: integer;
		error_file: string;
		error_line: System.SizeUint;
		error_colm: System.SizeUint;

	public
		constructor Create(const msg: string); overload;
		constructor Create(const msg: string; cod: integer; const fil: string; lin: System.SizeUint; col: System.SizeUint); overload;

		property Code: integer read error_code;
		property Line: System.SizeUint read error_line;
		property Column: System.SizeUint read error_colm;
		property FileName: string read error_file;
	end;

{$ifndef HAK_CCI_BUF_LEN}
{$define HAK_CCI_BUF_LEN := 2048}
{$endif}

//{$packrecords c}
	CciArgPtr = ^CciArg;
	CciArg = record (* this record must follow the public part of hak_io_cciarg_t in hak.h *)
		name: PUchar;
		handle: pointer;
		byte_oriented: integer;
		buf: array[0..(HAK_CCI_BUF_LEN - 1)] of Uchar;
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
		procedure CompileFile(filename: System.PAnsiChar);
		procedure CompileText(text: System.PAnsiChar);
		procedure CompileText(text: System.PAnsiChar; len: System.SizeUint);
		procedure CompileText(text: PUchar);
		procedure CompileText(text: PUchar; len: System.SizeUint);
{$if defined(HAK_WIDE_CHAR_SIZE_IS_4)}
		procedure CompileText(text: PWideChar);
		procedure CompileText(text: PWideChar; len: System.SizeUint);
{$endif}
		procedure Execute();

	protected
		function FetchException(const hdr: string): Exception;
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

	SynerrBPtr = ^SynerrB;
	SynerrB = record
		num: integer;
		loc: LocationB;
	end;

	ErrbinfPtr = ^Errbinf;
	Errbinf = record
		num: integer;
		msg: array[0..2047] of Bchar;
		loc: LocationB;
	end;

	ErruinfPtr = ^Errbinf;
	Erruinf = record
		num: integer;
		msg: array[0..2047] of Uchar;
		loc: LocationU;
	end;

	Errinf = Erruinf;
	ErrinfPtr = ErruinfPtr;


(*----- external hak function -----*)
function hak_errnum_to_errbcstr(errnum: integer; errbuf: pointer; errbufsz: System.SizeUint): pointer; cdecl; external;
function hak_errnum_is_synerr(errnum: integer): boolean; cdecl; external;

function hak_openstd(xtnsize: System.SizeUint; errinf: pointer): pointer; cdecl; external;
procedure hak_close(handle: pointer); cdecl; external;
function hak_getxtn(handle: pointer): InterpExtPtr; cdecl; external;

function hak_setoption(handle: pointer; option: Option; value: pointer): integer; cdecl; external;
function hak_getoption(handle: pointer; option: Option; value: pointer): integer; cdecl; external;

procedure hak_seterrnum(handle: pointer; errnum: integer); cdecl; external;
function hak_geterrnum(handle: pointer): integer; cdecl; external;

procedure hak_seterrbmsg(handle: pointer; errnum: integer; errmsg: PBchar); cdecl; external;
function hak_geterrbmsg(handle: pointer): PBchar; cdecl; external;

procedure hak_geterrbinf(handle: pointer; errinf: pointer); cdecl; external;
procedure hak_geterruinf(handle: pointer; errinf: pointer); cdecl; external;

function hak_ignite(handle: pointer; heapsize: System.SizeUint): integer; cdecl; external;
function hak_addbuiltinprims(handle: pointer): integer; cdecl; external;
function hak_beginfeed(handle: pointer; on_cnode: pointer): integer; cdecl; external;
function hak_feedbchars(handle: pointer; data: PBchar; len: System.SizeUint): integer; cdecl; external;
function hak_feeduchars(handle: pointer; data: PUchar; len: System.SizeUint): integer; cdecl; external; (* this is wrong in deed - hak_uchar_t may not been Uchar ..*)
function hak_endfeed(handle: pointer): integer; cdecl; external;

function hak_attachccio(handle: pointer; cci: pointer): integer; cdecl; external;
function hak_attachcciostdwithbcstr(handle: pointer; cci: PBchar): integer; cdecl; external;
procedure hak_detachccio(handle: pointer); cdecl; external;
function hak_attachudiostdwithbcstr(handle: pointer; udi: PBchar; udo: PBchar): integer; cdecl; external;
procedure hak_detachudio(handle: pointer); cdecl; external;
function hak_compile(handle: pointer; cnode: pointer; flags: integer): integer; cdecl; external;
function hak_execute(handle: pointer): pointer; cdecl; external;
procedure hak_abort(handle: pointer) cdecl; external;

procedure hak_getsynerrb(handle: pointer; synerr: SynerrBPtr) cdecl; external;
function hak_syserrstrb(handle: pointer; syserr_type: integer; syserr_code: integer; buf: PBchar; len: System.SizeUint): integer; cdecl; external;
function hak_count_ucstr(ptr: PUchar): System.SizeUint; cdecl; external;
(*----- end external hak function -----*)

implementation

uses Math, Classes;

type
	NamedHandle = record
		handle: THandle;
		name: System.RawByteString;
	end;

	NamedHandlePtr = ^NamedHandle;


function PUCS4CharLength(p: PUCS4Char): System.SizeUint;
var
	len: System.SizeUint;
begin
	len := 0;
	while p[len] <> 0 do Inc(len);
	exit(len);
end;

function PUCS4CharToWideString(p: PUCS4Char): System.WideString;
var
	len: System.SizeUint;
	arr: System.UCS4String;
begin
	len := PUCS4CharLength(p);

	(* len + 1 for SetLength because UCS4StringToWideString() skips the last character in RTL.
	   https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/inc/ustrings.inc
  function UCS4StringToUnicodeString(const s : UCS4String) : UnicodeString;
  var
    i        : SizeInt;
    reslen   : SizeInt;
  begin
    reslen:=0;
    for i:=0 to length(s)-2 do
      Inc(reslen,1+ord((s[i]>$ffff) and (cardinal(s[i])<=$10ffff)));
    SetLength(result,reslen);
    UCS4Decode(s,pointer(result));
  end;
	*)

	SetLength(arr, len + 1);
	Move(p^, arr[0], len * SizeOf(UCS4Char));

	exit(UCS4StringToWideString(arr));
end;

constructor ErrorException.Create (const msg: string);
begin
	inherited Create(msg);
end;

constructor ErrorException.Create (const msg: string; cod: integer; const fil: string; lin: System.SizeUint; col: System.SizeUint);
begin
	inherited Create(msg);
	self.error_code := cod;
	self.error_file := fil;
	self.error_line := lin;
	self.error_colm := col;
end;

constructor Interp.Create (x: integer);
var
	h: pointer;
	ei: Errinf;
	ebi: Errbinf;
	tb: BitMask;
	ext: InterpExtPtr;
begin
	h := hak_openstd(System.SizeOf(Interp), @ei);
	if h = nil then begin
{$if defined(HAK_WIDE_CHAR_SIZE_IS_4)}
		raise Exception.Create(System.UTF8Encode(PUCS4CharToWideString(ei.msg)));
{$else}
		raise Exception.Create(System.UTF8Encode(ei.msg));
{$endif}
	end;

	if hak_getoption(h, Option.TRAIT, @tb) <= -1 then tb := 0;

	tb := tb or BitMask(TraitBit.LANG_ENABLE_EOL);
	if hak_setoption(h, Option.TRAIT, @tb) <= -1 then begin
		hak_geterrbinf(h, @ebi);
		hak_close(h);
		raise Exception.Create(ebi.msg);
	end;

	self.handle := h;

	ext := hak_getxtn(h);
	ext^.self := self;
end;

destructor Interp.Destroy;
begin
	if self.handle <> nil then
	begin
		hak_close(self.handle);
		self.handle := nil;
	end;
	inherited;
end;

function Interp.FetchException(const hdr: string): Exception;
var
	ebi: Errbinf;
	serr: SynerrB;
	filp: PBchar;
	xmsg: string;
begin
	hak_geterrbinf(self.handle, @ebi);
	if hak_errnum_is_synerr(ebi.num) then begin
		hak_getsynerrb(self.handle, @serr);
		filp := PBchar('');
		if serr.loc.filp <> nil then filp := serr.loc.filp;
		exit(ErrorException.Create(string(ebi.msg), ebi.num, string(filp), serr.loc.line, serr.loc.colm));
	end
	else if ebi.loc.line > 0 then begin
		exit(ErrorException.Create(string(ebi.msg), ebi.num, string(ebi.loc.filp), ebi.loc.line, ebi.loc.colm));
	end
	else begin
		if hdr = '' then
			xmsg := string(ebi.msg)
		else
			xmsg := SysUtils.Format('%s - %s', [hdr, string(ebi.msg)]);
		exit(Exception.Create(xmsg));
	end;
end;

procedure Interp.Ignite(heapsize: System.SizeUint);
begin
	if hak_ignite(self.handle, heapsize) <= -1 then
	begin
		raise self.FetchException('failed to ignite');
	end;
end;

procedure Interp.AddBuiltinPrims();
begin
	if hak_addbuiltinprims(self.handle) <= -1 then
	begin
		raise self.FetchException('failed to add builtin primitives');
	end;
end;

function handle_to_self(handle: pointer): Interp;
var
	ext: InterpExtPtr;
begin
	ext := hak_getxtn(handle);
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
				name := System.UTF8Encode(WideString(arg^.name));
				if SysUtils.CompareStr(basedir, '') <> 0 then
					name := SysUtils.ConcatPaths([basedir, name]);
			end;

			System.New(nf);
			if nf = nil then begin
				err := SysUtils.GetLastOSError();
				hak_seterrbmsg(handle, hak_syserrstrb(handle, 0, err, nil, 0), PBchar(SysUtils.SysErrorMessage(err)));
				exit(-1);
			end;

			if arg^.includer <> nil then begin
				(* included file *)
				nf^.handle := SysUtils.FileOpen(name, SysUtils.fmOpenRead);
				if nf^.handle = System.THandle(-1) then begin
					err := SysUtils.GetLastOSError();
					hak_seterrbmsg(handle, hak_syserrstrb(handle, 0, err, nil, 0), PBchar(SysUtils.SysErrorMessage(err)));
					System.Dispose(nf);
					exit(-1);
				end;
			end
			else begin
				nf^.handle := THandle(-1);
			end;

			nf^.name := name;
			arg^.handle := pointer(nf);
			arg^.byte_oriented := 1;
		end;

		IO_CLOSE: begin
			nf := NamedHandlePtr(arg^.handle);
			if nf^.handle <> System.THandle(-1) then SysUtils.FileClose(nf^.handle);
			System.Dispose(nf);
		end;

		IO_READ_BYTES: begin
			nf := NamedHandlePtr(arg^.handle);
			len := SysUtils.FileRead(nf^.handle, arg^.buf, System.SizeOf(arg^.buf)); (* use SizeOf a Uchar buffer as it needs to fill it with bytes *)
			if len <= -1 then begin
				hak_seterrbmsg(handle, hak_syserrstrb(handle, 0, err, nil, 0), PBchar(SysUtils.SysErrorMessage(err)));
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
			hak_seterrnum(handle, 999); (* TODO: change error code *)
			exit(-1);
		end;
	end;

	exit(0);
end;

procedure Interp.CompileFile(filename: System.PAnsiChar);
var
	f: System.THandle = -1;
	attached: boolean = false;
	feed_ongoing: boolean = false;
	buf: array[0..1023] of System.AnsiChar;
	len: System.LongInt;
	excpt: SysUtils.Exception;
label
	oops;
begin
	f := SysUtils.FileOpen(filename, SysUtils.fmOpenRead);
	if f = System.THandle(-1) then begin
		excpt := Exception.Create('failed to open ' + filename + ' - ' +  SysUtils.SysErrorMessage(SysUtils.GetLastOSError()));
		goto oops;
	end;

	self.basefile := filename;
	if hak_attachccio(self.handle, @cci_handler) <= -1 then begin
		excpt := self.FetchException('failed to attach ccio handler');
		goto oops;
	end;
	attached := true;

	if hak_beginfeed(self.handle, nil) <= -1 then begin
		excpt := self.FetchException('failed to begin feeding');
		goto oops;
	end;
	feed_ongoing := true;

	while true do begin
		len := SysUtils.FileRead(f, buf, System.SizeOf(buf));
		if len <= -1 then begin
			excpt := Exception.Create('failed to read ' + filename + ' - ' +  SysUtils.SysErrorMessage(SysUtils.GetLastOSError()));
			goto oops;
		end;
		if len = 0 then break;

		if hak_feedbchars(self.handle, buf, len) <= -1 then begin
			excpt := self.FetchException('failed to feed text');
			goto oops;
		end;
	end;

	if hak_endfeed(self.handle) <= -1 then begin
		excpt := self.FetchException('failed to end feeding');
		goto oops;
	end;
	feed_ongoing := false;

	hak_detachccio(self.handle);
	self.basefile := '';
	SysUtils.FileClose(f);
	exit();

oops:
	if feed_ongoing then hak_endfeed(self.handle);
	if attached then hak_detachccio(self.handle);
	self.basefile := '';
	if f <> System.THandle(-1) then SysUtils.FileClose(f);
	raise excpt;
end;

procedure Interp.CompileText(text: System.PAnsiChar);
begin
	self.CompileText(text, SysUtils.Strlen(text));
end;

procedure Interp.CompileText(text: System.PAnsiChar; len: System.SizeUint);
var
	excpt: Exception;
begin
	if hak_attachcciostdwithbcstr(self.handle, nil) <= -1 then begin
		excpt := self.FetchException('failed to attach ccio handler');
		raise excpt;
	end;

	if hak_beginfeed(self.handle, nil) <= -1 then begin
		excpt := self.FetchException('failed to begin feeding');
		hak_detachccio(self.handle);
		raise excpt;
	end;

	if hak_feedbchars(self.handle, text, len) <= -1 then begin
		excpt := self.FetchException('failed to feed text');
		hak_endfeed(self.handle);
		hak_detachccio(self.handle);
		raise excpt;
	end;

	if hak_endfeed(self.handle) <= -1 then begin
		excpt := self.FetchException('failed to end feeding');
		hak_detachccio(self.handle);
		raise excpt;
	end;

	hak_detachccio(self.handle);
end;

{$if defined(HAK_WIDE_CHAR_SIZE_IS_4)}
procedure Interp.CompileText(text: PWideChar);
var
	x: UCS4String;
begin
	x := WideStringToUcs4String(text);
	self.CompileText(PUchar(x));
end;

procedure Interp.CompileText(text: PWideChar; len: System.SizeUint);
var

	x_text: PBchar;
	x_capa: System.SizeUint;
	x_len: System.SizeUint;
begin
	x_capa := len * 4 + 1; (* allocation sizing for the worst case *)
	System.GetMem(x_text, x_capa);
	try
		x_len := System.UnicodeToUtf8(x_text, x_capa, text, len);
		self.CompileText(x_text, x_len);
	finally
		FreeMem(x_text);
	end;
end;
{$endif}

procedure Interp.CompileText(text: PUchar);
begin
	(*self.CompileText(text, SysUtils.Strlen(text));*)
	self.CompileText(text, hak_count_ucstr(text));
end;

procedure Interp.CompileText(text: PUchar; len: System.SizeUint);
var
	excpt: Exception;
begin
	if hak_attachcciostdwithbcstr(self.handle, nil) <= -1 then begin
		excpt := self.FetchException('failed to attach ccio handler');
		raise excpt;
	end;

	if hak_beginfeed(self.handle, nil) <= -1 then begin
		excpt := self.FetchException('failed to begin feeding');
		hak_detachccio(self.handle);
		raise excpt;
	end;

	if hak_feeduchars(self.handle, text, len) <= -1 then begin
		excpt := self.FetchException('failed to feed text');
		hak_endfeed(self.handle);
		hak_detachccio(self.handle);
		raise excpt;
	end;

	if hak_endfeed(self.handle) <= -1 then begin
		excpt := self.FetchException('failed to end feeding');
		hak_detachccio(self.handle);
		raise excpt;
	end;

	hak_detachccio(self.handle);
end;


procedure Interp.Execute();
var
	excpt: Exception;
begin
	if hak_attachudiostdwithbcstr(self.handle, nil, nil) <= -1 then begin
		excpt := self.FetchException('failed to attach udio handlers');
		raise excpt;
	end;
	if hak_execute(self.handle) = nil then begin
		excpt := self.FetchException(''); (* no header message used to make the error format the same as bin/hak *)
		hak_detachudio(self.handle);
		raise excpt;
	end;

	hak_detachudio(self.handle);
end;

end. (* unit *)
