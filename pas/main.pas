program main;

{$mode objfpc}{$H+}

uses Hak, SysUtils, GetOpts
{$ifdef unix}
	, BaseUnix
{$endif}
	;

var

	(*c: System.AnsiChar;*)
	c: Char;
	optind: System.LongInt;
	opts: array[0..3] of GetOpts.TOption;

	source_file: string;
	x: Hak.Interp = nil;

	incdirs: string;
	modlibdirs: string;

	procedure print_usage_and_halt();
	begin
		writeln(System.Stderr, SysUtils.Format('Usage: %s <filename>', [SysUtils.ExtractFileName(System.ParamStr(0))]));
		System.Halt(-1);
	end;

begin
{$ifdef unix}
	(* Writing to a pipe whose reader has gone - `hakpas foo.hak | head -1', or a
	 * child spawned by sys.popen exiting early - raises SIGPIPE, which kills the
	 * process by default before write() ever returns EPIPE. Ignore it so the
	 * failing write reports an error the script can act on instead. See
	 * t/sig-01.hak, and the matching note in bin/hak.c.
	 *
	 * This belongs to the application rather than to the Hak unit: the
	 * disposition is process-wide, and a program that would rather die quietly
	 * on a broken pipe is making a legitimate choice. *)
	BaseUnix.fpSignal(BaseUnix.SIGPIPE, BaseUnix.SignalHandler(BaseUnix.SIG_IGN));
{$endif}

	(* System.ParamCount() returns only the number of argumetns except System.ParamStr(0). It is the upper bound to System.ParamStr(). *)

	opts[0].name := 'heapsize';
	opts[0].has_arg := 1;
	opts[0].value := #0;
	opts[0].flag := nil;

	opts[1].name := 'incdirs';
	opts[1].has_arg := 1;
	opts[1].value := #1;
	opts[1].flag := nil;

	opts[2].name := 'modlibdirs';
	opts[2].has_arg := 1;
	opts[2].value := #2;
	opts[2].flag := nil;

	opts[3].name := ''; (* marker for the last item *)
	opts[3].has_arg := 0;
	opts[3].value := #3;
	opts[3].flag := nil;

(* TODO: proper command-line options handling *)
	c := #0;
	GetOpts.OptErr := false;
	repeat
		c := GetOpts.GetLongOpts(':', @opts[0], optind);
		case c of
		#0: (* --heapsize *)
			begin
				(* TODO *)
			end;

		#1: (* --incdirs *)
			begin
				incdirs := GetOpts.OptArg;
			end;

		#2: (* --modlibdirs *)
			begin
				modlibdirs := GetOpts.OptArg;
			end;

		#3:
			(* this must not be reached *)
			begin
			end;

		'?', ':':
			print_usage_and_halt;
		end;
	until c = GetOpts.EndOfOptions;

	if GetOpts.OptInd <> System.ParamCount() then begin
		print_usage_and_halt;
	end;

	source_file := System.ParamStr(GetOpts.OptInd);
	try
		x := Hak.Interp.Create(100);

		if incdirs <> '' then x.SetIncDirs(pansichar(incdirs));
		if modlibdirs <> '' then x.SetModLibDirs(pansichar(modlibdirs));

		x.Ignite(0);

		x.AddBuiltinPrims();
		//x.AttachCCIO();
		//x.AttachUDIO();

		(*
		x.CompileText(pwidechar('(printf "hello 동키콩\n")'));
		x.CompileText('(printf "hello 동키콩월드\n") ');
		x.CompileText('(동가리오 := 20)');
		x.CompileText('(printf "%d %d\n" 동가리오 (+ 동가리오 동가리오))');

		x.Compile(pwidechar('(printf "%d %d\n" 동가리오 (동가리오 * 동가리오))'#10'printf "hello, world\n";;;'#10));
		*)

		x.CompileFile(pansichar(ansistring(source_file)));
		x.Execute(); // check if exception...
	except
		on e: Hak.ErrorException do begin
			if e.FileName <> '' then source_file := e.FileName;
			writeln('ERROR: ', SysUtils.Format('%s[%u,%u] %s', [source_file, e.Line, e.Column, e.Message]));
		end;
		on e: Exception do
			writeln('ERROR: ', e.Message);
		else
			writeln('ERROR: unknonw exception');
	end;

	if x <> nil then x.Destroy();
end.
