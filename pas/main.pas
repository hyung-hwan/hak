program main;

{$mode objfpc}{$H+}

uses HAK, SysUtils;

var
	x: HAK.Interp = nil;
begin
	(* System.ParamCount() returns only the number of argumetns except System.ParamStr(0). It is the upper bound to System.ParamStr(). *)
	if System.ParamCount() <> 1 then begin
		writeln (System.Stderr, SysUtils.Format('Usage: %s <filename>', [SysUtils.ExtractFileName(System.ParamStr(0))]));
		System.Halt(-1);
	end;

	try
		x := HAK.Interp.Create(100);
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

		x.CompileFile(pansichar(ansistring(System.ParamStr(1))));
		x.Execute(); // check if exception...
	except
		on e: Exception do
			writeln ('exception: ', e.Message);
		else
			writeln ('unknonw exception');
	end;

	if x <> nil then x.Destroy();
end.