program main;

{$mode objfpc}{$H+}

uses HCL, sysutils;

var
	x: HCL.Interp = nil;
begin
	try
		x := HCL.Interp.Create(100);
		x.Ignite(0);

		x.AddBuiltinPrims();
		//x.AttachCCIO();
		//x.AttachUDIO();

		x.Compile(pwidechar('(printf "hello 동키콩\n")'));
		x.Compile('(printf "hello 동키콩월드\n") ');
		x.Compile('(동가리오 := 20)');
		x.Compile('(printf "%d %d\n" 동가리오 (+ 동가리오 동가리오))');

		x.Compile(pwidechar('(printf "%d %d\n" 동가리오 (동가리오 * 동가리오))'#10'printf "hello, world\n";;;'#10));

		x.Execute(); // check if exception...
	except
		on e: Exception do
			writeln ('exception:', e.Message);
		else
			writeln ('unknonw exception');
	end;

	if x <> nil then x.Destroy();
end.