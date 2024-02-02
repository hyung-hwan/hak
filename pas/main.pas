program main;

{$mode objfpc}{$H+}

uses HCL, sysutils;

(*
function Make(): HCL.InterpPtr;
var
	x: HCL.Interp;
begin
	x := HCL.Interp.Create(20);
	Make := @x;
end;

function Make2(): HCL.Interp;
begin
	Make2 := HCL.Interp.Create(20);
end;


var
	x: HCL.Interp;
	x2: ^HCL.Interp;
begin
	Write ('sizeof X=>');
	Writeln (SizeOf(x));
	x := HCL.Interp.Make(20);
	Write ('instance sizeof X=>');
	Writeln (x.InstanceSize());
//	x.Open();
//	x.Close();
	x.Destroy();
	//x.Free();

	Write ('sizeof X2=>');
	Writeln (SizeOf(x2));

//	New(x2);
//	x2^.Open();
//	//x2^.Destroy();
//	//x2^.Free();
//	Dispose (x2);

	x := Make2();
	//Writeln (x.handle);
	x.Destroy();
	x := nil;

//	x2 := Make();
//	Writeln (x2^.handle);
//	x2^.Destroy();
end.
*)

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
		x.Compile('(printf "hello 동키콩월드\n")');
		x.Compile('(a := 20)');
		x.Compile('(printf "%d\n" a)');

		x.Execute();
	except
		on e: Exception do
			writeln ('exception:', e.Message);
		else
			writeln ('unknonw exception');
	end;

	if x <> nil then x.Destroy();
end.