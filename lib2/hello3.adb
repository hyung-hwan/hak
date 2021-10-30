with H3.Compilers;
with ada.text_io;

procedure hello3 is
	package C is new H3.Compilers(Standard.Wide_Character);

	Compiler: C.Compiler;
begin
	Compiler.Feed ("<<=hello world");
	Compiler.End_Feed;
end hello3;
