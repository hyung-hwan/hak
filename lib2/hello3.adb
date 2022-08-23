with H3.Compilers;
with H3.Storage;

procedure hello3 is
	package H3C is new H3.Compilers(Standard.Wide_Character, H3.Storage.Global_Pool_Box);
	Compiler: H3C.Compiler;
begin
	Compiler.Feed ("#include ""abc.txt""; donkey");
	Compiler.End_Feed;
end hello3;
