generic 
	type Rune_Type is (<>);
	with package Storage_Pool_box is new H3.Storage.Pool_Box(<>);
package H3.IO is

	type Stream is abstract tagged limited null record;

	procedure Open (Handle: in out Stream; ) is abstract;
	procedure Close (Handle: in out Stream) is abstract;
	procedure Read (Handle: in out Stream; Data: out System_Byte_Array; Last: out System_Size) is abstract;
	procedure Write (Handle: in out Stream; Data: in System_Byte_Array; Last: out System_Size) is abstract;


	type File_Stream is new Stream with record
		Name: ...
		Handle: Ada.Wide_Text_IO.File_Type;
	end record;


	
end H3.IO;
