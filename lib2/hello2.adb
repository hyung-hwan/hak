with H3.Arrays;
with H3.Strings;
with H3.CC;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Assertions;
with Interfaces.C;

use type H3.System_Size;

procedure hello2 is
	package A is new H3.Arrays(Standard.Wide_Character, 1, Wide_Character'Val(0));
	package S is new H3.Strings(Standard.Wide_Character, Wide_Character'Val(0));

	--package S_I is new H3.Arrays(Integer, 1, 16#FF#);
	Arr: A.Elastic_Array;
	Arr2: A.Elastic_Array;
	
	Str: S.Elastic_String;
	Str2: S.Elastic_String;

	use type S.Elastic_String;
begin
	A.Append (Arr, "hello");
	A.Append (Arr, "world");

	Arr.Append ("fantastic");


	Arr2 := Arr;
	A.Delete (Arr2, 1, 5);


	Str.Append ("wonderful");
	Str.Delete (2, 3);

	Str2 := Str;
	Str2.Clear;
	Str2.Append ("savage");

	Str := Str2;
	Str.Clear;
	Str.Append("primitive");

	if Str = "savage" then
		ada.text_io.put_line ("is savage");
	else
		ada.text_io.put_line ("is not savage");
	end if;
	Ada.Wide_Text_IO.Put_Line (Standard.Wide_String(A.To_Item_Array(Arr)));
	Ada.Wide_Text_IO.Put_Line (Standard.Wide_String(A.To_Item_Array(Arr2)));

	-- ---------------------
	Ada.Wide_Text_IO.Put_Line (Standard.Wide_String(Str.To_Item_Array));
	Ada.Wide_Text_IO.Put_Line (Standard.Wide_String(Str2.To_Item_Array));

	declare
		package C renames Interfaces.C;
		package CC is new H3.CC(Standard.Wide_Character);

		--function isspace(a: Standard.Character) return C.int
		--	with Import => True, Convention => C, External_Name => "isspace";
		function isspace(a: Standard.Character) return C.int;
		pragma Import (C, isspace, "isspace");

		ch: Standard.Wide_Character;
	begin
		for i in 0 .. 255 loop
			ch := Standard.Wide_Character'Val(i);
			Ada.Text_IO.Put (I'img & "[" & ch'Img & "]");

			for j in CC.Class'Range loop
				Ada.Text_IO.Put (" " & J'Img & ":" & CC.Is_Class(ch, j)'Img);	
			end loop;

			Ada.Text_IO.Put_Line ("");
		end loop;
	
	end;
end;	

