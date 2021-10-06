with H3.Arrays;
with H3.Strings;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Assertions;

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
end;

