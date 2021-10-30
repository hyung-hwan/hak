with H3.Arrays;
with H3.Strings;
with H3.Runes;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Assertions;
with Interfaces.C;
--with Interfaces.C.Strings;
with System;

use type H3.System_Size;

procedure hello2 is
	package A is new H3.Arrays(Standard.Wide_Character, 1, Wide_Character'First);
	package S is new H3.Strings(Standard.Wide_Character);
	package R is new H3.Runes(Standard.Wide_Character);
	package C renames Interfaces.C;

	--package S_I is new H3.Arrays(Integer, 1, 16#FF#);
	Arr: A.Elastic_Array;
	Arr2: A.Elastic_Array;
	
	Str: S.Elastic_String;
	Str2: S.Elastic_String;

	use type S.Elastic_String;

	--procedure setlocale(a: C.int; b: Interfaces.C.Strings.chars_ptr);
	procedure setlocale(a: C.int; b: System.Address);
	pragma Import (C, setlocale, "setlocale");

	function is_class (V: Standard.Wide_Character; Class: R.Item_Class) return Standard.Boolean is
		function iswalpha(a: C.int) return C.int;
		pragma Import (C, iswalpha, "iswalpha");
		function iswalnum(a: C.int) return C.int;
		pragma Import (C, iswalnum, "iswalnum");
		function iswblank(a: C.int) return C.int;
		pragma Import (C, iswblank, "iswblank");
		function iswcntrl(a: C.int) return C.int;
		pragma Import (C, iswcntrl, "iswcntrl");
		function iswdigit(a: C.int) return C.int;
		pragma Import (C, iswdigit, "iswdigit");
		function iswgraph(a: C.int) return C.int;
		pragma Import (C, iswgraph, "iswgraph");
		function iswlower(a: C.int) return C.int;
		pragma Import (C, iswlower, "iswlower");
		function iswprint(a: C.int) return C.int;
		pragma Import (C, iswprint, "iswprint");
		function iswpunct(a: C.int) return C.int;
		pragma Import (C, iswpunct, "iswpunct");
		function iswspace(a: C.int) return C.int;
		pragma Import (C, iswspace, "iswspace");
		function iswupper(a: C.int) return C.int;
		pragma Import (C, iswupper, "iswupper");
		function iswxdigit(a: C.int) return C.int;
		pragma Import (C, iswxdigit, "iswxdigit");

		use type C.int;
		X: C.int := Standard.Wide_Character'Pos(V);
	begin
		case Class is
			when R.ALPHA => return IswAlpha(X) /= 0;
			when R.ALNUM => return IswAlnum(X) /= 0;
			when R.BLANK => return IswBlank(X) /= 0;
			when R.CNTRL => return IswCntrl(X) /= 0;
			when R.DIGIT => return IswDigit(X) /= 0;
			when R.GRAPH => return IswGraph(X) /= 0;
			when R.LOWER => return IswLower(X) /= 0;
			when R.PRINT => return IswPrint(X) /= 0;
			when R.PUNCT => return IswPunct(X) /= 0;
			when R.SPACE => return IswSpace(X) /= 0;
			when R.UPPER => return IswUpper(X) /= 0;
			when R.XDIGIT => return IswXdigit(X) /= 0;
		end case;
	end is_class;

	--Empty_String: aliased Standard.String := "en_US.utf8" & Standard.Character'Val(0);
	Empty_String: aliased Standard.String := "" & Standard.Character'Val(0);
begin
	--setlocale (6, Interfaces.C.Strings.To_Chars_Ptr(Empty_String'access));
	setlocale (6, Empty_String'Address);

	A.Append (Arr, "hello");
	A.Append (Arr, "world");

	Arr.Append ("fantastic");

	Arr2 := Arr;
	A.Delete (Arr2, 1, 5);

	Str.Append ("wonderful");
	Str.Delete (2, 3);

	Str2 := Str;
	Str2.Clear;
	Str2.Append ("saxage");

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
		--function isspace(a: Standard.Character) return C.int
		--	with Import => True, Convention => C, External_Name => "isspace";
		ch: Standard.Wide_Character;
	begin
		for i in 0 .. 10000 loop
			ch := Standard.Wide_Character'Val(i);
			Ada.Text_IO.Put (I'img & "[" & ch'Img & "]");

			for j in R.Item_Class'Range loop
				Ada.Text_IO.Put (" " & J'Img & ":" & R.Is_Class(ch, j)'Img);
				if R.Is_Class(ch, j) /= Is_Class(ch, j) then
					Ada.Text_IO.Put ("[X]");
				--else
				--	Ada.Text_IO.Put ("[O]");
				end if;
			end loop;

			Ada.Text_IO.Put_Line ("");
		end loop;
	
		Ada.Text_IO.Put_line (R.Is_Alpha('σ')'Img);
	end;
end;	

