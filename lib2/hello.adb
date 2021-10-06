with H3.Pool;
with H3.Limited_Pool;
with H3.Arrays;
with H3.Strings;
with H3.Storage_Pools;
with H3.MM;
with GNAT.Debug_Pools;
with System.Storage_Pools;
with System.Pool_Global;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Assertions;

use type H3.System_Size;

procedure hello is
	package S is new H3.Strings(Standard.Wide_Character, Wide_Character'Val(0));
	

	--type Global_Pool is new System.Storage_Pools.Root_Storage_Pool with null record;
	P1: aliased System.Pool_Global.Unbounded_No_Reclaim_Pool;
	P2: aliased GNAT.Debug_Pools.Debug_Pool;
	P3: aliased H3.Storage_Pools.Global_Pool;

	type T is record
		A: Integer := 99;
		B: Integer := 88;
		C: Float;
	end record;

	type L is limited record
		A: Integer := 1234;
		B: Integer;
		C: Float;
	end record;

	type T_Pointer is access T;
	package TP is new H3.Pool(T, T_Pointer, P1'Unchecked_Access);

	type L_Pointer is access L;
	package LP is new H3.Limited_Pool(L, L_Pointer, P1'Unchecked_Access);

	type I_Pointer is access Integer;
	package IP is new H3.Pool(Integer, I_Pointer, P1'Unchecked_Access);

 	procedure Info is new GNAT.Debug_Pools.Print_Info(Ada.Text_IO.Put_Line, Ada.Text_IO.Put);

	x: T_Pointer;
	i: I_Pointer;
	y: L_Pointer;

	SS: S.Elastic_String;

	procedure print_string_info (Str: in S.Elastic_String; Name: in Standard.String) is
		len: H3.System_Size;
		capa: H3.System_Size;
		first: H3.System_Size;
		last: H3.System_Size;
	begin
		len := S.Get_Length(Str);
		capa := S.Get_Capacity(Str);
		first := S.Get_First_Index(Str);
		last := S.Get_Last_Index(Str);
		Ada.Text_IO.Put (Name & " len:" & len'Img & " capa:" & capa'Img & " first:" & first'img & " last:" & last'img & " => ");
		Ada.Wide_Text_IO.Put_line (Standard.Wide_String(S.To_Item_Array(Str)));

		if S.Terminator_Length > 0 then
			pragma Assert (S.Get_Item(Str, S.Get_Last_Index(Str) + 1) = S.Terminator_Value);
		end if;
	end print_string_info;

begin
	declare
		TTT: H3.System_Size := H3.System_Size'Last;
		--NNN: Standard.Natural := Standard.Natural'Last;
	begin
		TTT := TTT + 1;
		ada.text_io.put_line ("-----------------");
		ada.text_io.put_line (TTT'Img);
		ada.text_io.put_line ("-----------------");

		--NNN := NNN + 1;
		--ada.text_io.put_line ("-----------------");
		--ada.text_io.put_line (NNN'Img);
		--ada.text_io.put_line ("-----------------");
	end;

	x := TP.Allocate((A => 900, B => 800, C => 1.1));
	i := IP.Allocate(200);

	y := LP.Allocate;
-- can't do this as it's limited
	--y.all := (A => 1900, B => 1800, C => 11.1);
-- this works...
	--y.A := 1900;
	y.B := 1800;
	y.C := 11.1;

	declare
		type LL_Pointer is access L;
		for LL_Pointer'Storage_Pool use P3;
		z: LL_Pointer;
		procedure Dealloc is new Ada.Unchecked_Deallocation(L, LL_Pointer);
	begin
		z := new L'(A => 9900, B => 9800, C => 99.1);	
		Ada.Text_IO.Put_Line (Z.A'Img);
		Dealloc (z);
	end;


	Ada.Text_IO.Put_Line(Integer'Image(x.A));
	Ada.Text_IO.Put_Line(Integer'Image(x.B));
	Ada.Text_IO.Put_Line(Integer'Image(i.all));
	Ada.Text_IO.Put_Line(Integer'Image(y.A));

	IP.Deallocate (i);
	TP.Deallocate (x);
	LP.Deallocate (y);
	
	--GNAT.Debug_Pools.Print_Info_Stdout(P2);
	--GNAT.Debug_Pools.Dump_Stdout(P2,  100);
	
	declare
		str: S.Elastic_String;
		str2: S.Elastic_String;
		
	begin
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 0);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 0);

		S.Append(Str, "Hello, world!");
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 13);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 13);

		S.Append(Str, "");
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 13);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 13);

		S.Append(Str, ' ', 0);
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 13);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 13);

		S.Prepend(Str, ' ', 0);
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 13);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 13);

		S.Append(Str, ' ', 2);
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 15);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 15);
		
		S.Append(Str, "donkey");
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 21);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 21);

		S.Prepend(Str, "Oh! ");
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 25);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 25);

		declare
			-- unsafe way to access the internal buffer.
			--arr: constant S.P.Item_Array := S.To_Item_Array(Str);
			arr: constant S.Character_Array := S.To_Item_Array(Str);
		begin
			Ada.Wide_Text_IO.Put ("STR[1] => [");	
			for i in arr'Range loop
				Ada.Wide_Text_IO.Put (arr(i));	
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");

			Ada.Wide_Text_IO.Put ("STR[2] => [");	
			for i in S.Get_First_Index(Str) .. S.Get_Last_Index(Str) loop
				Ada.Wide_Text_IO.Put (S.Get_Item(Str, i));	
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");

			Ada.Wide_Text_IO.Put ("STR[3] => [");	
			Ada.Wide_Text_IO.Put (Standard.Wide_String(arr));
			Ada.Wide_Text_IO.Put_Line ("]");
		end;

		pragma Assert (S."="(Str, "Oh! Hello, world!  donkey"));

		S.Append (Str, '>');
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 26);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 26);
		pragma Assert (S."="(Str, "Oh! Hello, world!  donkey>"));


		S.Append (Str, "donkeyX");
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 33);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 33);
		pragma Assert (S."="(Str, "Oh! Hello, world!  donkey>donkeyX"));

		S.Append (Str, "ABCDE");
		print_string_info (Str, "Str");
		pragma Assert (S.Get_Length(Str) = 38);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 38);
		pragma Assert (S."="(Str, "Oh! Hello, world!  donkey>donkeyXABCDE"));

		Str2 := Str;
		S.Append (Str2, "EXTRA");
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 43);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 43);
		pragma Assert (S."="(Str2, "Oh! Hello, world!  donkey>donkeyXABCDEEXTRA"));
		pragma Assert (S.Get_Length(Str) = 38);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 38);
		pragma Assert (S."="(Str, "Oh! Hello, world!  donkey>donkeyXABCDE"));

		S.Append (Str2, " THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3");
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 91);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 91);
		pragma Assert (S."="(Str2, "Oh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));
		pragma Assert (S.Get_Length(Str) = 38);
		pragma Assert (S.Get_First_Index(Str) = 1);
		pragma Assert (S.Get_Last_Index(Str) = 38);
		pragma Assert (S."="(Str, "Oh! Hello, world!  donkey>donkeyXABCDE"));
	
		S.Replace (Str2, 1, 1, 'Q');
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 91);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 91);
		pragma Assert (S."="(Str2, "Qh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Insert (Str2, 1, 'B');
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 92);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 92);
		pragma Assert (S."="(Str2, "BQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Insert (Str2, 1, 'A', 3);
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 95);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 95);
		pragma Assert (S."="(Str2, "AAABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Insert (Str2, 3, 'C', 2);
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 97);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 97);
		pragma Assert (S."="(Str2, "AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Insert (Str2, 5, "");
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 97);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 97);
		pragma Assert (S."="(Str2, "AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Insert (Str2, S.Get_Last_Index(Str2) + 1, "");
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 97);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 97);
		pragma Assert (S."="(Str2, "AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Insert (Str2, S.Get_Last_Index(Str2) + 1, " => ABCDEF");
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 107);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 107);
		pragma Assert (S."="(Str2, "AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3 => ABCDEF"));

		S.Prepend (Str2, '>', 3);
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 110);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 110);
		pragma Assert (S."="(Str2, ">>>AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3 => ABCDEF"));

		S.Delete (Str2, 1, 3);
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 107);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 107);
		pragma Assert (S."="(Str2, "AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3 => ABCDEF"));

		S.Delete (Str2, S.Get_Last_Index(Str2) - 9, S.Get_Last_Index(Str2));
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 97);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 97);
		pragma Assert (S."="(Str2, "AACCABQh! Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Delete (Str2, 5, 9);
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 92);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 92);
		pragma Assert (S."="(Str2, "AACC Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Replace (Str2, 1, 5, ""); 
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 87);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 87);
		pragma Assert (S."="(Str2, "Hello, world!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Replace (Str2, 8, 12, "cougar"); 
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 88);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 88);
		pragma Assert (S."="(Str2, "Hello, cougar!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3"));

		S.Replace (Str2, S.Get_Last_Index(Str2) - 1, S.Get_Last_Index(Str2) + 100, "HH"); 
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 88);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 88);
		pragma Assert (S."="(Str2, "Hello, cougar!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR HH"));

		S.Replace (Str2, 8, 13, "bee"); 
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 85);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 85);
		pragma Assert (S."="(Str2, "Hello, bee!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR HH"));

		S.Replace (Str2, 8, 10, "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"); 
		print_string_info (Str2, "Str2");
		pragma Assert (S.Get_Length(Str2) = 160);
		pragma Assert (S.Get_First_Index(Str2) = 1);
		pragma Assert (S.Get_Last_Index(Str2) = 160);
		pragma Assert (S."="(Str2, "Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ!  donkey>donkeyXABCDEEXTRA THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR HH"));

		declare
			--arr: constant S.P.Thin_Item_Array_Pointer := S.Get_Slot_Pointer(Str);
			--arr2: constant S.P.Thin_Item_Array_Pointer := S.Get_Slot_Pointer(Str2);
			arr: constant S.Thin_Character_Array_Pointer := S.Get_Slot_Pointer(Str);
			arr2: constant S.Thin_Character_Array_Pointer := S.Get_Slot_Pointer(Str2);
			use type H3.System_Word;
		begin
			print_string_info (Str, "Str");
			
			Ada.Wide_Text_IO.Put ("STR(By-Pointer) [");	
			for i in S.Get_First_Index(Str) .. S.Get_Last_Index(Str) + S.Terminator_Length loop -- this must loop to the terminating null.
				Ada.Wide_Text_IO.Put (arr.all(i));
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");	

			print_string_info (Str2, "Str2");
		
			Ada.Wide_Text_IO.Put ("Str2(By-Pointer) [");	 -- this must loop to the terminating null.
			for i in S.Get_First_Index(Str2) .. S.Get_Last_Index(Str2) + S.Terminator_Length loop
				Ada.Wide_Text_IO.Put (arr2.all(i));
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");	
		end;

		S.Clear (Str2);
		print_string_info (Str2, "Str2");

		--declare
		--	arr: constant Standard.Wide_String := S.To_Item_Array(str);
		--begin
		--	Ada.Wide_Text_IO.Put_Line (arr);	
		--end;
		SS := Str;

	end;

	declare
		type R_Record is record
			X: Standard.Integer := 3;
			Y: Standard.Integer := 4;
		end record;
		package Q is new H3.MM(R_Record);

		T: Q.Ref_Counted;
		T2: Q.Ref_Counted;

		P: Q.Item_Pointer;
	begin

		declare
			T3: Q.Ref_Counted;
		begin
			Q.Create (T3, (X => 20, Y => 30));	
			T := T3;
			--Q.Create (T);
		end;

		P := Q.Get_Item_Pointer(T);
		T2 := T;
		Q.Get_Item_Pointer(T).X := 12345;
		Ada.Text_IO.Put_Line(Q.Get_Item_Pointer(T).Y'Img);
		Ada.Text_IO.Put_Line(Q.Get_Item_Pointer(T).X'Img);
		
		Ada.Text_IO.Put_Line(Q.Get_Item_Pointer(T2).Y'Img);
		Ada.Text_IO.Put_Line(Q.Get_Item_Pointer(T2).X'Img);
	end;


	declare
	package S_I is new H3.Arrays(Integer, 1, 16#FF#);
		t1: S_I.Elastic_Array;
	begin
		S_I.Append (t1, 20, 5);
		S_I.Prepend (t1, 30, 2);
		S_I.Append (t1, 30, 5);

		Ada.Text_IO.Put_Line ("-------------------------------");
		for i in S_I.Get_First_Index(t1) .. S_I.Get_Last_Index(t1) loop
			Ada.Text_IO.Put  (" " & S_I.Get_Item(t1, i)'Img);
		end loop;
		Ada.Text_IO.Put_Line ("");

		Ada.Text_IO.Put_Line (t1.Find(30, t1.Get_Last_Index, S_I.DIRECTION_BACKWARD)'Img);
		Ada.Text_IO.Put_Line (t1.Find(30, t1.Get_First_Index)'Img);
		Ada.Text_IO.Put_Line (t1.Find(90, t1.Get_First_Index)'Img);
		Ada.Text_IO.Put_Line (t1.Find(90, t1.Get_First_Index)'Img);

		Ada.Text_IO.Put_Line (t1.Find((30, 30, 30, 30), t1.Get_First_Index)'Img);
		Ada.Text_IO.Put_Line (t1.Find((20, 20), t1.Get_First_Index, S_I.DIRECTION_FORWARD)'Img);
		Ada.Text_IO.Put_Line (t1.Find((20, 20), t1.Get_Last_Index, S_I.DIRECTION_BACKWARD)'Img);
		Ada.Text_IO.Put_Line (t1.Find((30, 20, 20), t1.Get_First_Index, S_I.DIRECTION_FORWARD)'Img);
		Ada.Text_IO.Put_Line (t1.Find((30, 20, 20), t1.Get_Last_Index, S_I.DIRECTION_BACKWARD)'Img);
	end;
end;

