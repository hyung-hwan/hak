with H3.Pool;
with H3.Limited_Pool;
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
	package S is new H3.Strings(Wide_Character, Wide_Character'Val(0));

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
		len: H3.System_Size;
		capa: H3.System_Size;
		first: H3.System_Size;
		last: H3.System_Size;
	begin
		len := S.Get_Length(Str);
		capa := S.Get_Capacity(Str);
		first := S.Get_First_Index(Str);
		last := S.Get_Last_Index(Str);
		Ada.Text_IO.Put_Line ("length=>" & len'Img & " Capacity=>" & capa'Img & " First=>" & first'img & " Last=>" & last'img);

		S.Append(Str, "Hello, world");
		len := S.Get_Length(Str);
		capa := S.Get_Capacity(Str);
		first := S.Get_First_Index(Str);
		last := S.Get_Last_Index(Str);
		Ada.Text_IO.Put_Line ("length=>" & len'Img & " Capacity=>" & capa'Img & " First=>" & first'img & " Last=>" & last'img);
		

		S.Append(Str, "");
		len := S.Get_Length(Str);
		capa := S.Get_Capacity(Str);
		first := S.Get_First_Index(Str);
		last := S.Get_Last_Index(Str);
		Ada.Text_IO.Put_Line ("length=>" & len'Img & " Capacity=>" & capa'Img & " First=>" & first'img & " Last=>" & last'img);
		
--		S.Append(Str, "donkey");
		len := S.Get_Length(Str);
		capa := S.Get_Capacity(Str);
		first := S.Get_First_Index(Str);
		last := S.Get_Last_Index(Str);
		Ada.Text_IO.Put_Line ("length=>" & len'Img & " Capacity=>" & capa'Img & " First=>" & first'img & " Last=>" & last'img);
		
		
		declare
			arr: constant S.Character_Array := S.To_Character_Array(str);
		begin
			Ada.Wide_Text_IO.Put ("[");	
			for i in arr'Range loop
				Ada.Wide_Text_IO.Put (arr(i));	
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");	

			Ada.Wide_Text_IO.Put ("PRINTING AGAIN [");	
			Ada.Wide_Text_IO.Put (Standard.Wide_String(arr));
			Ada.Wide_Text_IO.Put_Line ("]");	
		end;
		
		-- unsafe way to access the internal buffer.
		S.Append (Str, 'X');
		S.Append(Str, "donkeyX");
		S.Append(Str, "ABCDE");

		Str2 := Str;
		S.Append (Str2, "EXTRA");
		S.Append (Str2, " THIS IS FANTASTIC ELASTIC STRING WRITTEN FOR H3");
	
		S.Replace (Str2, 1, 'Q');
		--S.Replace (Str2, 10000, 'Q'); -- constraint error

		declare
			arr: constant S.Thin_Character_Array_Pointer := S.Get_Slot_Pointer(Str);
			arr2: constant S.Thin_Character_Array_Pointer := S.Get_Slot_Pointer(Str2);
			use type H3.System_Word;
		begin
			Ada.Assertions.Assert (S.Get_Length(Str) = 25, "invalid string length");
			Ada.Assertions.Assert (S.Get_Length(Str2) = 78, "invalid string length");

			len := S.Get_Length(Str);
			capa := S.Get_Capacity(Str);
			first := S.Get_First_Index(Str);
			last := S.Get_Last_Index(Str);
			Ada.Text_IO.Put_Line ("STR length=>" & len'Img & " Capacity=>" & capa'Img & " First=>" & first'img & " Last=>" & last'img);
			
			Ada.Wide_Text_IO.Put ("STR(By-Pointer) [");	
			for i in S.Get_First_Index(Str) .. S.Get_Last_Index(Str) + 1 loop -- this must loop to the terminating null.
				Ada.Wide_Text_IO.Put (arr.all(i));
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");	

			len := S.Get_Length(Str2);
			capa := S.Get_Capacity(Str2);
			first := S.Get_First_Index(Str2);
			last := S.Get_Last_Index(Str2);
			Ada.Text_IO.Put_Line ("STR2 length=>" & len'Img & " Capacity=>" & capa'Img & " First=>" & first'img & " Last=>" & last'img);
		
			Ada.Wide_Text_IO.Put ("Str2(By-Pointer) [");	 -- this must loop to the terminating null.
			for i in S.Get_First_Index(Str2) .. S.Get_Last_Index(Str2) + 1 loop
				Ada.Wide_Text_IO.Put (arr2.all(i));
			end loop;
			Ada.Wide_Text_IO.Put_Line ("]");	
		end;

		--declare
		--	arr: constant Standard.Wide_String := S.To_Character_Array(str);
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

end;

