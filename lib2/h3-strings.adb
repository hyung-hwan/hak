with Ada.Unchecked_Deallocation;

package body H3.Strings is
	BUFFER_ALIGN: constant := 128; -- TODO: change it to a reasonably large value.

	type Shift_Direction is (SHIFT_LEFT, SHIFT_RIGHT);

	function To_Item_Array (Str: in Elastic_String) return Item_Array is
	begin
		return Str.Buffer.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last);
	end To_Item_Array;

	-- return the buffer capacity excluding the terminator
	function Get_Capacity (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Slot'Length - Terminator_Length;
	end Get_Capacity;

	-- private. return the buffer capacity including the terminator
	function Get_Hard_Capacity (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Slot'Length;
	end Get_Hard_Capacity;
	pragma Inline (Get_Hard_Capacity);

	function Get_Length (Str: in Elastic_String) return System_Size is
	begin
		return 1 + Str.Buffer.Last - Str.Buffer.Slot'First;
	end Get_Length;

	function Get_First_Index (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Slot'First;
	end Get_First_Index;

	function Get_Last_Index (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Last;
	end Get_Last_Index;

	function Get_Item (Str: in Elastic_String; Pos: in System_Index) return Item_Type is
	begin
		return Str.Buffer.Slot(Pos);
	end Get_Item;

	-- unsafe as it exposes the internal buffer which can go away.
	-- assume the system address is equal to the thin pointer in size.
	function Get_Slot_Pointer (Str: in Elastic_String) return Thin_Item_Array_Pointer is
		A: System.Address := Str.Buffer.Slot(Str.Buffer.Slot'First)'Address;
		P: Thin_Item_Array_Pointer;
		for P'Address use A'Address;
		pragma Import (Ada, P);
	begin
		return P;
	end Get_Slot_Pointer;

	function Is_Shared(Str: in Elastic_String) return Standard.Boolean is
	begin
		return Str.Buffer /= Empty_Buffer'Access and then Str.Buffer.Refs > 1;
	end Is_Shared;

	procedure Free_Buffer (Str: in out Elastic_String) is
	begin
		if Str.Buffer /= Empty_Buffer'Access then
			declare
				procedure Free is new Ada.Unchecked_Deallocation(Buffer_Record, Buffer_Pointer);
			begin
				Free (Str.Buffer);
			end;
		end if;
	end Free_Buffer;

	procedure Ref_Buffer (Buf: in out Buffer_Pointer) is
	begin
		if Buf /= Empty_Buffer'Access then
			Buf.Refs := Buf.Refs + 1;
		end if;
	end Ref_Buffer;

	procedure Unref_Buffer (Buf: in out Buffer_Pointer) is
	begin
		if Buf /= Empty_Buffer'Access then
			if Buf.Refs = 1 then
				declare
					procedure Free is new Ada.Unchecked_Deallocation(Buffer_Record, Buffer_Pointer);
				begin
					Free (Buf);
				end;
				Buf := Empty_Buffer'Access;
			else
				Buf.Refs := Buf.Refs - 1;
			end if;
		end if;
	end Unref_Buffer;

	function New_Buffer_Container (Hard_Capa: in System_Size) return Elastic_String is
		Tmp: Elastic_String;
	begin
		Tmp.Buffer := new Buffer_Record(Hard_Capa);
		Tmp.Buffer.Refs := 1;
		return Tmp;
	end New_Buffer_Container;

	-- prepare the buffer for writing 
	procedure Prepare_Buffer (Str: in out Elastic_String) is
		Tmp: Elastic_String;
	begin
		if Str.Buffer /= Empty_Buffer'Access then
			if Is_Shared(Str) then
				-- The code like this doesn't work correctly in terms of finalization.
				-- The buffer pointer held inside a finalization controlled record must be 
				-- manipluated through the record itself. otherwise, the Adjust and Finalize
				-- calls goes incompatible with the reference counting implementation.
				-- It is because finalization is set on the record rather than the buffer pointer.
				--Tmp: Buffer_Pointer;
				--Tmp := new Buffer_Record(Get_Hard_Capacity(Str));
				--Tmp.Slot := Str.Buffer.Slot;
				--Tmp.Last := Str.Buffer.Last;
				--Tmp.Refs := 1;
				--Unref_Buffer (Str.Buffer);
				--Str.Buffer := Tmp;
				Tmp := Str;
				Str := New_Buffer_Container(Get_Hard_Capacity(Str));
				Str.Buffer.Slot := Tmp.Buffer.Slot;
				Str.Buffer.Last := Tmp.Buffer.Last;
			end if;
		end if;
	end Prepare_Buffer;

	-- prepare the buffer for writing 
	procedure Prepare_Buffer (Str: in out Elastic_String; Req_Hard_Capa: in System_Size; Shift_Pos: in System_Size := 0; Shift_Size: in System_Size := 0; Shift_Dir: in Shift_Direction := Shift_Right) is
		Tmp: Elastic_String;
		First, Last: System_Size;
		Hard_Capa: System_Size;
	begin
		First := Get_First_Index(Str);
		Last := Get_Last_Index(Str);

		if Str.Buffer /= Empty_Buffer'Access and then Is_Shared(Str) then
			if Req_Hard_Capa < Get_Hard_Capacity(Str) then
				Hard_Capa := Get_Hard_Capacity(Str);
			else
				Hard_Capa := Req_Hard_Capa;
			end if;

			Tmp := Str;
			Str := New_Buffer_Container(Hard_Capa);
			goto COPY_OVER;
		else
			if Req_Hard_Capa > Get_Hard_Capacity(Str) then
				Tmp := Str;
				Str := New_Buffer_Container(Req_Hard_Capa);
				goto COPY_OVER;
			elsif Shift_Pos > 0 then
				Tmp := Str;
				goto COPY_OVER_WITH_SHIFT;
			else
				-- no shift, no change in the buffer
				null;
			end if;
		end if;

		return;

	<<COPY_OVER>>
		if Shift_Pos <= 0 then
			-- no shift is required. copy the entire string including th
			Str.Buffer.Slot(First .. Last + Terminator_Length) := Tmp.Buffer.Slot(First .. Last + Terminator_Length);
			Str.Buffer.Last := Last;
			return;
		end if;
	<<COPY_OVER_WITH_SHIFT>>
		-- it is an internal function. perform no sanity check.
		-- if Shift_Pos or Shift_Size is beyond the allocated capacity, 
		-- it will end up in an exception.
		if Shift_Dir = SHIFT_LEFT then
			declare
				Mid: System_Size := Shift_Pos - Shift_Size;
			begin
				Str.Buffer.Slot(First .. Mid) := Tmp.Buffer.Slot(First .. Mid);
				Str.Buffer.Slot(Mid + 1 .. Last - Shift_Size + Terminator_Length) := Tmp.Buffer.Slot(Shift_Pos + 1 .. Last + Terminator_Length);
				Str.Buffer.Last := Last - Shift_Size;
			end;
		else
			Str.Buffer.Slot(First .. Shift_Pos - 1) := Tmp.Buffer.Slot(First .. Shift_Pos - 1);
			Str.Buffer.Slot(Shift_Pos + Shift_Size .. Last + Shift_Size + Terminator_Length) := Tmp.Buffer.Slot(Shift_Pos .. Last + Terminator_Length);
			Str.Buffer.Last := Last + Shift_Size;
		end if;
	end Prepare_Buffer;

	procedure Clear (Str: in out Elastic_String) is
	begin
		Prepare_Buffer (Str);
		Str.Buffer.Last := Get_First_Index(Str) - 1;
	end Clear;

	procedure Purge (Str: in out Elastic_String) is
	begin 
		Unref_Buffer (Str.Buffer);
		Str.Buffer := Empty_Buffer'Access;
	end Purge;

	function Calc_Inc_Capa (Str: in Elastic_String; Inc: in System_Size) return System_Size is
	begin
		return H3.Align(Get_Length(Str) + Inc + Terminator_Length, BUFFER_ALIGN);
	end Calc_Inc_Capa;

	procedure Insert (Str: in out Elastic_String; Pos: in System_Index; V: in Item_Type; Repeat: in System_Size := 1) is
		Act_Pos: System_Index := Pos;
		Act_Inc: System_Size := Repeat;
	begin
		if Act_Pos > Str.Buffer.Last then
			Act_Pos := Str.Buffer.Last + 1;
		end if;

		Prepare_Buffer (Str, Calc_Inc_Capa(Str, Act_Inc), Act_Pos, Act_Inc);
		Str.Buffer.Slot(Act_Pos .. Act_Pos + Act_Inc - 1) := (others => V);
	end Insert;

	procedure Insert (Str: in out Elastic_String; Pos: in System_Index; V: in Item_Array) is
		Act_Pos: System_Index := Pos;
	begin
		if Act_Pos > Str.Buffer.Last then
			Act_Pos := Str.Buffer.Last + 1;
		end if;

		Prepare_Buffer (Str, Calc_Inc_Capa(Str, V'Length), Act_Pos, V'Length);
		Str.Buffer.Slot(Act_Pos .. Act_Pos + V'Length - 1) := V;
	end Insert;

-- TODO: operator "&" that returns a new Elastic_String
	procedure Append (Str: in out Elastic_String; V: in Item_Type; Repeat: in System_Size := 1) is
	begin
		Insert (Str, Get_Last_Index(Str) + 1, V, Repeat);
	end Append;

	procedure Append (Str: in out Elastic_String; V: in Item_Array) is
	begin
		Insert (Str, Get_Last_Index(Str) + 1, V);
	end Append;

	procedure Prepend (Str: in out Elastic_String; V: in Item_Type; Repeat: in System_Size := 1) is
	begin
		Insert (Str, Get_First_Index(Str), V, Repeat);
	end Prepend;

	procedure Prepend (Str: in out Elastic_String; V: in Item_Array) is
	begin
		Insert (Str, Get_First_Index(Str), V);
	end Prepend;

	procedure Replace (Str: in out Elastic_String; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Type; Repeat: in System_Size := 1) is
		Act_To_Pos, Repl_Len: System_Size;
	begin
		if From_Pos <= To_Pos and then From_Pos <= Str.Buffer.Last then
			Act_To_Pos := To_Pos;
			if Act_To_Pos > Str.Buffer.Last then
				Act_To_Pos := Str.Buffer.Last;
			end if;

			Repl_Len := Act_To_Pos - From_Pos + 1;
			if Repeat < Repl_Len then
				Prepare_Buffer (Str, Get_Hard_Capacity(Str), Act_To_Pos, Repl_Len - Repeat, SHIFT_LEFT);
				Act_To_Pos := From_Pos + Repeat - 1;
			elsif Repeat > Repl_Len then
				Prepare_Buffer (Str, Calc_Inc_Capa(Str, Repeat - Repl_Len), From_Pos, Repeat - Repl_Len, SHIFT_RIGHT);
				Act_To_Pos := From_Pos + Repeat - 1;
			else
				Prepare_Buffer (Str);
			end if;
			Str.Buffer.Slot(From_Pos .. Act_To_Pos) := (others => V);
		end if;
	end Replace;

	procedure Replace (Str: in out Elastic_String; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Array) is
		Act_To_Pos, Repl_Len: System_Size;
	begin
		if From_Pos <= To_Pos and then From_Pos <= Str.Buffer.Last then
			Act_To_Pos := To_Pos;
			if Act_To_Pos > Str.Buffer.Last then
				Act_To_Pos := Str.Buffer.Last;
			end if;

			Repl_Len := Act_To_Pos - From_Pos + 1;
			if V'Length < Repl_Len then
				Prepare_Buffer (Str, Get_Hard_Capacity(Str), Act_To_Pos, Repl_Len - V'Length, SHIFT_LEFT);
				Act_To_Pos := From_Pos + V'Length - 1;
			elsif V'Length > Repl_Len then
				Prepare_Buffer (Str, Calc_Inc_Capa(Str, V'Length - Repl_Len), From_Pos, V'Length - Repl_Len, SHIFT_RIGHT);
				Act_To_Pos := From_Pos + V'Length - 1;
			else
				Prepare_Buffer (Str);	
			end if;
			Str.Buffer.Slot(From_Pos .. Act_To_Pos) := V;
		end if;
	end Replace;

	procedure Delete (Str: in out Elastic_String; From_Pos: in System_Index; To_Pos: in System_Size) is
		Act_To_Pos: System_Size;
	begin
		if From_Pos <= To_Pos and then From_Pos <= Str.Buffer.Last then
			Act_To_Pos := To_Pos;
			if Act_To_Pos > Str.Buffer.Last then
				Act_To_Pos := Str.Buffer.Last;
			end if;
			Prepare_Buffer (Str, Get_Hard_Capacity(Str), Act_To_Pos, Act_To_Pos - From_Pos + 1, SHIFT_LEFT);
		end if;
	end Delete;

	function "=" (Str: in Elastic_String; Str2: in Elastic_String) return Standard.Boolean is
	begin
		return Str.Buffer = Str2.Buffer or else Str.Buffer.Slot(Get_First_Index(Str) .. Get_Last_Index(Str)) = Str2.Buffer.Slot(Get_First_Index(Str2) .. Get_Last_Index(Str2));
	end "=";

	function "=" (Str: in Elastic_String; Str2: in Item_Array) return Standard.Boolean is
	begin
		return Str.Buffer.Slot(Get_First_Index(Str) .. Get_Last_Index(Str)) = Str2;
	end "=";

	-- ---------------------------------------------------------------------
	-- Controlled Management
	-- ---------------------------------------------------------------------
	procedure Initialize (Str: in out Elastic_String) is
	begin
		-- the string is initialized to the empty buffer all the time.
		-- there is no need to reference the buffer.
		null;
	end Initialize;

	procedure Adjust (Str: in out Elastic_String) is
	begin
		Ref_Buffer (Str.Buffer);
	end Adjust;

	procedure Finalize (Str: in out Elastic_String) is
	begin
		Unref_Buffer (Str.Buffer);
	end Finalize;

end H3.Strings;
