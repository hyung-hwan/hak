with Ada.Unchecked_Deallocation;

package body H3.Arrays is
	BUFFER_ALIGN: constant := 128; -- TODO: change it to a reasonably large value.

	function To_Item_Array (Obj: in Elastic_Array) return Item_Array is
	begin
		return Obj.Buffer.Slot(Obj.Buffer.Slot'First .. Obj.Buffer.Last);
	end To_Item_Array;

	-- return the buffer capacity excluding the terminator
	function Get_Capacity (Obj: in Elastic_Array) return System_Size is
	begin
		return Obj.Buffer.Slot'Length - Terminator_Length;
	end Get_Capacity;

	-- private. return the buffer capacity including the terminator
	function Get_Hard_Capacity (Obj: in Elastic_Array) return System_Size is
	begin
		return Obj.Buffer.Slot'Length;
	end Get_Hard_Capacity;
	pragma Inline (Get_Hard_Capacity);

	function Get_Length (Obj: in Elastic_Array) return System_Size is
	begin
		return 1 + Obj.Buffer.Last - Obj.Buffer.Slot'First;
	end Get_Length;

	function Get_First_Index (Obj: in Elastic_Array) return System_Size is
	begin
		return Obj.Buffer.Slot'First;
	end Get_First_Index;

	function Get_Last_Index (Obj: in Elastic_Array) return System_Size is
	begin
		return Obj.Buffer.Last;
	end Get_Last_Index;

	function Get_Item (Obj: in Elastic_Array; Pos: in System_Index) return Item_Type is
	begin
		return Obj.Buffer.Slot(Pos);
	end Get_Item;

	-- unsafe as it exposes the internal buffer which can go away.
	-- assume the system address is equal to the thin pointer in size.
	function Get_Slot_Pointer (Obj: in Elastic_Array) return Thin_Item_Array_Pointer is
		A: System.Address := Obj.Buffer.Slot(Obj.Buffer.Slot'First)'Address;
		P: Thin_Item_Array_Pointer;
		for P'Address use A'Address;
		pragma Import (Ada, P);
	begin
		return P;
	end Get_Slot_Pointer;

	function Is_Shared(Obj: in Elastic_Array) return Boolean is
	begin
		return Obj.Buffer /= Empty_Buffer'Access and then Obj.Buffer.Refs > 1;
	end Is_Shared;

	procedure Ref_Buffer (Buf: in Buffer_Pointer) is
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

	function New_Buffer_Container (Hard_Capa: in System_Size) return Elastic_Array is
		Tmp: Elastic_Array;
	begin
		Tmp.Buffer := new Buffer_Record(Hard_Capa);
		Tmp.Buffer.Refs := 1;
		return Tmp;
	end New_Buffer_Container;

	-- prepare the buffer for writing
	procedure Prepare_Buffer (Obj: in out Elastic_Array) is
		Tmp: Elastic_Array;
	begin
		if Obj.Buffer /= Empty_Buffer'Access then
			if Is_Shared(Obj) then
				-- The code like this doesn't work correctly in terms of finalization.
				-- The buffer pointer held inside a finalization controlled record must be
				-- manipluated through the record itself. otherwise, the Adjust and Finalize
				-- calls goes incompatible with the reference counting implementation.
				-- It is because finalization is set on the record rather than the buffer pointer.
				--Tmp: Buffer_Pointer;
				--Tmp := new Buffer_Record(Get_Hard_Capacity(Obj));
				--Tmp.Slot := Obj.Buffer.Slot;
				--Tmp.Last := Obj.Buffer.Last;
				--Tmp.Refs := 1;
				--Unref_Buffer (Obj.Buffer);
				--Obj.Buffer := Tmp;
				Tmp := Obj;
				Obj := New_Buffer_Container(Get_Hard_Capacity(Obj));
				Obj.Buffer.Slot := Tmp.Buffer.Slot;
				Obj.Buffer.Last := Tmp.Buffer.Last;
			end if;
		end if;
	end Prepare_Buffer;

	-- prepare the buffer for writing
	procedure Prepare_Buffer (Obj: in out Elastic_Array; Req_Hard_Capa: in System_Size; Shift_Pos: in System_Size := 0; Shift_Size: in System_Size := 0; Shift_Dir: in Direction := DIRECTION_FORWARD) is
		Tmp: Elastic_Array;
		First, Last: System_Size;
		Hard_Capa: System_Size;
	begin
		First := Get_First_Index(Obj);
		Last := Get_Last_Index(Obj);

		if Obj.Buffer /= Empty_Buffer'Access and then Is_Shared(Obj) then
			if Req_Hard_Capa < Get_Hard_Capacity(Obj) then
				Hard_Capa := Get_Hard_Capacity(Obj);
			else
				Hard_Capa := Req_Hard_Capa;
			end if;

			Tmp := Obj;
			Obj := New_Buffer_Container(Hard_Capa);
			goto COPY_OVER;
		else
			if Req_Hard_Capa > Get_Hard_Capacity(Obj) then
				Tmp := Obj;
				Obj := New_Buffer_Container(Req_Hard_Capa);
				goto COPY_OVER;
			elsif Shift_Pos > 0 then
				Tmp := Obj;
				goto COPY_OVER_WITH_SHIFT;
			else
				-- no shift, no change in the buffer
				null;
			end if;
		end if;

		return;

	<<COPY_OVER>>
		if Shift_Pos <= 0 then
			-- no shift is required. copy the entire Array including th
			Obj.Buffer.Slot(First .. Last + Terminator_Length) := Tmp.Buffer.Slot(First .. Last + Terminator_Length);
			Obj.Buffer.Last := Last;
			return;
		end if;
	<<COPY_OVER_WITH_SHIFT>>
		-- it is an internal function. perform no sanity check.
		-- if Shift_Pos or Shift_Size is beyond the allocated capacity,
		-- it will end up in an exception.
		if Shift_Dir = DIRECTION_BACKWARD then
			declare
				Mid: constant System_Size := Shift_Pos - Shift_Size;
			begin
				Obj.Buffer.Slot(First .. Mid) := Tmp.Buffer.Slot(First .. Mid);
				Obj.Buffer.Slot(Mid + 1 .. Last - Shift_Size + Terminator_Length) := Tmp.Buffer.Slot(Shift_Pos + 1 .. Last + Terminator_Length);
				Obj.Buffer.Last := Last - Shift_Size;
			end;
		else
			Obj.Buffer.Slot(First .. Shift_Pos - 1) := Tmp.Buffer.Slot(First .. Shift_Pos - 1);
			Obj.Buffer.Slot(Shift_Pos + Shift_Size .. Last + Shift_Size + Terminator_Length) := Tmp.Buffer.Slot(Shift_Pos .. Last + Terminator_Length);
			Obj.Buffer.Last := Last + Shift_Size;
		end if;
	end Prepare_Buffer;

	procedure Clear (Obj: in out Elastic_Array) is
	begin
		Prepare_Buffer (Elastic_Array(Obj));
		Obj.Buffer.Last := Get_First_Index(Obj) - 1;
		Obj.Buffer.Slot(Get_First_Index(Obj) .. Get_First_Index(Obj) + Terminator_Length - 1) := (others => Terminator_Value);
	end Clear;

	procedure Purge (Obj: in out Elastic_Array) is
	begin
		Unref_Buffer (Obj.Buffer);
		Obj.Buffer := Empty_Buffer'Access;
	end Purge;

	function Calc_Inc_Capa (Obj: in Elastic_Array; Inc: in System_Size) return System_Size is
	begin
		return H3.Align(Get_Length(Obj) + Inc + Terminator_Length, BUFFER_ALIGN);
	end Calc_Inc_Capa;

	procedure Insert (Obj: in out Elastic_Array; Pos: in System_Index; V: in Item_Type; Repeat: in System_Size := 1) is
		Act_Pos: System_Index := Pos;
		Act_Inc: constant System_Size := Repeat;
	begin
		if Act_Pos > Obj.Buffer.Last then
			Act_Pos := Obj.Buffer.Last + 1;
		end if;

		Prepare_Buffer (Elastic_Array(Obj), Calc_Inc_Capa(Obj, Act_Inc), Act_Pos, Act_Inc);
		Obj.Buffer.Slot(Act_Pos .. Act_Pos + Act_Inc - 1) := (others => V);
	end Insert;

	procedure Insert (Obj: in out Elastic_Array; Pos: in System_Index; V: in Item_Array) is
		Act_Pos: System_Index := Pos;
	begin
		if Act_Pos > Obj.Buffer.Last then
			Act_Pos := Obj.Buffer.Last + 1;
		end if;

		Prepare_Buffer (Elastic_Array(Obj), Calc_Inc_Capa(Obj, V'Length), Act_Pos, V'Length);
		Obj.Buffer.Slot(Act_Pos .. Act_Pos + V'Length - 1) := V;
	end Insert;

-- TODO: operator "&" that returns a new Elastic_Array
	procedure Append (Obj: in out Elastic_Array; V: in Item_Type; Repeat: in System_Size := 1) is
	begin
		Insert (Obj, Get_Last_Index(Obj) + 1, V, Repeat);
	end Append;

	procedure Append (Obj: in out Elastic_Array; V: in Item_Array) is
	begin
		Insert (Obj, Get_Last_Index(Obj) + 1, V);
	end Append;

	procedure Prepend (Obj: in out Elastic_Array; V: in Item_Type; Repeat: in System_Size := 1) is
	begin
		Insert (Obj, Get_First_Index(Obj), V, Repeat);
	end Prepend;

	procedure Prepend (Obj: in out Elastic_Array; V: in Item_Array) is
	begin
		Insert (Obj, Get_First_Index(Obj), V);
	end Prepend;

	procedure Replace (Obj: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Type; Repeat: in System_Size := 1) is
		Act_To_Pos, Repl_Len: System_Size;
	begin
		if From_Pos <= To_Pos and then From_Pos <= Obj.Buffer.Last then
			Act_To_Pos := To_Pos;
			if Act_To_Pos > Obj.Buffer.Last then
				Act_To_Pos := Obj.Buffer.Last;
			end if;

			Repl_Len := Act_To_Pos - From_Pos + 1;
			if Repeat < Repl_Len then
				Prepare_Buffer (Elastic_Array(Obj), Get_Hard_Capacity(Obj), Act_To_Pos, Repl_Len - Repeat, DIRECTION_BACKWARD);
				Act_To_Pos := From_Pos + Repeat - 1;
			elsif Repeat > Repl_Len then
				Prepare_Buffer (Elastic_Array(Obj), Calc_Inc_Capa(Obj, Repeat - Repl_Len), From_Pos, Repeat - Repl_Len, DIRECTION_FORWARD);
				Act_To_Pos := From_Pos + Repeat - 1;
			else
				Prepare_Buffer (Elastic_Array(Obj));
			end if;
			Obj.Buffer.Slot(From_Pos .. Act_To_Pos) := (others => V);
		end if;
	end Replace;

	procedure Replace (Obj: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Array) is
		Act_To_Pos, Repl_Len: System_Size;
	begin
		if From_Pos <= To_Pos and then From_Pos <= Obj.Buffer.Last then
			Act_To_Pos := To_Pos;
			if Act_To_Pos > Obj.Buffer.Last then
				Act_To_Pos := Obj.Buffer.Last;
			end if;

			Repl_Len := Act_To_Pos - From_Pos + 1;
			if V'Length < Repl_Len then
				Prepare_Buffer (Elastic_Array(Obj), Get_Hard_Capacity(Obj), Act_To_Pos, Repl_Len - V'Length, DIRECTION_BACKWARD);
				Act_To_Pos := From_Pos + V'Length - 1;
			elsif V'Length > Repl_Len then
				Prepare_Buffer (Elastic_Array(Obj), Calc_Inc_Capa(Obj, V'Length - Repl_Len), From_Pos, V'Length - Repl_Len, DIRECTION_FORWARD);
				Act_To_Pos := From_Pos + V'Length - 1;
			else
				Prepare_Buffer (Elastic_Array(Obj));
			end if;
			Obj.Buffer.Slot(From_Pos .. Act_To_Pos) := V;
		end if;
	end Replace;

	procedure Delete (Obj: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size) is
		Act_To_Pos: System_Size;
	begin
		if From_Pos <= To_Pos and then From_Pos <= Obj.Buffer.Last then
			Act_To_Pos := To_Pos;
			if Act_To_Pos > Obj.Buffer.Last then
				Act_To_Pos := Obj.Buffer.Last;
			end if;
			Prepare_Buffer (Elastic_Array(Obj), Get_Hard_Capacity(Obj), Act_To_Pos, Act_To_Pos - From_Pos + 1, DIRECTION_BACKWARD);
		end if;
	end Delete;

	function Find (Obj: in Elastic_Array; V: in Item_Type; Start_Pos: in System_Index; Find_Dir: in Direction := DIRECTION_FORWARD) return System_Size is
		Act_Start_Pos: System_Index := Start_Pos;
	begin
		if Find_Dir = DIRECTION_FORWARD then
			if Act_Start_Pos < Get_First_Index(Obj) then
				Act_Start_Pos := Get_First_Index(Obj);
			end if;
			for i in Act_Start_Pos .. Get_Last_Index(Obj) loop
				if Get_Item(Obj, i) = V then
					return i;
				end if;
			end loop;
		else
			if Act_Start_Pos > Get_Last_Index(Obj) then
				Act_Start_Pos := Get_Last_Index(Obj);
			end if;
			for i in reverse Get_First_Index(Obj) .. Act_Start_Pos loop
				if Get_Item(Obj, i) = V then
					return i;
				end if;
			end loop;
		end if;

		return System_Size'First;
	end Find;

	function Find (Obj: in Elastic_Array; V: in Item_Array; Start_Pos: in System_Index; Find_Dir: in Direction := DIRECTION_FORWARD) return System_Size is
		End_Pos: System_Size;
	begin
		if Get_Length(Obj) > 0 and then V'Length > 0 and then V'Length <= Get_Length(Obj) then
			End_Pos := Get_Last_Index(Obj) - V'Length + 1;

			if Find_Dir = DIRECTION_FORWARD then
				for i in Start_Pos .. End_Pos loop
					if Obj.Buffer.Slot(i .. i + V'Length - 1) = V then
						return i;
					end if;
				end loop;
			else
				if Start_Pos < End_Pos then
					End_Pos := Start_Pos;
				end if;
				for i in reverse Get_First_Index(Obj) .. End_Pos loop
					if Obj.Buffer.Slot(i .. i + V'Length - 1) = V then
						return i;
					end if;
				end loop;
			end if;
		end if;
		return System_Size'First;
	end Find;

	function Equals (Obj: in Elastic_Array; Obj2: in Elastic_Array) return Boolean is
	begin
		return Obj.Buffer = Obj2.Buffer or else Obj.Buffer.Slot(Get_First_Index(Obj) .. Get_Last_Index(Obj)) = Obj2.Buffer.Slot(Get_First_Index(Obj2) .. Get_Last_Index(Obj2));
	end Equals;

	function Equals (Obj: in Elastic_Array; Obj2: in Item_Array) return Boolean is
	begin
		return Obj.Buffer.Slot(Get_First_Index(Obj) .. Get_Last_Index(Obj)) = Obj2;
	end Equals;

	-- ---------------------------------------------------------------------
	-- Controlled Management
	-- ---------------------------------------------------------------------
	procedure Initialize (Obj: in out Elastic_Array) is
	begin
		-- the Array is initialized to the empty buffer all the time.
		-- there is no need to reference the buffer.
		null;
	end Initialize;

	procedure Adjust (Obj: in out Elastic_Array) is
	begin
		Ref_Buffer (Obj.Buffer);
	end Adjust;

	procedure Finalize (Obj: in out Elastic_Array) is
	begin
		Unref_Buffer (Obj.Buffer);
	end Finalize;

end H3.Arrays;
