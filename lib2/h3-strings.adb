with Ada.Unchecked_Deallocation;

package body H3.Strings is
	BUFFER_ALIGN: constant := 16;

	function To_Character_Array (Str: in Elastic_String) return Character_Array is
	begin
		return Str.Buffer.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last);
	end To_Character_Array;

	-- return the buffer capacity excluding the terminator
	function Get_Capacity (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Slot'Length - 1;
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

	function Get_Item (Str: in Elastic_String; Pos: in System_Index) return Character_Type is
	begin
		return Str.Buffer.Slot(Pos);
	end Get_Item;

	-- unsafe as it exposes the internal buffer which can go away.
	-- assume the system address is equal to the thin pointer in size.
	function Get_Slot_Pointer (Str: in Elastic_String) return Thin_Character_Array_Pointer is
		A: System.Address := Str.Buffer.Slot(Str.Buffer.Slot'First)'Address;
		P: Thin_Character_Array_Pointer;
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
	procedure Prepare_Buffer (Str: in out Elastic_String; Req_Hard_Capa: in System_Size) is
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

			Tmp := New_Buffer_Container(Hard_Capa);
			Tmp.Buffer.Slot(First .. Last + 1) := Str.Buffer.Slot(First .. Last + 1);
			Tmp.Buffer.Last := Last;

			Str := Tmp;
		else
			if Req_Hard_Capa > Get_Hard_Capacity(Str) then
				Tmp := Str;
				Str := New_Buffer_Container(Req_Hard_Capa);
				Str.Buffer.Slot(First .. Last + 1) := Tmp.Buffer.Slot(First .. Last + 1);
				Str.Buffer.Last := Last;
			end if;
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

-- TODO: operator "&"
	procedure Append (Str: in out Elastic_String; V: in Character_Array) is
	begin
		if V'Length > 0 then	
			Prepare_Buffer (Str, H3.Align(Get_Length(Str) + V'Length + 1, BUFFER_ALIGN));
			Str.Buffer.Slot(Str.Buffer.Last + 1 .. Str.Buffer.Last + V'Length) := V;
			Str.Buffer.Last := Str.Buffer.Last + V'Length;
			Str.Buffer.Slot(Str.Buffer.Last + 1) := Null_Character;
		end if;
	end Append;

	procedure Append (Str: in out Elastic_String; V: in Character_Type) is
		Tmp: Character_Array(1 .. 1) := (1 => V);
	begin
		Append (Str, Tmp);
	end Append;

	procedure Delete (Str: in out Elastic_String; Pos: in System_Index; Length: in System_Size) is
	begin
		null;
	end Delete;

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
