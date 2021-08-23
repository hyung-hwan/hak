with Ada.Unchecked_Deallocation;

with ada.text_io;

package body H3.Strings is
	BUFFER_ALIGN: constant := 16;

	function To_Character_Array (Str: in Elastic_String) return Character_Array is
	begin
		return Str.Buffer.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last);
	end To_Character_Array;

	function Get_Capacity (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Slot'Length - 1;
	end Get_Capacity;

	function Get_Length (Str: in Elastic_String) return System_Size is
	begin
		return Str.Buffer.Last - Str.Buffer.Slot'First + 1;
	end Get_Length;

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
ada.text_io.put_line ("ref_buffer -> " & Buf.Refs'Img);
			Buf.Refs := Buf.Refs + 1;
		end if;
	end Ref_Buffer;

	procedure Deref_Buffer (Buf: in out Buffer_Pointer) is
	begin
		if Buf /= Empty_Buffer'Access then
ada.text_io.put_line ("deref_buffer -> " & Buf.Refs'Img);
			if Buf.Refs = 1 then
				declare
					procedure Free is new Ada.Unchecked_Deallocation(Buffer_Record, Buffer_Pointer);
				begin
					Free (Buf);
				end;
			else
				Buf.Refs := Buf.Refs - 1;
			end if;
		end if;
	end Deref_Buffer;

	procedure Prepare_Buffer (Str: in out Elastic_String) is
		Tmp: Buffer_Pointer;
	begin
		if Str.Buffer /= Empty_Buffer'Access then
			if Is_Shared(Str) then
				Tmp := new Buffer_Record(Str.Buffer.Slot'Length);
				Tmp.Slot := Str.Buffer.Slot;
				Tmp.Last := Str.Buffer.Last;
				Tmp.Refs := 1;	--Ref_Buffer (Tmp);
				Deref_Buffer (Str.Buffer);
				Str.Buffer := Tmp;
			end if;
		end if;
	end Prepare_Buffer;

	procedure Prepare_Buffer (Str: in out Elastic_String; ReqCapa: in System_Size) is
		Tmp: Buffer_Pointer;
	begin
		if Str.Buffer /= Empty_Buffer'Access then
			if Is_Shared(Str) then
				-- ReqCapa must be greater than Str.Buffer.Slot'Length
				Tmp := new Buffer_Record(ReqCapa);
				Tmp.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last + 1) := Str.Buffer.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last + 1);
				Tmp.Last := Str.Buffer.Last;
				Tmp.Refs := 1; --Ref_Buffer (Tmp);
				Deref_Buffer (Str.Buffer);
				Str.Buffer := Tmp;
			end if;
		end if;
	end Prepare_Buffer;

	procedure Clear (Str: in out Elastic_String) is
	begin
		Prepare_Buffer (Str);
		Str.Buffer.Last := Str.Buffer.Slot'First - 1;
	end Clear;

	procedure Purge (Str: in out Elastic_String) is
	begin 
		Deref_Buffer (Str.Buffer);
		Str.Buffer := Empty_Buffer'Access;
	end Purge;

-- TODO: operator "&"
	procedure Append (Str: in out Elastic_String; V: in Character_Array) is
		ReqCapa: System_Size;
		Tmp: Buffer_Pointer;
	begin
		if V'Length > 0 then
			ReqCapa := H3.Align(Str.Buffer.Last + V'Length + 1, BUFFER_ALIGN);
			Prepare_Buffer (Str, ReqCapa);

			if ReqCapa > Get_Capacity(Str) then
				Tmp := new Buffer_Record(ReqCapa);
				Tmp.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last) := Str.Buffer.Slot(Str.Buffer.Slot'First .. Str.Buffer.Last);
				Tmp.Last := Str.Buffer.Last;
				Free_Buffer (Str);
				Str.Buffer := Tmp;
			end if;

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

	procedure Delete (Str: in out Elastic_String; Pos: in System_Index; Length: in System_Length) is
	begin
		null;
	end Delete;



	-- ---------------------------------------------------------------------
	-- Controlled Management
	-- ---------------------------------------------------------------------
	procedure Initialize (Str: in out Elastic_String) is
	begin
ada.text_io.put_line("ES Initialize");
		null;
	end Initialize;

	procedure Adjust (Str: in out Elastic_String) is
	begin
ada.text_io.put_line("ES Adhust");
		Ref_Buffer (Str.Buffer);
	end Adjust;

	procedure Finalize (Str: in out Elastic_String) is
	begin
ada.text_io.put_line("ES Finalize");
		Deref_Buffer (Str.Buffer);
	end Finalize;
end H3.Strings;
