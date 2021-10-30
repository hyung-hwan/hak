with Ada.Finalization;

generic
	--type Item_Type is private;
	type Item_Type is (<>);
	G_Terminator_Length: System_Zero_Or_One;
	G_Terminator_Value: Item_Type;
package H3.Arrays is

	subtype Item is Item_Type;

	Terminator_Length: constant System_Zero_Or_One := G_Terminator_Length;
	Terminator_Value: constant Item_Type := G_Terminator_Value;

	type Direction is (DIRECTION_BACKWARD, DIRECTION_FORWARD);

	type Elastic_Array is tagged private;
	--type Item_Array is array(System_Index range <>) of aliased Item_Type;
	type Item_Array is array(System_Index range <>) of Item_Type;
	--type Item_Array_Pointer is access all Item_Array;

	subtype Thin_Item_Array is Item_Array(System_Index'Range);
	type Thin_Item_Array_Pointer is access Thin_Item_Array;

	function To_Item_Array (Obj: in Elastic_Array) return Item_Array;

	function Get_Capacity (Obj: in Elastic_Array) return System_Size;
	pragma Inline (Get_Capacity);

	function Get_Length (Obj: in Elastic_Array) return System_Size;
	pragma Inline (Get_Length);

	-- the return type is System_Size for consistency with Get_Last_Index.
	function Get_First_Index (Obj: in Elastic_Array) return System_Size;
	pragma Inline (Get_First_Index);

	-- the return type is System_Size because the Last index is -1 off the System_Index'First for an empty array
	function Get_Last_Index (Obj: in Elastic_Array) return System_Size;
	pragma Inline (Get_Last_index);

	function Get_Item (Obj: in Elastic_Array; Pos: in System_Index) return Item_Type;
	pragma Inline (Get_Item);

	-- unsafe
	function Get_Slot_Pointer (Obj: in Elastic_Array) return Thin_Item_Array_Pointer;
	pragma Inline (Get_Slot_Pointer);

	function Is_Shared(Obj: in Elastic_Array) return Boolean;
	pragma Inline (Is_Shared);

	procedure Clear (Obj: in out Elastic_Array);
	procedure Purge (Obj: in out Elastic_Array); -- clear and reset the buffer to Empty_Buffer.

	procedure Insert (Obj: in out Elastic_Array; Pos: in System_Index; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Insert (Obj: in out Elastic_Array; Pos: in System_Index; V: in Item_Array);

	procedure Append (Obj: in out Elastic_Array; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Append (Obj: in out Elastic_Array; V: in Item_Array);

	procedure Prepend (Obj: in out Elastic_Array; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Prepend (Obj: in out Elastic_Array; V: in Item_Array);

	procedure Replace (Obj: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Replace (Obj: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Array);

	procedure Delete (Obj: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size);

	function Find (Obj: in Elastic_Array; V: in Item_Type; Start_Pos: in System_Index; Find_Dir: in Direction := DIRECTION_FORWARD) return System_Size;
	function Find (Obj: in Elastic_Array; V: in Item_Array; Start_Pos: in System_Index; Find_Dir: in Direction := DIRECTION_FORWARD) return System_Size;

	function "=" (Obj: in Elastic_Array; Obj2: in Elastic_Array) return Boolean;
	function "=" (Obj: in Elastic_Array; Obj2: in Item_Array) return Boolean;

private
	type Buffer_Record(Capa: System_Size) is limited record
		Refs: System_Size := 1;
		Slot: Item_Array(1 .. Capa) := (others => Terminator_Value);
		Last: System_Size := 0;
	end record;

	type Buffer_Pointer is access all Buffer_Record;
	--for Buffer_Pointer'Storage_Pool use <<TODO: custom storage pool?>> H3'Storage_Pool;

	--Empty_Buffer: aliased Buffer_Record(1);
	-- Use 1 slot to hold the terminator value regardless of th terminator length in Empty_Buffer.
	Empty_Buffer: aliased Buffer_Record := (Capa => 1, Refs => 0, Slot => (1 => Terminator_Value), Last => 0);

	type Elastic_Array is new Ada.Finalization.Controlled with record
		Buffer: Buffer_Pointer := Empty_Buffer'Access;
	end record;

	overriding procedure Initialize (Obj: in out Elastic_Array);
	overriding procedure Adjust     (Obj: in out Elastic_Array);
	overriding procedure Finalize   (Obj: in out Elastic_Array);

end H3.Arrays;
