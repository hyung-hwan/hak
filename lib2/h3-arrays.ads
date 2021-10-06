with Ada.Finalization;

generic
	--type Item_Type is private;
	type Item_Type is (<>);
	G_Terminator_Length: System_Zero_Or_One;
	G_Terminator_Value: Item_Type;
package H3.Arrays is

	Terminator_Length: constant System_Zero_Or_One := G_Terminator_Length;
	Terminator_Value: constant Item_Type := G_Terminator_Value;

	type Elastic_Array is tagged private;
	type Item_Array is array(System_Index range <>) of Item_Type;
	--type Item_Array_Pointer is access all Item_Array;

	subtype Thin_Item_Array is Item_Array(System_Index'Range);
	type Thin_Item_Array_Pointer is access Thin_Item_Array;

	function To_Item_Array (Str: in Elastic_Array) return Item_Array;

	function Get_Capacity (Str: in Elastic_Array) return System_Size;
	pragma Inline (Get_Capacity);

	function Get_Length (Str: in Elastic_Array) return System_Size;
	pragma Inline (Get_Length);

	-- the return type is System_Size for consistency with Get_Last_Index.
	function Get_First_Index (Str: in Elastic_Array) return System_Size;
	pragma Inline (Get_First_Index);

	-- the return type is System_Size because the Last index is -1 off the System_Index'First for an empty array
	function Get_Last_Index (Str: in Elastic_Array) return System_Size;
	pragma Inline (Get_Last_index);

	function Get_Item (Str: in Elastic_Array; Pos: in System_Index) return Item_Type;
	pragma Inline (Get_Item);

	-- unsafe
	function Get_Slot_Pointer (Str: in Elastic_Array) return Thin_Item_Array_Pointer;
	pragma Inline (Get_Slot_Pointer);

	function Is_Shared(Str: in Elastic_Array) return Standard.Boolean;
	pragma Inline (Is_Shared);

	procedure Clear (Str: in out Elastic_Array);
	procedure Purge (Str: in out Elastic_Array); -- clear and reset the buffer to Empty_Buffer.

	procedure Insert (Str: in out Elastic_Array; Pos: in System_Index; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Insert (Str: in out Elastic_Array; Pos: in System_Index; V: in Item_Array);

	procedure Append (Str: in out Elastic_Array; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Append (Str: in out Elastic_Array; V: in Item_Array);

	procedure Prepend (Str: in out Elastic_Array; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Prepend (Str: in out Elastic_Array; V: in Item_Array);
	
	procedure Replace (Str: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Type; Repeat: in System_Size := 1);
	procedure Replace (Str: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size; V: in Item_Array);

	procedure Delete (Str: in out Elastic_Array; From_Pos: in System_Index; To_Pos: in System_Size);

	function "=" (Str: in Elastic_Array; Str2: in Elastic_Array) return Standard.Boolean;
	function "=" (Str: in Elastic_Array; Str2: in Item_Array) return Standard.Boolean;

private
	type Buffer_Record(Capa: System_Size) is limited record
		Refs: System_Size := 1;
		Slot: Item_Array(1 .. Capa) := (others => Terminator_Value);
		Last: System_Size := 0;
	end record;

	type Buffer_Pointer is access all Buffer_Record;

	--Empty_Buffer: aliased Buffer_Record(1);
	-- Use 1 slot to hold the terminator value regardless of th terminator length in Empty_Buffer.
	Empty_Buffer: aliased Buffer_Record := (Capa => 1, Refs => 0, Slot => (1 => Terminator_Value), Last => 0);

	type Elastic_Array is new Ada.Finalization.Controlled with record
		Buffer: Buffer_Pointer := Empty_Buffer'Access;
	end record;

	overriding procedure Initialize (Str: in out Elastic_Array);
	overriding procedure Adjust     (Str: in out Elastic_Array);
	overriding procedure Finalize   (Str: in out Elastic_Array);

end H3.Arrays;
