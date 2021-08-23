with Ada.Finalization;

generic
	--type Character_Type is private;
	type Character_Type is (<>);
	Null_Character: Character_Type;
package H3.Strings is

	type Elastic_String is private;
	type Character_Array is array(System_Index range <>) of Character_Type;
	--type Character_Array_Pointer is access all Character_Array;

	subtype Thin_Character_Array is Character_Array(System_Index'Range);
	type Thin_Character_Array_Pointer is access Thin_Character_Array;

	function To_Character_Array (Str: in Elastic_String) return Character_Array;

	function Get_Capacity (Str: in Elastic_String) return System_Size;
	pragma Inline (Get_Capacity);

	function Get_Length (Str: in Elastic_String) return System_Size;
	pragma Inline (Get_Length);

	function Get_Item (Str: in Elastic_String; Pos: in System_Index) return Character_Type;
	pragma Inline (Get_Item);

	function Get_Slot_Pointer (Str: in Elastic_String) return Thin_Character_Array_Pointer;
	pragma Inline (Get_Slot_Pointer);

	function Is_Shared(Str: in Elastic_String) return Standard.Boolean;

	procedure Clear (Str: in out Elastic_String);
	procedure Purge (Str: in out Elastic_String);

	procedure Append (Str: in out Elastic_String; V: in Character_Array);
	procedure Append (Str: in out Elastic_String; V: in Character_Type);

private
	
	type Buffer_Record(Size: System_Size) is limited record
		Refs: System_Size := 0;
		Slot: Character_Array(1 .. Size);
		Last: System_Size := 0;
	end record;

	type Buffer_Pointer is access all Buffer_Record;

	--Empty_Buffer: aliased Buffer_Record(1);
	Empty_Buffer: aliased Buffer_Record := (Size => 1, Refs => 0, Slot => (1 => Null_Character), Last => 0);

	type Elastic_String is new Ada.Finalization.Controlled with record
		Buffer: Buffer_Pointer := Empty_Buffer'Access;
	end record;

	overriding procedure Initialize (Str: in out Elastic_String);
	overriding procedure Adjust     (Str: in out Elastic_String);
	overriding procedure Finalize   (Str: in out Elastic_String);



end H3.Strings;
