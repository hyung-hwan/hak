with H3.Arrays;

generic
	type Rune_Type is (<>);
package H3.Strings is

	package P is new H3.Arrays(Rune_Type, 1, Rune_Type'First);

	subtype Rune is P.Item;
	subtype Rune_Array is P.Item_Array;
	subtype Thin_Rune_Array_Pointer is P.Thin_Item_Array_Pointer;

	Terminator_Length: System_Zero_Or_One renames P.Terminator_Length;
	Terminator_Value: Rune renames P.Terminator_Value;

	type Elastic_String is new P.Elastic_Array with record
		--A: standard.integer := 999;
		null;
	end record;

	function Get_Rune (Obj: in Elastic_String; Pos: in System_Index) return Rune;
	function To_Rune_Array (Obj: in Elastic_String) return Rune_Array;

	overriding procedure Append (Obj: in out Elastic_String; V: in Rune_Array);

end H3.Strings;
