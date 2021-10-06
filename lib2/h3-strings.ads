with H3.Arrays;

generic
	type Item_Type is (<>);
	G_Terminator_Value: Item_Type;
package H3.Strings is

	package P is new H3.Arrays(Item_Type, 1, G_Terminator_Value);

	Terminator_Length: System_Zero_Or_One renames P.Terminator_Length;
	Terminator_Value: Item_Type renames P.Terminator_Value;

	subtype Character_Array is P.Item_Array;
	subtype Thin_Character_Array_Pointer is P.Thin_Item_Array_Pointer;

	type Elastic_String is new P.Elastic_Array with record
		--A: standard.integer := 999;
		null;
	end record;

	overriding procedure Append (Obj: in out Elastic_String; V: in Character_Array);

end H3.Strings;
