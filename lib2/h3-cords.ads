with H3.Arrays;

generic
	type Item_Type is private; -- any limited definite type
	G_Terminator_Length: System_Zero_Or_One := 0;
	G_Terminator_Value: Item_Type;
package H3.Strings is

	package P is new H3.Arrays(Item_Type, G_Terminator_Length);

end H3.Cords;
