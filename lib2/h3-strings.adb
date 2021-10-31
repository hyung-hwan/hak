package body H3.Strings is

	procedure Append (Obj: in out Elastic_String; V: in Rune_Array) is
	begin
		P.Append (P.Elastic_Array(Obj), V);
	end Append;

	function Get_Rune (Obj: in Elastic_String; Pos: in System_Index) return Rune is
	begin
		return P.Get_Item(P.Elastic_Array(Obj), Pos);
	end Get_Rune;

	function To_Rune_Array (Obj: in Elastic_String) return Rune_Array is
	begin
		return P.To_Item_Array(P.Elastic_Array(Obj));
	end To_Rune_Array;


end H3.Strings;
