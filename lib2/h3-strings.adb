package body H3.Strings is

	procedure Append (Obj: in out Elastic_String; V: in Rune_Array) is
	begin
		P.Append (P.Elastic_Array(Obj), V);
	end;

end H3.Strings;
