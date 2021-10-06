package body H3.Strings is

	procedure Append (Str: in out Elastic_String; V: in Character_Array) is
	begin
		P.Append (P.Elastic_Array(Str), V);
	end;

end H3.Strings;
