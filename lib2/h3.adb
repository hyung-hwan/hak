
package body H3 is

	function Align (X: in System_Size; Y: in System_Size) return System_Size is
	begin
		return ((X + Y - 1) / Y) * Y;
	end Align;



--	procedure Set (R: in out Ref; Data: in Ref_Counted_Pointer) is
--	begin
--		if R.Data /= null then
--			Finalize (R);
--		end if;
--
--		R.Data := Data;
--		Adjust (R);
--	end Set;

--	function Get (R: in Ref) return Ref_Counted_Pointer is
--	begin
--		return R.Data;
--	end Get;

--	procedure Adjust (R: in out Ref) is
--	begin
--		if R.Data /= null then
--			R.Data.Ref_Count := R.Data.Ref_Count + 1;
--		end if;
--	end Adjust;

--	procedure Finalize (R: in out Ref) is
--	begin
--		if R.Data /= null then
--			R.Data.Ref_Count  := R.Data.Ref_Count - 1;
--			if R.Data.Ref_Count = 0 then
--				null;
--			end if;
--			R.Data := null;
--		end if;
--	end Finalize;
end H3;