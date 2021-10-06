with Ada.Unchecked_Deallocation;

package body H3.MM is
	procedure Create (R: in out Ref_Counted) is
	begin
		Finalize (R);
		R.Data := new Ref_Counted_Record;
		R.Data.Refs := 1;
		--System.Atomic_Counters.Initialize (R.Data.Ref_Count); -- initialize to 1
	end Create;
 
	procedure Create (R: in out Ref_Counted; V: in Item_Type) is
	begin
		Create (R);
		R.Data.Item := V;
	end Create;

	function Get_Item_Pointer (R: in Ref_Counted) return Item_Pointer is
	begin
		if R.Data /= null then
			return R.Data.Item'Access;
		else
			return null;
		end if;
	end Get_Item_Pointer;

	function Is_Shared (R: in Ref_Counted) return Standard.Boolean is
	begin
		--return R.Data /= null and then not System.Atomic_Counters.Is_One(R.Data.Refs);
		return R.Data /= null and then R.Data.Refs > 1;
	end Is_Shared;

	procedure Initialize (R: in out Ref_Counted) is
	begin
		R.Data := null;
	end Initialize;

	procedure Adjust (R: in out Ref_Counted) is
	begin
		if R.Data /= null then
			R.Data.Refs := R.Data.Refs + 1;
			--System.Atomic_Counters.Increment (R.Data.Refs);
		end if;
	end Adjust;

	procedure Finalize (R: in out Ref_Counted) is
		procedure Dealloc is new Ada.Unchecked_Deallocation(Ref_Counted_Record, Ref_Counted_Pointer);
	begin
		if R.Data /= null then	
			--if System.Atomic_Counters.Decrement(R.Data.Ref_Count) then
			--	-- The reference count reached 0
			--	Dealloc (R.Data); 
			--	-- R.DAta must be null here
			--end if;
			if R.Data.Refs = 1 then
				Dealloc (R.Data); 
			else
				R.Data.Refs := R.Data.Refs - 1;
			end if;
		end if;
	end Finalize;
end H3.MM;
