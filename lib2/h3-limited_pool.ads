--------------------------------------------------------------------
-- Instantantiate this package before using. To allocate integers,
--
--   type Integer_Pointer is access Integer_Pointer;
--   package Integer_Pool is new Pool(Integer, Integer_Pointer, Storage_Pool'Unchecked_Access);
--   x: Integer_Pointer;
--
--   x := Integer_Pool.Allocate(10);
--------------------------------------------------------------------

generic
	type Normal_Type is limited private;
	type Pointer_Type is access Normal_Type;
	Storage_Pool: in Storage_Pool_Pointer := null;

package H3.Limited_Pool is
	--pragma Preelaborate (Pool);

	function Allocate (Pool: in Storage_Pool_Pointer := null) return Pointer_Type;

	procedure Deallocate (Target: in out Pointer_Type;
	                      Pool:   in     Storage_Pool_Pointer := null);

end H3.Limited_Pool;
