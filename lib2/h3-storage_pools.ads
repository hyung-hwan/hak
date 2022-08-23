with System.Storage_Pools;
with System.Storage_Elements;

package H3.Storage_Pools is

	package SSE renames System.Storage_Elements;
	package SSP renames System.Storage_Pools;

	type Global_Pool is new SSP.Root_Storage_Pool with private;

	procedure Allocate (Pool:      in out Global_Pool;
	                    Address:   out    System.Address;
	                    Size:      in     SSE.Storage_Count;
	                    Alignment: in     SSE.Storage_Count);

	procedure Deallocate (Pool:      in out Global_Pool;
	                      Address:   in     System.Address;
	                      Size:      in     SSE.Storage_Count;
	                      Alignment: in     SSE.Storage_Count);

	function Storage_Size (Pool: in Global_Pool) return SSE.Storage_Count;

	-- TODO: implement more pools

private
	type Global_Pool is new SSP.Root_Storage_Pool with null record;

end H3.Storage_Pools;
