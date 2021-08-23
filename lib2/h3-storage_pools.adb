with System;
with System.Address_Image;

with Ada.Text_IO;

package body H3.Storage_Pools is

	function Sys_Malloc (Size: System_Size) return System.Address;
	pragma Import (Convention => C, Entity => Sys_Malloc, External_Name => "malloc");

	procedure Sys_Free (Ptr: System.Address);
	pragma Import (Convention => C, Entity => Sys_Free, External_Name => "free");

	procedure Allocate (Pool:      in out Global_Pool;
	                    Address :  out    System.Address;
	                    Size:      in     SSE.Storage_Count;
	                    Alignment: in     SSE.Storage_Count) is
		tmp: System.Address;
		use type SSE.Storage_Count;
	begin
		tmp := Sys_Malloc(System_Size(((Size + Alignment - 1) / Alignment) * Alignment));
		if System."="(tmp, System.Null_Address) then
			raise Storage_Error;
		else
			Address := tmp;
Ada.Text_IO.Put_Line ("Global_Pool Allocating Size: " & SSE.Storage_Count'Image (Size) & " Actual-Size: " & SSE.Storage_Count'Image (((Size + Alignment - 1) / Alignment) * Alignment) & "Address: " & System.Address_Image(Address));
		end if;
	end Allocate;

	procedure Deallocate (Pool:      in out Global_Pool;
	                      Address :  in     System.Address;
	                      Size:      in     SSE.Storage_Count;
	                      Alignment: in     SSE.Storage_Count) is

	begin
--Ada.Text_IO.Put_Line ("Global_Pool Deallocating Address: " & System.Address'Img);
Ada.Text_IO.Put_Line ("Global_Pool Deallocating " & System.Address_Image(Address));
		Sys_Free (Address);
	end Deallocate;

	function Storage_Size (Pool: in Global_Pool) return SSE.Storage_Count is
	begin
Ada.Text_IO.Put_Line ("Global_Pool Storage_Size ");
		return SSE.Storage_Count'Last;
	end Storage_Size;



	-- TODO: find a better solution
	-- gnat 3.15p somehow looks for the rountines below when H3.Pool is used.
	-- let me put these routines here temporarily until i find a proper solution.
	procedure Allocate_315P (Pool:      in out SSP.Root_Storage_Pool'Class;
	                         Address :  out    System.Address;
	                         Size:      in     SSE.Storage_Count;
	                         Alignment: in     SSE.Storage_Count);
	pragma Export (Ada, Allocate_315P, "system__storage_pools__allocate");
	procedure Allocate_315P (Pool:      in out SSP.Root_Storage_Pool'Class;
	                         Address :  out    System.Address;
	                         Size:      in     SSE.Storage_Count;
	                         Alignment: in     SSE.Storage_Count) is
	begin
ada.text_io.put_line ("system__storage_pools__allocate...");
		SSP.Allocate (Pool, Address, Size, Alignment);
	end Allocate_315P;

	procedure Deallocate_315P (Pool: in out SSP.Root_Storage_Pool'Class; Address : in System.Address; Size: in SSE.Storage_Count; Alignment: in SSE.Storage_Count);
	pragma Export (Ada, Deallocate_315P, "system__storage_pools__deallocate");
	procedure Deallocate_315P (Pool: in out SSP.Root_Storage_Pool'Class; Address : in System.Address; Size: in SSE.Storage_Count; Alignment: in SSE.Storage_Count) is
	begin
ada.text_io.put_line ("system__storage_pools__deallocate...");
		SSP.Deallocate (Pool, Address, Size, Alignment);
	end Deallocate_315P;

end H3.Storage_Pools;

