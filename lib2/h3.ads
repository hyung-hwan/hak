with System;
with System.Storage_Pools;
with System.Atomic_Counters;
with Ada.Finalization;

package H3 is
	--pragma Preelaborate (H2);
	System_Byte_Bits: constant := System.Storage_Unit;
	System_Word_Bits: constant := System.Word_Size;
	System_Word_Bytes: constant := System_Word_Bits / System_Byte_Bits;

	type System_Byte is mod 2 ** System_Byte_Bits;
	for System_Byte'Size use System_Byte_Bits;

	type System_Word is mod 2 ** System_Word_Bits;
	for System_Word'Size use System_Word_Bits;

	type System_Signed_Word is range -(2 ** (System_Word_Bits - 1)) ..
	                                 +(2 ** (System_Word_Bits - 1)) - 1;
	for System_Signed_Word'Size use System_Word_Bits;

	--type System_Size is new System_Word range 0 .. (2 ** System_Word_Bits) - 1;
	subtype System_Size is System_Word range 0 .. (2 ** System_Word_Bits) - 1;
	subtype System_Length is System_Size;

	--subtype System_Index is System_Size range 0 .. (System_Size'Last - 1);
	subtype System_Index is System_Size range 1 .. System_Size'Last;

	type Storage_Pool_Pointer is access all System.Storage_Pools.Root_Storage_Pool'Class;

	type System_Byte_Array is array(System_Index range<>) of System_Byte;

	-- ---------------------------------------------------------------------
	-- Utilities Functions
	-- ---------------------------------------------------------------------
	function Align (X: in System_Size; Y: in System_Size) return System_Size;
	pragma Inline(Align);

	-- ---------------------------------------------------------------------
	-- Reference Counting
	-- ---------------------------------------------------------------------
--	type Ref_Counted is abstract tagged record
--		--Ref_Count: System.Atomic_Counters.Atomic_Counter;
--		Ref_Count: System_Size;
--	end record;

--	type Ref_Counted_Pointer is access all Ref_Counted'Class;
--	type Ref is new Ada.Finalization.Controlled with record
--		Data: Ref_Counted_Pointer;
--	end record;

--	procedure Set (R: in out Ref; Data: in Ref_Counted_Pointer);
--	function Get (R: in Ref) return Ref_Counted_Pointer;
--	overriding procedure Adjust (R: in out Ref);
--	overriding procedure Finalize (R: in out Ref);
end H3;
