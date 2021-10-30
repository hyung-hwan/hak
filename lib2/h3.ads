with System;
with System.Storage_Pools;

package H3 is
	--pragma Preelaborate (H3);

	subtype Boolean is Standard.Boolean;
	subtype Natural is Standard.Natural;

	subtype System_Rune is Standard.Wide_Character;

	System_Byte_Bits: constant := System.Storage_Unit;
	System_Word_Bits: constant := System.Word_Size;
	System_Word_Bytes: constant := System_Word_Bits / System_Byte_Bits;

	type System_Byte is mod 2 ** System_Byte_Bits;
	for System_Byte'Size use System_Byte_Bits;

	type System_Word is mod 2 ** System_Word_Bits;
	--type System_Word is range 0 .. (2 ** System_Word_Bits) - 1;
	for System_Word'Size use System_Word_Bits;

	type System_Signed_Word is range -(2 ** (System_Word_Bits - 1)) ..
	                                 +(2 ** (System_Word_Bits - 1)) - 1;
	for System_Signed_Word'Size use System_Word_Bits;

	--type System_Size is new System_Word range 0 .. (2 ** System_Word_Bits) - 1;
	subtype System_Size is System_Word range 0 .. (2 ** System_Word_Bits) - 1;

	--subtype System_Index is System_Size range 0 .. (System_Size'Last - 1);
	subtype System_Index is System_Size range (System_Size'First + 1) .. System_Size'Last;

	subtype System_Zero_Or_One is System_Word range 0 .. 1;

	type Storage_Pool_Pointer is access all System.Storage_Pools.Root_Storage_Pool'Class;

	type System_Byte_Array is array(System_Index range<>) of System_Byte;

	-- ---------------------------------------------------------------------
	-- Utilities Functions
	-- ---------------------------------------------------------------------
	function Align (X: in System_Size; Y: in System_Size) return System_Size;
	pragma Inline(Align);

	Index_Error: exception;
end H3;
