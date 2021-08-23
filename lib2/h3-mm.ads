with Ada.Finalization;

generic
	type Item_Type is private;
--	type Pointer_Type is access Item_Type;
package H3.MM is
	type Item_Pointer is access all Item_Type;

	type Ref_Counted_Record is record
		Ref_Count: System.Atomic_Counters.Atomic_Counter;
		Item: aliased Item_Type;
	end record;

	type Ref_Counted_Pointer is access Ref_Counted_Record;

	type Ref_Counted is new Ada.Finalization.Controlled with record
		Data: Ref_Counted_Pointer;
	end record;

	procedure Create (R: in out Ref_Counted);
	procedure Create (R: in out Ref_Counted; V: in Item_Type);

	function Get_Item_Pointer (R: in out Ref_Counted) return Item_Pointer;
	pragma Inline(Get_Item_Pointer);

	function Is_Shared (R: in Ref_Counted) return Standard.Boolean;
	pragma Inline(Is_Shared);


	overriding procedure Initialize (R: in out Ref_Counted);
	overriding procedure Adjust (R: in out Ref_Counted);
	overriding procedure Finalize (R: in out Ref_Counted);

	
end H3.MM;