with Ada.Unchecked_Deallocation;

package body H3.Trees is

	procedure New_Node (Tr: in out Tree; Code: Node_Code) is
		N: Node_Pointer;
	begin
		N := new Node(Code);
		N.Next := Tr.Top;
		Tr.Top := N;
	end New_Node;

	procedure Free_Node (Tr: in out Tree; N: in out Node) is
	begin
		--case N.Code is
		--	when NODE_...
		--end case;
		null;
	end Free_Node;

	-- ------------------------------------------------------------------

	overriding procedure Initialize (C: in out Tree) is
	begin
		null;
	end Initialize;

	overriding procedure Finalize (C: in out Tree) is
	begin
		null;
	end Finalize;
end H3.Trees;
