with Ada.Unchecked_Deallocation;

with ada.text_io;

package body H3.Trees is

	procedure New_Node (Tr: in out Tree) is
		N: Node_Pointer;
	begin
		--N := new Node'(Code => NODE_VOID, Next => null );
		N := new Node;

		N.all := (Code => NODE_VOID, Next => Null);
		N.Next := Tr.Top;
		Tr.Top := N;

		ada.text_io.put_line ("new node...");
	end New_Node;

	procedure Free_Node (Tr: in out Tree; N: in out Node) is
	begin
		--case N.Code is
		--	when NODE_...
		--end case;
		null;
	end Free_Node;

	-- ------------------------------------------------------------------

	overriding procedure Initialize (Tr: in out Tree) is
	begin
		null;
	end Initialize;

	overriding procedure Finalize (Tr: in out Tree) is
	begin
		null;
	end Finalize;
end H3.Trees;
