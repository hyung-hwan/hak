with Ada.Finalization;

package H3.Trees is

	--package A is new H3.Arrays(XXXX, 0);

	type Node_Code is (
		NODE_ASSIGN,
		NODE_CALL,
		NODE_CLASS,
		NODE_IF,
		NODE_FUN,
		NODE_VOID,
		NODE_WHILE
	);

	type Node;
	type Node_Pointer is access Node;

	type Node(Code: Node_Code := NODE_VOID) is record
		-- Loc: location.
		Next: Node_Pointer;
		case Code is
			when NODE_ASSIGN =>
				null;
			when NODE_CALL =>
				null;
			when NODE_CLASS =>
				null;
			when NODE_IF =>
				null;
			when NODE_FUN =>
				null;
			when NODE_VOID =>
				null;
			when NODE_WHILE =>
				null;
		end case;
	end record;

	-- parse tree
	type Tree is new Ada.Finalization.Limited_Controlled with record
		--Next_Node: System_Index := System_Index'First;
		--Toplevel_Node := Node;
		Top: Node_Pointer := null;
	end record;

	-- ------------------------------------------------------------------
	procedure New_Node (Tr: in out Tree);


	overriding procedure Initialize (Tr: in out Tree);
	overriding procedure Finalize (Tr: in out Tree);
end H3.Trees;
