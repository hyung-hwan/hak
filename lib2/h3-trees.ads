use H3.Arrays;

package H3.Trees is

	-- parse tree

	--package A is new H3.Arrays(XXXX, 0);

	type Node_Code is (
		NODE_ASSIGN,
		NODE_CALL,
		NODE_CLASS
		NODE_IF,
		NODE_FUN,
		NODE_VOID,
		NODE_WHILE
	);

	type Node(Code: Node_Code := NODE_VOID) is record
		-- Loc: location.
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
	end record;

	type Tree is record
		Next_Node: System_Index := System_Index'First;
		Toplevel_Node := Node
	end Tree;

end H3.Trees;
