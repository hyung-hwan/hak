with System.UTF_32; -- TOOD: remove dependency on this package. create a seperate unicode package.

package body H3.CC is

	package UC renames System.UTF_32;
	use type System.UTF_32.Category;

	SP: constant Item_Type := Item_Type'Val(32);
	HT: constant Item_Type := Item_Type'Val(9);

	function Is_Alpha (V: in Item_Type) return Boolean is
	begin
		return UC.Is_UTF_32_Letter(Item_Type'Pos(V));
	end Is_Alpha;

	function Is_Alnum (V: in Item_Type) return Boolean is
	begin
		return UC.Is_UTF_32_Letter(Item_Type'Pos(V)) or else
		       UC.Is_UTF_32_Digit(Item_Type'Pos(V));
	end Is_Alnum;

	function Is_Blank (V: in Item_Type) return Boolean is
	begin
		return V = SP or else V = HT;
	end Is_Blank;

	function Is_Cntrl (V: in Item_Type) return Boolean is
	begin
		return UC.Get_Category(Item_Type'Pos(V)) = UC.Cc;
	end Is_Cntrl;

	function Is_Digit (V: in Item_Type) return Boolean is
	begin
		return UC.Is_UTF_32_Digit(Item_Type'Pos(V));
	end Is_Digit;

	function Is_Graph (V: in Item_Type) return Boolean is
	begin
		return Is_Print(V) and then V /= SP;
	end Is_Graph;

	function Is_Lower (V: in Item_Type) return Boolean is
	begin
		return UC.Get_Category(Item_Type'Pos(V)) = UC.Ll;
	end Is_Lower;

	function Is_Print (V: in Item_Type) return Boolean is
	begin
		return not UC.IS_UTF_32_Non_Graphic(Item_Type'Pos(V));
	end Is_Print;

	function Is_Punct (V: in Item_Type) return Boolean is
	begin
		--return UC.Is_UTF_32_Punctuation(Item_Type'Pos(V));
		return Is_Print(V) and then not Is_Space(V) and then not Is_Alnum(V);
	end Is_Punct;

	function Is_Space (V: in Item_Type) return Boolean is
	begin
		return UC.Is_UTF_32_Space(Item_Type'Pos(V)) or else
		       UC.Is_UTF_32_Line_Terminator(Item_Type'Pos(V)) or else
		       V = HT;
	end Is_Space;

	function Is_Upper (V: in Item_Type) return Boolean is
	begin
		return UC.Get_Category(Item_Type'Pos(V)) = UC.Lu;
	end Is_Upper;

	function Is_Xdigit (V: in Item_Type) return Boolean is
	begin
		return UC.Is_UTF_32_Digit(Item_Type'Pos(V)) or else
		       Item_Type'Pos(V) in System_Character'Pos('A') .. System_Character'Pos('F') or else
		       Item_Type'Pos(V) in System_Character'Pos('a') .. System_Character'Pos('f');
	end Is_Xdigit;

	function To_Lower (V: in Item_Type) return Item_Type is
	begin
		return Item_Type'Val(UC.UTF_32_To_Lower_Case(Item_Type'Pos(V)));
	end To_Lower;

	function To_Upper (V: in Item_Type) return Item_Type is
	begin
		return Item_Type'Val(UC.UTF_32_To_Upper_Case(Item_Type'Pos(V)));
	end To_Upper;

	function Is_Class (V: in Item_Type; Cls: in Class) return Boolean is
	begin
		case Cls is
			when ALPHA => return Is_Alpha(V);
			when ALNUM => return Is_Alnum(V);
			when BLANK => return Is_Blank(V);
			when CNTRL => return Is_Cntrl(V);
			when DIGIT => return Is_Digit(V);
			when GRAPH => return Is_Graph(V);
			when LOWER => return Is_Lower(V);
			when PRINT => return Is_Print(V);
			when PUNCT => return Is_Punct(V);
			when SPACE => return Is_Space(V);
			when UPPER => return Is_Upper(V);
			when XDIGIT => return Is_Xdigit(V);
		end case;
	end Is_Class;

	function Is_Code (V: in Item_Type; Code: in Item_Code) return Boolean is
	begin
		-- a clumsy way to work around strong type checking
		-- with unknown Item_Type at the generic level?
		return Item_Type'Pos(V) = Code;
	end Is_Code;
end H3.CC;