with System.UTF_32; -- TOOD: remove dependency on this package. create a seperate unicode package.

package body H3.Runes is

	package UC renames System.UTF_32;
	use type System.UTF_32.Category;

	SP: constant Rune := Rune'Val(32);
	HT: constant Rune := Rune'Val(9);

	function Is_Alpha (V: in Rune) return Boolean is
	begin
		return UC.Is_UTF_32_Letter(Rune'Pos(V));
	end Is_Alpha;

	function Is_Alpha (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Alpha(Rune'Val(C));
	end Is_Alpha;

	function Is_Alnum (V: in Rune) return Boolean is
	begin
		return UC.Is_UTF_32_Letter(Rune'Pos(V)) or else
		       UC.Is_UTF_32_Digit(Rune'Pos(V));
	end Is_Alnum;

	function Is_Alnum (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Alnum(Rune'Val(C));
	end Is_Alnum;

	function Is_Blank (V: in Rune) return Boolean is
	begin
		return V = SP or else V = HT;
	end Is_Blank;

	function Is_Blank (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Blank(Rune'Val(C));
	end Is_Blank;

	function Is_Cntrl (V: in Rune) return Boolean is
	begin
		return UC.Get_Category(Rune'Pos(V)) = UC.Cc;
	end Is_Cntrl;

	function Is_Cntrl (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Cntrl(Rune'Val(C));
	end Is_Cntrl;

	function Is_Digit (V: in Rune) return Boolean is
	begin
		return UC.Is_UTF_32_Digit(Rune'Pos(V));
	end Is_Digit;

	function Is_Digit (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Digit(Rune'Val(C));
	end Is_Digit;

	function Is_Graph (V: in Rune) return Boolean is
	begin
		return Is_Print(V) and then V /= SP;
	end Is_Graph;

	function Is_Graph (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Graph(Rune'Val(C));
	end Is_Graph;

	function Is_Lower (V: in Rune) return Boolean is
	begin
		return UC.Get_Category(Rune'Pos(V)) = UC.Ll;
	end Is_Lower;

	function Is_Lower (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Lower(Rune'Val(C));
	end Is_Lower;

	function Is_Print (V: in Rune) return Boolean is
	begin
		return not UC.IS_UTF_32_Non_Graphic(Rune'Pos(V));
	end Is_Print;

	function Is_Print (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Print(Rune'Val(C));
	end Is_Print;

	function Is_Punct (V: in Rune) return Boolean is
	begin
		--return UC.Is_UTF_32_Punctuation(Rune'Pos(V));
		return Is_Print(V) and then not Is_Space(V) and then not Is_Alnum(V);
	end Is_Punct;

	function Is_Punct (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Punct(Rune'Val(C));
	end Is_Punct;

	function Is_Space (V: in Rune) return Boolean is
	begin
		return UC.Is_UTF_32_Space(Rune'Pos(V)) or else
		       UC.Is_UTF_32_Line_Terminator(Rune'Pos(V)) or else
		       V = HT;
	end Is_Space;

	function Is_Space (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Space(Rune'Val(C));
	end Is_Space;

	function Is_Upper (V: in Rune) return Boolean is
	begin
		return UC.Get_Category(Rune'Pos(V)) = UC.Lu;
	end Is_Upper;

	function Is_Upper (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Upper(Rune'Val(C));
	end Is_Upper;

	function Is_Xdigit (V: in Rune) return Boolean is
	begin
		return UC.Is_UTF_32_Digit(Rune'Pos(V)) or else
		       Rune'Pos(V) in System_Rune'Pos('A') .. System_Rune'Pos('F') or else
		       Rune'Pos(V) in System_Rune'Pos('a') .. System_Rune'Pos('f');
	end Is_Xdigit;

	function Is_Xdigit (C: in Code) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Xdigit(Rune'Val(C));
	end Is_Xdigit;

	function To_Lower (V: in Rune) return Rune is
	begin
		return Rune'Val(UC.UTF_32_To_Lower_Case(Rune'Pos(V)));
	end To_Lower;

	function To_Upper (V: in Rune) return Rune is
	begin
		return Rune'Val(UC.UTF_32_To_Upper_Case(Rune'Pos(V)));
	end To_Upper;

	function Is_Class (V: in Rune; Class: in Item_Class) return Boolean is
	begin
		case Class is
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

	function Is_Class (C: in Code; Class: in Item_Class) return Boolean is
	begin
		return not Is_Eof(C) and then Is_Class(To_Rune(C), Class);
	end Is_Class;

	function Is_Eof (C: in Code) return Boolean is
	begin
		return C = P.EOF;
	end Is_Eof;

	function Is_Code (V: in Rune; C: in Code) return Boolean is
	begin
		-- a clumsy way to work around strong type checking
		-- with unknown Rune at the generic level?
		return To_Code(V) = C;
	end Is_Code;

	function Is_Rune (C: in Code; V: in Rune) return Boolean is
	begin
		return To_Code(V) = C;
	end Is_Rune;

	function To_Rune (C: in Code) return Rune is
	begin
		pragma Assert (not Is_Eof(C));
		return Rune'Val(C);
	end To_Rune;

	function To_Code (V: in Rune) return Code is
	begin
		return Rune'Pos(V);
	end To_Code;

end H3.Runes;
