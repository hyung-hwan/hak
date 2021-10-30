generic
	-- any discrete type accepted.
	-- can't ada limit type to one of Character, Wide_Character, Wide_Wide_Character?
	type Rune_Type is (<>); 
package H3.Runes is
	-- <ctype.h>-like character classification plus other features.
	-- unicode-based. no system locale honored.

	subtype Rune is Rune_Type;
	type Code is range -1 .. 16#7FFF_FFFF#;

	-- virtual code to indicate end of input
	EOF: constant Code := Code'First;

	C_Colon: constant Code := System_Rune'Pos(':');
	C_Semicolon: constant Code := System_Rune'Pos(';');
	C_Tilde: constant Code := System_Rune'Pos('~');
	C_Underline: constant Code := System_Rune'Pos('_');
	C_Equal: constant Code := System_Rune'Pos('=');
	C_Left_Arrow: constant Code := System_Rune'Pos('<');
	C_Right_Arrow: constant Code := System_Rune'Pos('>');

	C_A: constant Code := System_Rune'Pos('A');
	C_B: constant Code := System_Rune'Pos('B');
	C_C: constant Code := System_Rune'Pos('C');
	C_D: constant Code := System_Rune'Pos('D');
	C_E: constant Code := System_Rune'Pos('E');

	Colon: constant Rune := Rune'Val(C_Colon);
	Semicolon: constant Rune := Rune'Val(C_Semicolon);
	Tilde: constant Rune := Rune'Val(C_Tilde);
	Underline: constant Rune := Rune'Val(C_Underline);
	Equal: constant Rune := Rune'Val(C_Equal);
	Left_Arrow: constant Rune := Rune'Val(C_Left_Arrow);
	Right_Arrow: constant Rune := Rune'Val(C_Right_Arrow);

	UC_A: constant Rune := Rune'Val(C_A);
	UC_B: constant Rune := Rune'Val(C_B);
	UC_C: constant Rune := Rune'Val(C_C);
	UC_D: constant Rune := Rune'Val(C_D);
	UC_E: constant Rune := Rune'Val(C_E);
	UC_O: constant Rune := Rune'Val(System_Rune'Pos('O'));
	UC_F: constant Rune := Rune'Val(System_Rune'Pos('F'));

	type Item_Class is (
		ALPHA,
		ALNUM,
		BLANK,
		CNTRL,
		DIGIT,
		GRAPH,
		LOWER,
		PRINT,
		PUNCT,
		SPACE,
		UPPER,
		XDIGIT
	);

	function Is_Alpha (V: in Rune) return Boolean;
	function Is_Alnum (V: in Rune) return Boolean;
	function Is_Blank (V: in Rune) return Boolean;
	function Is_Cntrl (V: in Rune) return Boolean;
	function Is_Digit (V: in Rune) return Boolean;
	function Is_Graph (V: in Rune) return Boolean;
	function Is_Print (V: in Rune) return Boolean;
	function Is_Punct (V: in Rune) return Boolean;
	function Is_Space (V: in Rune) return Boolean;
	function Is_Xdigit (V: in Rune) return Boolean;
	function Is_Lower (V: in Rune) return Boolean;
	function Is_Upper (V: in Rune) return Boolean;
	
	function To_Lower (V: in Rune) return Rune;
	function To_Upper (V: in Rune) return Rune;

	function Is_Alpha (C: in Code) return Boolean;
	function Is_Alnum (C: in Code) return Boolean;
	function Is_Blank (C: in Code) return Boolean;
	function Is_Cntrl (C: in Code) return Boolean;
	function Is_Digit (C: in Code) return Boolean;
	function Is_Graph (C: in Code) return Boolean;
	function Is_Print (C: in Code) return Boolean;
	function Is_Punct (C: in Code) return Boolean;
	function Is_Space (C: in Code) return Boolean;
	function Is_Xdigit (C: in Code) return Boolean;
	function Is_Lower (C: in Code) return Boolean;
	function Is_Upper (C: in Code) return Boolean;

	function Is_Class (V: in Rune; Class: in Item_Class) return Boolean;
	function Is_Class (C: in Code; Class: in Item_Class) return Boolean;

	function Is_Eof (C: in Code) return Boolean;
	pragma Inline (Is_Eof);

	function Is_Code (V: in Rune; C: in Code) return Boolean;
	pragma Inline (Is_Code);
	function Is_Rune (C: in Code; V: in Rune) return Boolean;
	pragma Inline (Is_Rune);

	function To_Rune (C: in Code) return Rune;
	pragma Inline (To_Rune);
	function To_Code (V: in Rune) return Code;
	pragma Inline (To_Code);

end H3.Runes;
