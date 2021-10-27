generic
	type Item_Type is (<>); -- any discrete type
package H3.CC is
	-- <ctype.h>-like character classification package
	-- unicode-based. no system locale honored.

	subtype Item_Code is H3.Natural;

	Colon: constant Item_Code := System_Character'Pos(':');
	Semicolon: constant Item_Code := System_Character'Pos(';');
	Tilde: constant Item_Code := System_Character'Pos('~');
	Underline: constant Item_Code := System_Character'Pos('_');
	Equal: constant Item_Code := System_Character'Pos('=');
	L_Arrow: constant Item_Code := System_Character'Pos('<');
	R_Arrow: constant Item_Code := System_Character'Pos('>');

	type Item_Class is (ALPHA, ALNUM, BLANK, CNTRL, DIGIT, GRAPH, LOWER, PRINT, PUNCT, SPACE, UPPER, XDIGIT);
	function Is_Class (V: in Item_Type; Class: in Item_Class) return Boolean;

	function Is_Alpha (V: in Item_Type) return Boolean;
	function Is_Alnum (V: in Item_Type) return Boolean;
	function Is_Blank (V: in Item_Type) return Boolean;
	function Is_Cntrl (V: in Item_Type) return Boolean;
	function Is_Digit (V: in Item_Type) return Boolean;
	function Is_Graph (V: in Item_Type) return Boolean;
	function Is_Print (V: in Item_Type) return Boolean;
	function Is_Punct (V: in Item_Type) return Boolean;
	function Is_Space (V: in Item_Type) return Boolean;
	function Is_Xdigit (V: in Item_Type) return Boolean;

	function Is_Lower (V: in Item_Type) return Boolean;
	function Is_Upper (V: in Item_Type) return Boolean;

	function To_Lower (V: in Item_Type) return Item_Type;
	function To_Upper (V: in Item_Type) return Item_Type;

	function Is_Code (V: in Item_Type; Code: in Item_Code) return Boolean;
end H3.CC;