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
	--EOF: constant Code := Code'First;

	package P is
		EOF                 : constant Code := -1; -- Code'First
		NUL                 : constant Code := 0;
		SOH                 : constant Code := 1;
		STX                 : constant Code := 2;
		ETX                 : constant Code := 3;
		EOT                 : constant Code := 4;
		ENQ                 : constant Code := 5;
		ACK                 : constant Code := 6;
		BEL                 : constant Code := 7;
		BS                  : constant Code := 8;
		HT                  : constant Code := 9;
		LF                  : constant Code := 10;
		VT                  : constant Code := 11;
		FF                  : constant Code := 12;
		CR                  : constant Code := 13;
		SO                  : constant Code := 14;
		SI                  : constant Code := 15;
		DLE                 : constant Code := 16;
		DC1                 : constant Code := 17;
		DC2                 : constant Code := 18;
		DC3                 : constant Code := 19;
		DC4                 : constant Code := 20;
		NAK                 : constant Code := 21;
		SYN                 : constant Code := 22;
		ETB                 : constant Code := 23;
		CAN                 : constant Code := 24;
		EM                  : constant Code := 25;
		SUB                 : constant Code := 26;
		ESC                 : constant Code := 27;
		FS                  : constant Code := 28;
		GS                  : constant Code := 29;
		RS                  : constant Code := 30;
		US                  : constant Code := 31;
		Space               : constant Code := 32;
		Exclamation         : constant Code := 33; -- !
		Quotation           : constant Code := 34; -- "
		Number_Sign         : constant Code := 35; -- #
		Dollar_Sign         : constant Code := 36; -- $
		Percent_Sign        : constant Code := 37; -- %
		Ampersand           : constant Code := 38; -- &
		Apostrophe          : constant Code := 39; -- '
		Left_Parenthesis    : constant Code := 40; -- (
		Right_Parenthesis   : constant Code := 41; -- )
		Asterisk            : constant Code := 42; -- *
		Plus_Sign           : constant Code := 43; -- +
		Comma               : constant Code := 44; -- ,
		Minus_Sign          : constant Code := 45; -- -
		Period              : constant Code := 46; -- .
		Slash               : constant Code := 47; -- /
		Zero                : constant Code := 48; -- 0
		One                 : constant Code := 49; -- 1
		Two                 : constant Code := 50; -- 2
		Three               : constant Code := 51; -- 3
		Four                : constant Code := 52; -- 4
		Five                : constant Code := 53; -- 5
		Six                 : constant Code := 54; -- 6
		Seven               : constant Code := 55; -- 7
		Eight               : constant Code := 56; -- 8
		Nine                : constant Code := 57; -- 9
		Colon               : constant Code := 58; -- :
		Semicolon           : constant Code := 59; -- ;
		Left_Arrow          : constant Code := 60; -- <
		Equal_Sign          : constant Code := 61; -- =
		Right_Arrow         : constant Code := 62; -- >
		Question            : constant Code := 63; -- ?
		Commercial_At       : constant Code := 64; -- @
		UC_A                : constant Code := 65; -- A
		UC_B                : constant Code := 66; -- B
		UC_C                : constant Code := 67; -- C
		UC_D                : constant Code := 68; -- D
		UC_E                : constant Code := 69; -- E
		UC_F                : constant Code := 70; -- F
		UC_G                : constant Code := 71; -- G
		UC_H                : constant Code := 72; -- H
		UC_I                : constant Code := 73; -- I
		UC_J                : constant Code := 74; -- J
		UC_K                : constant Code := 75; -- K
		UC_L                : constant Code := 76; -- L
		UC_M                : constant Code := 77; -- M
		UC_N                : constant Code := 78; -- N
		UC_O                : constant Code := 79; -- O
		UC_P                : constant Code := 80; -- P
		UC_Q                : constant Code := 81; -- Q
		UC_R                : constant Code := 82; -- R
		UC_S                : constant Code := 83; -- S
		UC_T                : constant Code := 84; -- T
		UC_U                : constant Code := 85; -- U
		UC_V                : constant Code := 86; -- V
		UC_W                : constant Code := 87; -- W
		UC_X                : constant Code := 88; -- X
		UC_Y                : constant Code := 89; -- Y
		UC_Z                : constant Code := 90; -- Z
		Left_Square_Bracket : constant Code := 91; -- [
		Backslash           : constant Code := 92; -- \
		Right_Square_Bracket: constant Code := 93; -- ]
		Circumflex          : constant Code := 94; -- ^
		Underline           : constant Code := 95; -- _
		Grave               : constant Code := 96; -- `
		LC_A                : constant Code := 97; -- a
		LC_B                : constant Code := 98; -- b
		LC_C                : constant Code := 99; -- c
		LC_D                : constant Code := 100; -- d
		LC_E                : constant Code := 101; -- e
		LC_F                : constant Code := 102; -- f
		LC_G                : constant Code := 103; -- g
		LC_H                : constant Code := 104; -- h
		LC_I                : constant Code := 105; -- i
		LC_J                : constant Code := 106; -- j
		LC_K                : constant Code := 107; -- k
		LC_L                : constant Code := 108; -- l
		LC_M                : constant Code := 109; -- m
		LC_N                : constant Code := 110; -- n
		LC_O                : constant Code := 111; -- o
		LC_P                : constant Code := 112; -- p
		LC_Q                : constant Code := 113; -- q
		LC_R                : constant Code := 114; -- r
		LC_S                : constant Code := 115; -- s
		LC_T                : constant Code := 116; -- t
		LC_U                : constant Code := 117; -- u
		LC_V                : constant Code := 118; -- v
		LC_W                : constant Code := 119; -- w
		LC_X                : constant Code := 120; -- x
		LC_Y                : constant Code := 121; -- y
		LC_Z                : constant Code := 122; -- z
		Left_Curly_Bracket  : constant Code := 123; -- {
		Vertical_Line       : constant Code := 124; -- |
		Right_Curly_Bracket : constant Code := 125; -- }
		Tilde               : constant Code := 126; -- ~
		DEL                 : constant Code := 127;
	end P;

	package V is
		NUL                 : constant Rune := Rune'Val(P.NUL);
		SOH                 : constant Rune := Rune'Val(P.SOH);
		STX                 : constant Rune := Rune'Val(P.STX);
		ETX                 : constant Rune := Rune'Val(P.ETX);
		EOT                 : constant Rune := Rune'Val(P.EOT);
		ENQ                 : constant Rune := Rune'Val(P.ENQ);
		ACK                 : constant Rune := Rune'Val(P.ACK);
		BEL                 : constant Rune := Rune'Val(P.BEL);
		BS                  : constant Rune := Rune'Val(P.BS);
		HT                  : constant Rune := Rune'Val(P.HT);
		LF                  : constant Rune := Rune'Val(P.LF);
		VT                  : constant Rune := Rune'Val(P.VT);
		FF                  : constant Rune := Rune'Val(P.FF);
		CR                  : constant Rune := Rune'Val(P.CR);
		SO                  : constant Rune := Rune'Val(P.SO);
		SI                  : constant Rune := Rune'Val(P.SI);
		DLE                 : constant Rune := Rune'Val(P.DLE);
		DC1                 : constant Rune := Rune'Val(P.DC1);
		DC2                 : constant Rune := Rune'Val(P.DC2);
		DC3                 : constant Rune := Rune'Val(P.DC3);
		DC4                 : constant Rune := Rune'Val(P.DC4);
		NAK                 : constant Rune := Rune'Val(P.NAK);
		SYN                 : constant Rune := Rune'Val(P.SYN);
		ETB                 : constant Rune := Rune'Val(P.ETB);
		CAN                 : constant Rune := Rune'Val(P.CAN);
		EM                  : constant Rune := Rune'Val(P.EM);
		SUB                 : constant Rune := Rune'Val(P.SUB);
		ESC                 : constant Rune := Rune'Val(P.ESC);
		FS                  : constant Rune := Rune'Val(P.FS);
		GS                  : constant Rune := Rune'Val(P.GS);
		RS                  : constant Rune := Rune'Val(P.RS);
		US                  : constant Rune := Rune'Val(P.US);
		Space               : constant Rune := Rune'Val(P.Space);
		Exclamation         : constant Rune := Rune'Val(P.Exclamation);
		Quotation           : constant Rune := Rune'Val(P.Quotation);
		Number_Sign         : constant Rune := Rune'Val(P.Number_Sign);
		Dollar_Sign         : constant Rune := Rune'Val(P.Dollar_Sign);
		Percent_Sign        : constant Rune := Rune'Val(P.Percent_Sign);
		Ampersand           : constant Rune := Rune'Val(P.Ampersand);
		Apostrophe          : constant Rune := Rune'Val(P.Apostrophe);
		Left_Parenthesis    : constant Rune := Rune'Val(P.Left_Parenthesis);
		Right_Parenthesis   : constant Rune := Rune'Val(P.Right_Parenthesis);
		Asterisk            : constant Rune := Rune'Val(P.Asterisk);
		Plus_Sign           : constant Rune := Rune'Val(P.Plus_Sign);
		Comma               : constant Rune := Rune'Val(P.Comma);
		Minus_Sign          : constant Rune := Rune'Val(P.Minus_Sign);
		Period              : constant Rune := Rune'Val(P.Period);
		Slash               : constant Rune := Rune'Val(P.Slash);
		Zero                : constant Rune := Rune'Val(P.Zero);
		One                 : constant Rune := Rune'Val(P.One);
		Two                 : constant Rune := Rune'Val(P.Two);
		Three               : constant Rune := Rune'Val(P.Three);
		Four                : constant Rune := Rune'Val(P.Four);
		Five                : constant Rune := Rune'Val(P.Five);
		Six                 : constant Rune := Rune'Val(P.Six);
		Seven               : constant Rune := Rune'Val(P.Seven);
		Eight               : constant Rune := Rune'Val(P.Eight);
		Nine                : constant Rune := Rune'Val(P.Nine);
		Colon               : constant Rune := Rune'Val(P.Colon);
		Semicolon           : constant Rune := Rune'Val(P.Semicolon);
		Left_Arrow          : constant Rune := Rune'Val(P.Left_Arrow);
		Equal_Sign          : constant Rune := Rune'Val(P.Equal_Sign);
		Right_Arrow         : constant Rune := Rune'Val(P.Right_Arrow);
		Question            : constant Rune := Rune'Val(P.Question);
		Commercial_At       : constant Rune := Rune'Val(P.Commercial_At);
		UC_A                : constant Rune := Rune'Val(P.UC_A);
		UC_B                : constant Rune := Rune'Val(P.UC_B);
		UC_C                : constant Rune := Rune'Val(P.UC_C);
		UC_D                : constant Rune := Rune'Val(P.UC_D);
		UC_E                : constant Rune := Rune'Val(P.UC_E);
		UC_F                : constant Rune := Rune'Val(P.UC_F);
		UC_G                : constant Rune := Rune'Val(P.UC_G);
		UC_H                : constant Rune := Rune'Val(P.UC_H);
		UC_I                : constant Rune := Rune'Val(P.UC_I);
		UC_J                : constant Rune := Rune'Val(P.UC_J);
		UC_K                : constant Rune := Rune'Val(P.UC_K);
		UC_L                : constant Rune := Rune'Val(P.UC_L);
		UC_M                : constant Rune := Rune'Val(P.UC_M);
		UC_N                : constant Rune := Rune'Val(P.UC_N);
		UC_O                : constant Rune := Rune'Val(P.UC_O);
		UC_P                : constant Rune := Rune'Val(P.UC_P);
		UC_Q                : constant Rune := Rune'Val(P.UC_Q);
		UC_R                : constant Rune := Rune'Val(P.UC_R);
		UC_S                : constant Rune := Rune'Val(P.UC_S);
		UC_T                : constant Rune := Rune'Val(P.UC_T);
		UC_U                : constant Rune := Rune'Val(P.UC_U);
		UC_V                : constant Rune := Rune'Val(P.UC_V);
		UC_W                : constant Rune := Rune'Val(P.UC_W);
		UC_X                : constant Rune := Rune'Val(P.UC_X);
		UC_Y                : constant Rune := Rune'Val(P.UC_Y);
		UC_Z                : constant Rune := Rune'Val(P.UC_Z);
		Left_Square_Bracket : constant Rune := Rune'Val(P.Left_Square_Bracket);
		Backslash           : constant Rune := Rune'Val(P.Backslash);
		Right_Square_Bracket: constant Rune := Rune'Val(P.Right_Square_Bracket);
		Circumflex          : constant Rune := Rune'Val(P.Circumflex);
		Underline           : constant Rune := Rune'Val(P.Underline);
		Grave               : constant Rune := Rune'Val(P.Grave);
		LC_A                : constant Rune := Rune'Val(P.LC_A);
		LC_B                : constant Rune := Rune'Val(P.LC_B);
		LC_C                : constant Rune := Rune'Val(P.LC_C);
		LC_D                : constant Rune := Rune'Val(P.LC_D);
		LC_E                : constant Rune := Rune'Val(P.LC_E);
		LC_F                : constant Rune := Rune'Val(P.LC_F);
		LC_G                : constant Rune := Rune'Val(P.LC_G);
		LC_H                : constant Rune := Rune'Val(P.LC_H);
		LC_I                : constant Rune := Rune'Val(P.LC_I);
		LC_J                : constant Rune := Rune'Val(P.LC_J);
		LC_K                : constant Rune := Rune'Val(P.LC_K);
		LC_L                : constant Rune := Rune'Val(P.LC_L);
		LC_M                : constant Rune := Rune'Val(P.LC_M);
		LC_N                : constant Rune := Rune'Val(P.LC_N);
		LC_O                : constant Rune := Rune'Val(P.LC_O);
		LC_P                : constant Rune := Rune'Val(P.LC_P);
		LC_Q                : constant Rune := Rune'Val(P.LC_Q);
		LC_R                : constant Rune := Rune'Val(P.LC_R);
		LC_S                : constant Rune := Rune'Val(P.LC_S);
		LC_T                : constant Rune := Rune'Val(P.LC_T);
		LC_U                : constant Rune := Rune'Val(P.LC_U);
		LC_V                : constant Rune := Rune'Val(P.LC_V);
		LC_W                : constant Rune := Rune'Val(P.LC_W);
		LC_X                : constant Rune := Rune'Val(P.LC_X);
		LC_Y                : constant Rune := Rune'Val(P.LC_Y);
		LC_Z                : constant Rune := Rune'Val(P.LC_Z);
		Left_Curly_Bracket  : constant Rune := Rune'Val(P.Left_Curly_Bracket);
		Vertical_Line       : constant Rune := Rune'Val(P.Vertical_Line);
		Right_Curly_Bracket : constant Rune := Rune'Val(P.Right_Curly_Bracket);
		Tilde               : constant Rune := Rune'Val(P.Tilde);
		DEL                 : constant Rune := Rune'Val(P.DEL);
	end V;


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
