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

	package P is
		NUL                 : constant := 0;
		SOH                 : constant := 1;
		STX                 : constant := 2;
		ETX                 : constant := 3;
		EOT                 : constant := 4;
		ENQ                 : constant := 5;
		ACK                 : constant := 6;
		BEL                 : constant := 7;
		BS                  : constant := 8;
		HT                  : constant := 9;
		LF                  : constant := 10;
		VT                  : constant := 11;
		FF                  : constant := 12;
		CR                  : constant := 13;
		SO                  : constant := 14;
		SI                  : constant := 15;
		DLE                 : constant := 16;
		DC1                 : constant := 17;
		DC2                 : constant := 18;
		DC3                 : constant := 19;
		DC4                 : constant := 20;
		NAK                 : constant := 21;
		SYN                 : constant := 22;
		ETB                 : constant := 23;
		CAN                 : constant := 24;
		EM                  : constant := 25;
		SUB                 : constant := 26;
		ESC                 : constant := 27;
		FS                  : constant := 28;
		GS                  : constant := 29;
		RS                  : constant := 30;
		US                  : constant := 31;
		Space               : constant := 32; --  
		Exclamation         : constant := 33; -- !
		Quotation           : constant := 34; -- "
		Number_Sign         : constant := 35; -- #
		Dollar_Sign         : constant := 36; -- $
		Percent_Sign        : constant := 37; -- %
		Ampersand           : constant := 38; -- &
		Apostrophe          : constant := 39; -- '
		Left_Parenthesis    : constant := 40; -- (
		Right_Parenthesis   : constant := 41; -- )
		Asterisk            : constant := 42; -- *
		Plus_Sign           : constant := 43; -- +
		Comma               : constant := 44; -- ,
		Minus_Sign          : constant := 45; -- -
		Period              : constant := 46; -- .
		Slash               : constant := 47; -- /
		Zero                : constant := 48; -- 0
		One                 : constant := 49; -- 1
		Two                 : constant := 50; -- 2
		Three               : constant := 51; -- 3
		Four                : constant := 52; -- 4
		Five                : constant := 53; -- 5
		Six                 : constant := 54; -- 6
		Seven               : constant := 55; -- 7
		Eight               : constant := 56; -- 8
		Nine                : constant := 57; -- 9
		Colon               : constant := 58; -- :
		Semicolon           : constant := 59; -- ;
		Left_Arrow          : constant := 60; -- <
		Equal_Sign          : constant := 61; -- =
		Right_Arrow         : constant := 62; -- >
		Question            : constant := 63; -- ?
		Commercial_At       : constant := 64; -- @
		UC_A                : constant := 65; -- A
		UC_B                : constant := 66; -- B
		UC_C                : constant := 67; -- C
		UC_D                : constant := 68; -- D
		UC_E                : constant := 69; -- E
		UC_F                : constant := 70; -- F
		UC_G                : constant := 71; -- G
		UC_H                : constant := 72; -- H
		UC_I                : constant := 73; -- I
		UC_J                : constant := 74; -- J
		UC_K                : constant := 75; -- K
		UC_L                : constant := 76; -- L
		UC_M                : constant := 77; -- M
		UC_N                : constant := 78; -- N
		UC_O                : constant := 79; -- O
		UC_P                : constant := 80; -- P
		UC_Q                : constant := 81; -- Q
		UC_R                : constant := 82; -- R
		UC_S                : constant := 83; -- S
		UC_T                : constant := 84; -- T
		UC_U                : constant := 85; -- U
		UC_V                : constant := 86; -- V
		UC_W                : constant := 87; -- W
		UC_X                : constant := 88; -- X
		UC_Y                : constant := 89; -- Y
		UC_Z                : constant := 90; -- Z
		Left_Square_Bracket : constant := 91; -- [
		Backslash           : constant := 92; -- \
		Right_Square_Bracket: constant := 93; -- ]
		Circumflex          : constant := 94; -- ^
		Underline           : constant := 95; -- _
		Grave               : constant := 96; -- `
		LC_A                : constant := 97; -- a
		LC_B                : constant := 98; -- b
		LC_C                : constant := 99; -- c
		LC_D                : constant := 100; -- d
		LC_E                : constant := 101; -- e
		LC_F                : constant := 102; -- f
		LC_G                : constant := 103; -- g
		LC_H                : constant := 104; -- h
		LC_I                : constant := 105; -- i
		LC_J                : constant := 106; -- j
		LC_K                : constant := 107; -- k
		LC_L                : constant := 108; -- l
		LC_M                : constant := 109; -- m
		LC_N                : constant := 110; -- n
		LC_O                : constant := 111; -- o
		LC_P                : constant := 112; -- p
		LC_Q                : constant := 113; -- q
		LC_R                : constant := 114; -- r
		LC_S                : constant := 115; -- s
		LC_T                : constant := 116; -- t
		LC_U                : constant := 117; -- u
		LC_V                : constant := 118; -- v
		LC_W                : constant := 119; -- w
		LC_X                : constant := 120; -- x
		LC_Y                : constant := 121; -- y
		LC_Z                : constant := 122; -- z
		Left_Curly_Bracket  : constant := 123; -- {
		Vertical_Line       : constant := 124; -- |
		Right_Curly_Bracket : constant := 125; -- }
		Tilde               : constant := 126; -- ~
		DEL                 : constant := 127;
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
