with H3.Runes;
with H3.Strings;
with Ada.Finalization;
with Ada.Text_IO;

generic
	type Rune_Type is (<>);
package H3.Compilers is
	package R is new H3.Runes(Rune_Type);
	package S is new H3.Strings(Rune_Type);

	Syntax_Error: exception;

	--type Compiler is tagged limited private;
	type Compiler is new Ada.Finalization.Limited_Controlled with private;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array);
	procedure End_Feed (C: in out Compiler);

	overriding procedure Initialize (C: in out Compiler);
	overriding procedure Finalize (C: in out Compiler);

private
	type Lexer_State is (
		LX_START,

		LX_COMMENT,
		LX_CSTR,
		LX_DIRECTIVE,
		LX_IDENT,
		LX_NUMBER,
		LX_OP_DIV,
		LX_OP_GREATER,
		LX_OP_LESS,
		LX_OP_MINUS,
		LX_OP_MUL,
		LX_OP_PLUS
	);
	type Lexer is record
		State: Lexer_State := LX_START;
	end record;

	type Token_Id is (
		TK_BSTR,
		TK_BYTE,
		TK_CHAR,
		TK_CSTR,
		TK_DIRECTIVE,
		TK_DIV,
		TK_DIVDIV,
		TK_EOF,
		TK_EOL,
		TK_IDENT,
		TK_GE,
		TK_GT,
		TK_LE,
		TK_LT,
		TK_MINUS,
		TK_MINUSMINUS,
		TK_MUL,
		TK_MULMUL,
		TK_PLUS,
		TK_PLUSPLUS,
		TK_SEMICOLON
	);
	type Token is record
		Id: Token_Id := TK_EOF;
		Buf: S.Elastic_String;
	end record;

	-- ------------------------------------------------------------------

	type Parse_State_Code is (
		PS_START,
		PS_INCLUDE_TARGET,
		PS_INCLUDE_TERMINATOR
	);

	type Parse_State is record
		Current: Parse_State_Code := PS_START;
	end record;

	type Parse_State_Array is array(System_Index range<>) of Parse_State;

	type Parse_State_Stack(Capa: System_Index) is record
		States: Parse_State_Array(System_Index'First .. Capa);
		Top: System_Size := System_Size'First; -- 0
	end record;

	-- ------------------------------------------------------------------

	type Stream is record
		Handle: Ada.Text_IO.File_Type;
		Prs_Level: System_Index;
	end record;

	type Stream_Array is array(System_Index range <>) of Stream;

	type Include_Stack(Capa: System_Index) is record
		Streams: Stream_Array(System_Index'First .. Capa);
		Top: System_Size := System_Size'First; -- 0
	end record;
	-- ------------------------------------------------------------------

	--type Compiler is tagged limited record
	type Compiler is new Ada.Finalization.Limited_Controlled with record
		Lx: Lexer;
		Tk: Token;
		Prs: Parse_State_Stack(128);  -- TODO: make this dynamic. single access type. dynamic allocation
		Inc: Include_Stack(32); -- TODO: make this dynamic. single access type. dynamic allocation
	end record;

end H3.Compilers;
