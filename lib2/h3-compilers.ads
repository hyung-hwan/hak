with H3.Runes;
with H3.Strings;
with Ada.Text_IO;

generic
	type Rune_Type is (<>);
package H3.Compilers is
	package R is new H3.Runes(Rune_Type);
	package S is new H3.Strings(Rune_Type);

	Syntax_Error: exception;

	type Compiler is tagged limited private;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array);
	procedure End_Feed (C: in out Compiler);

private
	type Lexer_State is (
		LX_START,

		LX_COMMENT,
		LX_CSTR,
		LX_DIRECTIVE,
		LX_IDENT,
		LX_NUMBER,
		LX_OP_GREATER,
		LX_OP_LESS
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
		TK_EOF,
		TK_EOL,
		TK_IDENT,
		TK_GE,
		TK_GT,
		TK_LE,
		TK_LT,
		TK_SEMICOLON
	);
	type Token is record
		Id: Token_Id := TK_EOF;
		Buf: S.Elastic_String;
	end record;

	type Parser_State is (
		PS_START,
		PS_INCLUDE_TARGET,
		PS_INCLUDE_TERMINATOR
	);
	type Parser is record
		State: Parser_State := PS_START;
		Prev_State: Parser_State := PS_START;
		Level: System_Index := 1;
	end record;

	type Stream is record
		Handle: Ada.Text_IO.File_Type;
		--Handle: System_Size;

		
		Initial_Level: System_Index; -- the block level where this inclusion is entered
		Initial_Parser_State: Parser_State;  -- the parser state before the #include has been seen?
		Next_Parser_State: Parser_State;
	end record;

	type Stream_Array is array(System_Index range <>) of Stream;
	type Include_Stack(Capa: System_Index) is record
		Streams: Stream_Array(System_Index'First .. Capa);
		Top: System_Size := 0;
	end record;

	type Compiler is tagged limited record
		Lx: Lexer;
		Tk: Token;
		Ps: Parser;
		Inc: Include_Stack(32);
	end record;
end H3.Compilers;
