with H3.Runes;
with H3.Strings;

generic
	type Rune_Type is (<>);
package H3.Compilers is
	package R is new H3.Runes(Rune_Type);
	package S is new H3.Strings(Rune_Type);

	Syntax_Error: exception;

	type Compiler is tagged private;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array);
	procedure End_Feed (C: in out Compiler);

private
	type Lexer_State is (
		LX_START,
		LX_COMMENT,
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

	type Parser_State is (START, INCLUDE);
	type Parser is record
		State: Parser_State := START;
	end record;

	type Compiler is tagged record
		Lx: Lexer;
		Tk: Token;
	end record;
end H3.Compilers;
