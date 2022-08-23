with H3.Runes;
with H3.Strings;
with H3.Storage;
with H3.Trees;
with Ada.Finalization;
with Ada.Text_IO;

generic
	type Rune_Type is (<>);
	with package Storage_Pool_box is new H3.Storage.Pool_Box(<>);
package H3.Compilers is
	package R is new H3.Runes(Rune_Type);
	package S is new H3.Strings(Rune_Type, Storage_Pool_Box);

	Syntax_Error: exception;
	Internal_Error: exception;

	--type Compiler is tagged limited private;
	type Compiler is new Ada.Finalization.Limited_Controlled with private;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array);
	procedure End_Feed (C: in out Compiler);

	overriding procedure Initialize (C: in out Compiler);
	overriding procedure Finalize (C: in out Compiler);

private
	type Token_Id is (
		TK_ASSIGN,
		TK_BSTR,
		TK_BYTE,
		TK_CHAR,
		TK_COLON,
		TK_CSTR,
		TK_DIRECTIVE,
		TK_DIV,
		TK_DIVDIV,
		TK_DOLLARED_LBRACE,
		TK_DOLLARED_LBRACK,
		TK_DOLLARED_LPAREN,
		TK_EOF,
		TK_EOL,
		TK_HASHED_LBRACE,
		TK_HASHED_LBRACK,
		TK_HASHED_LPAREN,
		TK_IDENT,
		TK_GE,
		TK_GT,
		TK_LBRACE,
		TK_LBRACK,
		TK_LE,
		TK_LPAREN,
		TK_LT,
		TK_MINUS,
		TK_MINUSMINUS,
		TK_MUL,
		TK_MULMUL,
		TK_PLUS,
		TK_PLUSPLUS,
		TK_RBRACE,
		TK_RBRACK,
		TK_RPAREN,
		TK_SEMICOLON
	);
	type Token is record
		Id: Token_Id := TK_EOF;
		Buf: S.Elastic_String;
	end record;

	-- ------------------------------------------------------------------
	type Location is record
		line: System_Size := 0;
		colm: System_Size := 0;
		-- file: S.Bounded_String_Pointer := null;
	end record;

	package Feeder is
		type Lex_State_Code is (LX_START, LX_COMMENT, LX_DT, LX_HC);

		type Lex_Data(State: Lex_State_Code := LX_START) is record
			case State is
				when LX_START =>
					null;
				when LX_COMMENT =>
					null;
				when LX_DT =>
					Row_Start: Integer;
					Row_End: Integer;
					Col_NexT: Integer;
				when LX_HC =>
					Char_Count: System_Size;
			end case;
		end record;

		type Lex is record
			loc: Location;
			oloc: Location;
			data: Lex_Data;
		end record;

		type Read is record
			level: Integer;
			flagv: Integer;
			expect_include_file: Boolean;
			expect_vlist_item: Boolean;
			do_include_file: Boolean;
			-- TODO: obj: Cnode;
		end record;

		type Feed is record
			lx: Lex;
			rd: Read;
		end record;

		procedure Start (C: in out Compiler; Code: in R.Code; Consumed: out Boolean);
		procedure Comment (C: in out Compiler; Code: in R.Code; Consumed: out Boolean);
		procedure Delim_Token (C: in out Compiler; Code: in R.Code; Consumed: out Boolean);
		procedure Hmarked_Token (C: in out Compiler; Code: in R.Code; Consumed: out Boolean);

		procedure Update_Location (C: in out Compiler; Code: in R.Code);
		procedure Begin_Include (C: in out Compiler);
		procedure Feed_From_Includee (C: in out Compiler);
	end Feeder;

	-- ------------------------------------------------------------------

	package Parser is
		-- move parser types here.
	end Parser;

	-- ------------------------------------------------------------------

	type Parse_State_Code is (
		PS_START,

		PS_INCLUDE_TARGET,
		PS_INCLUDE_TERMINATOR,

		PS_CLASS_1,
		PS_CLASS_2,

		PS_FUN_1,
		PS_FUN_2,

		PS_PLAIN_STATEMENT_START
	);

	type Parse_Data_Code is (
		PD_VOID,
		PD_STATEMENT,
		PD_ASSIGNMENT
	);

	type Parse_Data(Code: Parse_Data_Code := PD_VOID) is record
		case Code is
			when PD_VOID =>
				null;
			when PD_STATEMENT =>
				Stmt_Starter: S.Elastic_String;
			when PD_ASSIGNMENT =>
				Assign_Starter: S.Elastic_String;
		end case;
	end record;

	type Parse_State is record
		Current: Parse_State_Code := PS_START;
		Data: Parse_Data;
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
		F: Feeder.Feed;

		Tk: Token;
		Prs: Parse_State_Stack(128);  -- TODO: make this dynamic. single access type. dynamic allocation
		Inc: Include_Stack(32); -- TODO: make this dynamic. single access type. dynamic allocation

	end record;

end H3.Compilers;
