with H3.Utf8;

package body H3.Compilers is
	type Char_Array is array(System_Index range<>) of Standard.Character;
	package Utf8 is new H3.Utf8(Standard.Character, S.Rune, Char_Array, S.Rune_Array);

	LB_EOF: constant S.Rune_Array := (R.V.Left_Arrow,R.V.UC_E,R.V.UC_O,R.V.UC_F,R.V.Right_Arrow); -- <EOF>
	LB_XINCLUDE: constant S.Rune_Array := (R.V.Number_Sign,R.V.LC_I,R.V.LC_N,R.V.LC_C,R.V.LC_L,R.V.LC_U,R.V.LC_D,R.V.LC_E); -- #include

	-- -------------------------------------------------------------------

	procedure Start_Token (C: in out Compiler) is
	begin
		C.Tk.Id := TK_EOF; -- indicate the token id is not set yet
		-- TODO: store token location.
		S.Clear (C.Tk.Buf);
	end Start_Token;

	procedure Start_Token (C: in out Compiler; Ch: in R.Rune) is
	begin
		Start_Token (C);
		S.Append (C.Tk.Buf, Ch);
	end Start_Token;

	procedure Start_Token (C: in out Compiler; Code: in R.Code) is
	begin
		Start_Token (C, R.To_Rune(Code));
	end Start_Token;

	procedure Start_Token (C: in out Compiler; Str: in S.Rune_Array) is
	begin
		Start_Token (C);
		S.Append (C.Tk.Buf, Str);
	end Start_Token;

	procedure Feed_Token (C: in out Compiler; Ch: in R.Rune) is
	begin
		S.Append (C.Tk.Buf, Ch);
	end Feed_Token;

	procedure Feed_Token (C: in out Compiler; Code: in R.Code) is
	begin
		Feed_Token(C, R.To_Rune(Code));
	end Feed_Token;

	procedure Set_Lexer_State (C: in out Compiler; State: in Lexer_State) is
	begin
		C.Lx.State := State;
		Start_Token (C); -- empty the token buffer
	end Set_Lexer_State;

	procedure Switch_Lexer_State (C: in out Compiler; State: in Lexer_State) is
	begin
		C.Lx.State := State;
		-- don't reset the token buffer;
	end Switch_Lexer_State;

	procedure Set_Lexer_State (C: in out Compiler; State: in Lexer_State; Ch: in R.Rune) is
	begin
		-- change the lexer state while storing the first character in the token buffer.
		C.Lx.State := State;
		Start_Token (C, Ch);
	end Set_Lexer_State;

	procedure Set_Lexer_State (C: in out Compiler; State: in Lexer_State; Code: in R.Code) is
	begin
		Set_Lexer_State (C, State, R.To_Rune(Code));
	end Set_Lexer_State;

	procedure Got_Token (C: in out Compiler); -- defined further down

	procedure End_Token (C: in out Compiler; Id: in Token_Id) is
	begin
		C.Tk.Id := Id;
		Got_Token (C);
		Set_Lexer_State (C, LX_START);
	end End_Token;

	procedure End_Token (C: in out Compiler; Id: in Token_Id; Ch: in R.Rune) is
	begin
		S.Append (C.Tk.Buf, Ch);
		C.Tk.Id := Id;
		Got_Token (C);
		Set_Lexer_State (C, LX_START);
	end End_Token;

	procedure End_Token (C: in out Compiler; Id: in Token_Id; Code: in R.Code) is
	begin
		S.Append (C.Tk.Buf, R.To_Rune(Code));
		C.Tk.Id := Id;
		Got_Token (C);
		Set_Lexer_State (C, LX_START);
	end End_Token;

	procedure Dump_Token (Tk: in Token) is
	begin
		Ada.Text_IO.Put (Tk.Id'Img);
		Ada.Text_IO.Put (": ");
		Ada.Text_IO.Put_Line (Standard.String(Utf8.From_Unicode_String(Tk.Buf.To_Rune_Array)));
	end Dump_Token;
	-- -------------------------------------------------------------------

	procedure Set_Parser_State (C: in out Compiler; State: in Parser_State) is
	begin
		C.Ps.Prev_State := C.Ps.State;
		C.Ps.State := State;
	end Set_Parser_State;

	procedure Start_Inclusion (C: in out Compiler; Name: in S.Rune_Array) is
		Top: System_Index;
	begin
		if C.Inc.Top = C.Inc.Streams'Last then
			raise Syntax_Error with "inclusion depth too deep";
		end if;

		Top := C.Inc.Top + 1;
		declare 
			St: Stream renames C.Inc.Streams(Top);
		begin
			Ada.Text_IO.Open (St.Handle, Ada.Text_IO.In_File, Standard.String(Utf8.From_Unicode_String(Name)));
			St.Initial_Level := C.Ps.Level;
			St.Initial_Parser_State := C.Ps.Prev_State;
			St.Next_Parser_State := PS_INCLUDE_TERMINATOR;
		end;
		C.Inc.Top := Top;

		-- the parser should resume at the state when the include directive is seen
		Set_Parser_State (C, C.Ps.Prev_State); -- the state when the include directive is seen
	end Start_Inclusion;


	procedure End_Inclusion (C: in out Compiler) is
		Top: constant System_Index := C.Inc.Top;
	begin
		if C.Ps.State /= C.Inc.Streams(Top).Initial_Parser_State or else C.Ps.Level /= C.Inc.Streams(Top).Initial_Level then
			raise Syntax_Error with "unexpected end of inclusion";
		end if;
		Ada.Text_IO.Close (C.Inc.Streams(C.Inc.Top).Handle);
		Set_Parser_State (C, C.Inc.Streams(C.Inc.Top).Next_Parser_State);
		C.Inc.Top := C.Inc.Top - 1;
	end End_Inclusion;

	procedure Parse_Start (C: in out Compiler) is
	begin
		case C.Tk.Id is
			when TK_BSTR =>
				null;
			when TK_BYTE =>
				null;
			when TK_CHAR =>
				null;
			when TK_CSTR =>
				null;
			when TK_DIRECTIVE =>
				if C.Tk.Buf.Equals(LB_XINCLUDE) then
					Set_Parser_State (C, PS_INCLUDE_TARGET);
				else
					raise Syntax_Error with "unknown directive name";
				end if;
			when TK_EOF =>
				if C.Inc.Top > 0 then
					End_Inclusion (C);
				else
					-- end of really the input??
					null;
				end if;
			when TK_EOL =>
				null;
			when TK_GE =>
				null;
			when TK_GT =>
				null;
			when TK_IDENT =>
				null;
			when TK_LE =>
				null;
			when TK_LT =>
				null;
			when TK_SEMICOLON =>
				null;
		end case;
	end Parse_Start;

	procedure Parse_Include_Target (C: in out Compiler) is
	begin
		if C.Tk.Id = TK_CSTR then
			-- arrange to feed more data from the included file.
			Start_Inclusion (C, S.To_Rune_Array(C.Tk.Buf));
		else
			-- the target is not a string.
			Dump_Token (C.Tk);
			raise Syntax_Error with "string literal required";
		end if;
	end Parse_Include_Target;

	procedure Parse_Include_Terminator (C: in out Compiler) is
	begin
		if C.Tk.Id /= TK_SEMICOLON then
			raise Syntax_Error with "semicolon required";
		end if;

		-- it is not safe to access information at the previous stack top.
		-- no problem in doing that becuase the current implementation uses 
		-- a static array. 
		Set_Parser_State (C, C.Inc.Streams(C.Inc.Top + 1).Initial_Parser_State);
	end Parse_Include_Terminator;

	procedure Got_Token (C: in out Compiler) is
	begin
ada.text_io.put (C.Tk.Id'Img);
ada.text_io.put (" ");
for i in C.Tk.Buf.Get_First_Index .. C.Tk.Buf.Get_Last_Index loop
	ada.text_io.put (Standard.Character'val(S.Rune'Pos(C.Tk.Buf.Get_Item(i))));
end loop;
ada.text_io.put_line("");

		case C.Ps.State is
			when PS_START =>
				Parse_Start (C);
			when PS_INCLUDE_TARGET =>
				Parse_Include_Target (C);
			when PS_INCLUDE_TERMINATOR =>
				Parse_Include_Terminator (C);
			when others =>
				raise Syntax_Error with "unknown parser state"; -- TODO: change this...
		end case;

	end Got_Token;

	procedure Feed_Char_Code (C: in out Compiler; Code: in R.Code) is
	begin
	<<Start_Over>>
if R.Is_Eof(Code) then
	ada.text_io.put_line ("EOF");
else
	ada.text_io.put_line (R.To_Rune(Code)'Img);
end if;
		case C.Lx.State is
			when LX_START =>
				if R.Is_Eof(Code) then
					Start_Token (C, LB_EOF);
					End_Token (C, TK_EOF);
					-- this procedure doesn't prevent you from feeding more
					-- after EOF. but it's not desirable to feed more after EOF.
				elsif R.Is_Space(Code) then
					-- ignore. carry on
					null;
				elsif R.Is_Alpha(Code) then
					Set_Lexer_State (C, LX_IDENT, Code);
				elsif R.Is_Digit(Code) then
					Set_Lexer_State (C, LX_NUMBER, Code);
				elsif R.Is_Rune(Code, R.V.Semicolon) then
					Start_Token (C, Code);
					End_Token (C, TK_SEMICOLON);
				elsif R.Is_Rune(Code, R.V.Left_Arrow) then
					Set_Lexer_State (C, LX_OP_LESS, Code);
				elsif R.Is_Rune(Code, R.V.Right_Arrow) then
					Set_Lexer_State (C, LX_OP_GREATER, Code);
				elsif R.Is_Rune(Code, R.V.Number_Sign) then
					Set_Lexer_State (C, LX_DIRECTIVE, Code);
				elsif R.Is_Rune(Code, R.V.Quotation) then -- double quote
					Set_Lexer_State (C, LX_CSTR);
				else
					raise Syntax_Error;
				end if;

			when LX_DIRECTIVE =>
				if R.Is_Alnum(Code) or else R.Is_Rune(Code, R.V.Underline) then
					Feed_Token (C, Code);
				else
					End_Token (C, TK_DIRECTIVE);
					goto Start_Over;
				end if;

			when LX_COMMENT =>
				null;

			when LX_CSTR =>
				-- TODO: escaping...
				if R.Is_Rune(Code, R.V.Quotation) then
					End_Token (C, TK_CSTR);
				else
					Feed_Token (C, Code);
				end if;

			when LX_IDENT =>
				if R.Is_Alnum(Code) or else R.Is_Rune(Code, R.V.Underline) then
					Feed_Token (C, Code);
				else
					End_Token (C, TK_IDENT);
					goto Start_Over;
				end if;

			when LX_NUMBER =>
				if R.Is_Digit(Code) then
					Feed_Token (C, Code);
				else
					End_Token (C, TK_IDENT); -- TODO: change this
					goto Start_Over;
				end if;

			when LX_OP_GREATER =>
				if R.Is_Rune(Code, R.V.Equal_Sign) then
					End_Token (C, TK_GE, Code);
				else
					End_Token (C, TK_GT);
					goto Start_Over;
				end if;

			when LX_OP_LESS =>
				if R.Is_Rune(Code, R.V.Equal_sign) then
					End_Token (C, TK_LE, Code);
				else
					End_Token (C, TK_LT);
					goto Start_Over;
				end if;

		end case;
	end Feed_Char_Code;

	procedure Feed_Inc (C: in out Compiler) is
		Entry_Top: constant System_Index := C.Inc.Top;
	begin
		loop
			while not Ada.Text_IO.End_Of_File(C.Inc.Streams(C.Inc.Top).Handle) loop
				declare
					Ch: Standard.Character;
				begin
					Ada.Text_IO.Get (C.Inc.Streams(C.Inc.Top).Handle, Ch);
					Feed_Char_Code (C, Standard.Character'Pos(Ch));
				end;
				-- After each feed, C.Inc.Top may get incremented if an inclusion
				-- directive is found. so the while loop iterates over the streams
				-- of all inner included levels. End_Feed() below drops C.Inc.Top
				-- and the outer loop will resume the inner while loop at the outer
				-- inclusion level until all entered inclusion levels are exited.
			end loop;
			End_Feed (C);

			if C.Inc.Top < Entry_Top then
				-- End_Inclusion() is called on EOF which is fed by End_Feed().
				-- It also decrements the stack pointer. The current inclusion
				-- stack pointer will get less that First_Top if the first inclusion
				-- level entered is exited.
				exit;
			end if;
		end loop;
	end Feed_Inc;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array) is
	begin
		for i in Data'Range loop
			Feed_Char_Code (C, R.To_Code(Data(i)));

			if C.Inc.Top > 0 then
				Feed_Inc (C);
			end if;
		end loop;
	end Feed;

	procedure End_Feed (C: in out Compiler) is
	begin
		Feed_Char_Code (C, R.P.EOF);
	end End_Feed;

end H3.Compilers;
