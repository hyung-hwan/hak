with H3.Utf8;

package body H3.Compilers is
	type Char_Array is array(System_Index range<>) of Standard.Character;
	package Utf8 is new H3.Utf8(Standard.Character, S.Rune, Char_Array, S.Rune_Array);

	LB_EOF: constant S.Rune_Array := (R.V.Left_Arrow,R.V.UC_E,R.V.UC_O,R.V.UC_F,R.V.Right_Arrow); -- <EOF>
	LB_XINCLUDE: constant S.Rune_Array := (R.V.Number_Sign,R.V.LC_I,R.V.LC_N,R.V.LC_C,R.V.LC_L,R.V.LC_U,R.V.LC_D,R.V.LC_E); -- #include


	procedure Dump_Token (Tk: in Token) is
	begin
		Ada.Text_IO.Put (Tk.Id'Img);
		Ada.Text_IO.Put (": ");
		Ada.Text_IO.Put_Line (Standard.String(Utf8.From_Unicode_String(Tk.Buf.To_Rune_Array)));
	end Dump_Token;

	procedure Dump_Rune (Code: in R.Code) is
	begin
		if R.Is_Eof(Code) then
			Ada.Text_IO.Put_Line ("EOF");
		else
			Ada.Text_IO.Put_Line (R.To_Rune(Code)'Img);
		end if;
	end Dump_Rune;

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
		End_Token (C, Id, R.To_Rune(Code));
	end End_Token;

	-- -------------------------------------------------------------------

	procedure Set_Parse_State (C: in out Compiler; Code: in Parse_State_Code) is
	begin
		C.Prs.States(C.Prs.Top).Current := Code;
	end Set_Parse_State;

	procedure Push_Parse_State (C: in out Compiler; Code: in Parse_State_Code) is
		Top: System_Index;
	begin
		if C.Prs.Top = C.Prs.States'Last then
			raise Syntax_Error with "parse state stack exhausted";
		end if;

		Top := C.Prs.Top + 1;
		declare
			S: Parse_State renames C.Prs.States(Top);
		begin
			S.Current := Code;
		end;
		C.Prs.Top := Top;
	end Push_Parse_State;

	procedure Pop_Parse_State (C: in out Compiler) is
	begin
		C.Prs.top := C.Prs.Top - 1;
	end Pop_Parse_State;

	-- -------------------------------------------------------------------

	procedure Push_Inclusion (C: in out Compiler; Name: in S.Rune_Array) is
		Top: System_Index;
	begin
		if C.Inc.Top = C.Inc.Streams'Last then
			raise Syntax_Error with "inclusion depth too deep";
		end if;

		Top := C.Inc.Top + 1;
		declare
			S: Stream renames C.Inc.Streams(Top);
		begin
			Ada.Text_IO.Open (S.Handle, Ada.Text_IO.In_File, Standard.String(Utf8.From_Unicode_String(Name)));
			S.Prs_Level := C.Prs.Top; -- this is the parse state level of this include directive.
		end;
		C.Inc.Top := Top;

		-- Switch the parse state to handle the terminator
		-- after the new pushed state has been popped out.
		Set_Parse_State (C, PS_INCLUDE_TERMINATOR);

		-- Let the inner content be handled at the state as the include directive is seen.
		Push_Parse_State (C, C.Prs.States(C.Prs.Top - 1).Current);
	end Push_Inclusion;

	procedure Pop_Inclusion (C: in out Compiler; Check: Boolean) is
	begin
		Ada.Text_IO.Close (C.Inc.Streams(C.Inc.Top).Handle);
		if Check then
			if C.Prs.Top /= C.Inc.Streams(C.Inc.Top).Prs_Level + 1 then
				raise Syntax_Error with "unblanced inclusion content";
			end if;
		end if;
		C.Inc.Top := C.Inc.Top - 1;
		Pop_Parse_State (C);
	end Pop_Inclusion;

	-- -------------------------------------------------------------------

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
					--Set_Parse_State (C, PS_INCLUDE_TARGET);
					Push_Parse_State (C, PS_INCLUDE_TARGET);
				else
					raise Syntax_Error with "unknown directive name";
				end if;

			when TK_EOF =>
				if C.Inc.Top > 0 then
					Pop_Inclusion (C, True);
				else
					-- end of really the input??
					null;
				end if;

			when TK_EOL =>
				null;

			when TK_IDENT =>
				null;

			--when TK_NUMBER =>
			--	null;

			--when TK_CLASS =>
			--when TK_FUNC =>

			-- plus or minus signed may be allowed here too.
			-- plusplus or miniusminus may be allowed here too.

			when TK_SEMICOLON =>
				null;

			when others =>
				raise Syntax_Error with "unexpected token";

		end case;
	end Parse_Start;

	procedure Parse_Include_Target (C: in out Compiler) is
	begin
		if C.Tk.Id = TK_CSTR then
			-- arrange to feed more data from the included file.
			Push_Inclusion (C, S.To_Rune_Array(C.Tk.Buf));
		else
			-- the target is not a string.
			--Dump_Token (C.Tk);
			raise Syntax_Error with "string literal required";
		end if;
	end Parse_Include_Target;

	procedure Parse_Include_Terminator (C: in out Compiler) is
	begin
		if C.Tk.Id /= TK_SEMICOLON then
			raise Syntax_Error with "semicolon required";
		end if;
		Pop_Parse_State (C);
	end Parse_Include_Terminator;

	procedure Got_Token (C: in out Compiler) is
	begin
		Dump_Token (C.Tk);

		case C.Prs.States(C.Prs.Top).Current is
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
		--Dump_Rune (Code);

		case C.Lx.State is
			when LX_START =>
				if R.Is_Eof(Code) then
					Start_Token (C, LB_EOF);
					End_Token (C, TK_EOF);
					-- this procedure doesn't prevent you from feeding more runes
					-- after EOF. but it's not desirable to feed more after EOF.
				elsif R.Is_Space(Code) then
					-- ignore. carry on
					null;
				elsif R.Is_Alpha(Code) then
					Set_Lexer_State (C, LX_IDENT, Code);
				elsif R.Is_Digit(Code) then
					Set_Lexer_State (C, LX_NUMBER, Code);
				elsif R.Is_Rune(Code, R.V.Plus_Sign) then -- +
					Set_Lexer_State (C, LX_OP_PLUS, Code);
				elsif R.Is_Rune(Code, R.V.Minus_Sign) then -- -
					Set_Lexer_State (C, LX_OP_MINUS, Code);
				elsif R.Is_Rune(Code, R.V.Asterisk) then -- *
					Set_Lexer_State (C, LX_OP_MUL, Code);
				elsif R.Is_Rune(Code, R.V.Slash) then -- /
					Set_Lexer_State (C, LX_OP_DIV, Code);
				elsif R.Is_Rune(Code, R.V.Left_Arrow) then -- <
					Set_Lexer_State (C, LX_OP_LESS, Code);
				elsif R.Is_Rune(Code, R.V.Right_Arrow) then -- >
					Set_Lexer_State (C, LX_OP_GREATER, Code);
				elsif R.Is_Rune(Code, R.V.Number_Sign) then -- #
					Set_Lexer_State (C, LX_DIRECTIVE, Code);
				elsif R.Is_Rune(Code, R.V.Quotation) then -- "
					Set_Lexer_State (C, LX_CSTR);
				elsif R.Is_Rune(Code, R.V.Semicolon) then
					Start_Token (C, Code);
					End_Token (C, TK_SEMICOLON);
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

			when LX_OP_PLUS =>
				if R.Is_Rune(Code, R.V.Plus_Sign) then
					End_Token (C, TK_PLUSPLUS, Code);
				else
					End_Token (C, TK_PLUS);
					goto Start_Over;
				end if;

			when LX_OP_MINUS =>
				if R.Is_Rune(Code, R.V.Minus_Sign) then
					End_Token (C, TK_MINUSMINUS, Code);
				else
					End_Token (C, TK_MINUS);
					goto Start_Over;
				end if;

			when LX_OP_MUL =>
				if R.Is_Rune(Code, R.V.Asterisk) then
					End_Token (C, TK_MULMUL, Code);
				else
					End_Token (C, TK_MUL);
					goto Start_Over;
				end if;

			when LX_OP_DIV =>
				if R.Is_Rune(Code, R.V.Slash) then
					End_Token (C, TK_DIVDIV, Code);
				else
					End_Token (C, TK_DIV);
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
				-- Pop_Inclusion() is called on EOF which is fed by End_Feed().
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


	-- -------------------------------------------------------------------
	procedure Initialize (C: in out Compiler) is
	begin
		Push_Parse_State (C, PS_START);
	end Initialize;

	procedure Finalize (C: in out Compiler) is
	begin
		while C.Inc.Top > 0 loop
			Pop_Inclusion (C, False);
		end loop;
		while C.Prs.Top > 0 loop
			Pop_Parse_State (C);
		end loop;
	end Finalize;
end H3.Compilers;
