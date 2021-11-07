with H3.Utf8;
with ada.text_io;

package body H3.Compilers is
	type Char_Array is array(System_Index range<>) of Standard.Character;
	package Utf8 is new H3.Utf8(Standard.Character, S.Rune, Char_Array, S.Rune_Array);

	LB_EOF: constant S.Rune_Array := (R.V.Left_Arrow,R.V.UC_E,R.V.UC_O,R.V.UC_F,R.V.Right_Arrow); -- <EOF>
	LB_XINCLUDE: constant S.Rune_Array := (R.V.Number_Sign,R.V.LC_I,R.V.LC_N,R.V.LC_C,R.V.LC_L,R.V.LC_U,R.V.LC_D,R.V.LC_E); -- #include

	procedure Set_Lexer_State (C: in out Compiler; State: in Lexer_State) is
	begin
		C.Lx.State := State;
	end Set_Lexer_State;

	procedure Set_Lexer_State (C: in out Compiler; State: in Lexer_State; Ch: in R.Rune) is
	begin
		-- change the lexer state while storing the first character in the token buffer.
		C.Lx.State := State;
		S.Clear (C.Tk.Buf);
		S.Append (C.Tk.Buf, Ch);
	end Set_Lexer_State;

	procedure Set_Lexer_State (C: in out Compiler; State: in Lexer_State; Code: in R.Code) is
	begin
		Set_Lexer_State (C, State, R.To_Rune(Code));
	end Set_Lexer_State;

	procedure Set_Parser_State (C: in out Compiler; State: in Parser_State) is
	begin
		C.Ps.State := State;
	end Set_Parser_State;

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
					Set_Parser_State (C, PS_INCLUDE);
				else
					raise Syntax_Error;
				end if;
			when TK_EOF =>
				null;
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

	procedure Start_Inclusion (C: in out Compiler; Name: in S.Rune_Array) is
		Top: System_Index;
	begin
		if C.St.Top = C.St.Items'Last then
			raise Syntax_Error; -- TODO: inclusion depth too deep
		end if;

		Top := C.St.Top + 1;
		Ada.Text_IO.Open (C.St.Items(Top).Handle, Ada.Text_IO.In_File, Standard.String(Utf8.From_Unicode_String(Name)));
		C.St.Top := Top;
	end Start_Inclusion;

	procedure End_Inclusion (C: in out Compiler) is
	begin
		Ada.Text_IO.Close (C.St.Items(C.St.Top).Handle);
		C.St.Top := C.St.Top - 1;
	end End_Inclusion;

	procedure Parse_Include (C: in out Compiler) is
	begin
		if C.Tk.Id = TK_CSTR then
			-- arrange to feed more data from the included file.
			Start_Inclusion (C, S.To_Rune_Array(C.Tk.Buf));
			null;
		else
			raise Syntax_Error; -- string literal required
		end if;
	end Parse_Include;

	procedure Parse_Include_End (C: in out Compiler) is
	begin
		if C.Tk.Id /= TK_SEMICOLON then
			raise Syntax_Error;
		end if;

		-- TODO: put the state back to START???
	end Parse_Include_End;

	procedure Got_Token (C: in out Compiler) is
	begin
		--case C.P.State IS
		--	when START =>
		--		null;
		--end case;

ada.text_io.put (C.Tk.Id'Img);
ada.text_io.put (" ");
for i in C.Tk.Buf.Get_First_Index .. C.Tk.Buf.Get_Last_Index loop
	ada.text_io.put (standard.character'val(S.Rune'Pos(C.Tk.Buf.Get_Item(i))));
end loop;
ada.text_io.put_line("");

		case C.Ps.State is
			when PS_START => 
				Parse_Start (C);
			when PS_INCLUDE =>
				Parse_Include (C);
			when others =>
				raise Syntax_Error; -- TODO: change this...
		end case;

	end Got_Token;

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
					Set_Lexer_State (C, LX_DIRECTIVE);
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

			when LX_COMMENT =>
				null;

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
		end case;
	end Feed_Char_Code;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array) is
	begin
		for i in Data'Range loop
			Feed_Char_Code (C, R.To_Code(Data(i)));

			if C.St.Top > 0 then
				declare
					Ch: Standard.Character;
				begin		
					while not Ada.Text_IO.End_Of_File(C.St.Items(C.St.Top).Handle) loop
						Ada.Text_IO.Get (C.St.Items(C.St.Top).Handle, Ch);
						Feed_Char_Code (C, Standard.Character'Pos(Ch));
						--if inclusion stack is not Empty???
					end loop;
				end;
			end if;
		end loop;
	end Feed;

	procedure End_Feed (C: in out Compiler) is
	begin
		Feed_Char_Code (C, R.P.EOF);
	end End_Feed;

end H3.Compilers;
