with ada.text_io;

package body H3.Compilers is
	LB_EOF: constant S.Rune_Array := (R.V.Left_Arrow,R.V.UC_E,R.V.UC_O,R.V.UC_F,R.V.Right_Arrow); -- <EOF>

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
				--Push_Feed_Layer (...
				null;
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
		end loop;
	end Feed;

	procedure End_Feed (C: in out Compiler) is
	begin
		Feed_Char_Code (C, R.P.EOF);
	end End_Feed;

end H3.Compilers;
