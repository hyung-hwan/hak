with H3.Utf8;

package body H3.Compilers is
	type Char_Array is array(System_Index range<>) of Standard.Character;
	package Utf8 is new H3.Utf8(Standard.Character, S.Rune, Char_Array, S.Rune_Array);

	LB_EOF: constant S.Rune_Array := (R.V.Left_Arrow,R.V.UC_E,R.V.UC_O,R.V.UC_F,R.V.Right_Arrow); -- <EOF>
	LB_EOL: constant S.Rune_Array := (R.V.Left_Arrow,R.V.UC_E,R.V.UC_O,R.V.UC_L,R.V.Right_Arrow); -- <EOL>
	LB_XINCLUDE: constant S.Rune_Array := (R.V.Number_Sign,R.V.LC_I,R.V.LC_N,R.V.LC_C,R.V.LC_L,R.V.LC_U,R.V.LC_D,R.V.LC_E); -- #include
	LB_CLASS: constant S.Rune_Array := (R.V.LC_C,R.V.LC_L,R.V.LC_A,R.V.LC_S,R.V.LC_S); -- class
	LB_FUN: constant S.Rune_Array := (R.V.LC_F,R.V.LC_U,R.V.LC_N); -- fun
	LB_END: constant S.Rune_Array := (R.V.LC_E,R.V.LC_N,R.V.LC_D); -- end
	LB_IF: constant S.Rune_Array := (R.V.LC_I,R.V.LC_F); -- if
	LB_ELIF: constant S.Rune_Array := (R.V.LC_E,R.V.LC_L,R.V.LC_I,R.V.LC_F); -- elif
	LB_ELSE: constant S.Rune_Array := (R.V.LC_E,R.V.LC_L,R.V.LC_S,R.V.LC_E); -- else
	LB_WHILE: constant S.Rune_Array := (R.V.LC_W,R.V.LC_H,R.V.LC_I,R.V.LC_L,R.V.LC_E); -- while
	LB_BREAK: constant S.Rune_Array := (R.V.LC_B,R.V.LC_R,R.V.LC_E,R.V.LC_A,R.V.LC_K); -- break
	LB_CONTINUE: constant S.Rune_Array := (R.V.LC_C,R.V.LC_O,R.V.LC_N,R.V.LC_T,R.V.LC_I,R.V.LC_N,R.V.LC_U,R.V.LC_E); -- continue
	LB_TRY: constant S.Rune_Array := (R.V.LC_T,R.V.LC_R,R.V.LC_Y); -- try
	LB_CATCH: constant S.Rune_Array := (R.V.LC_C,R.V.LC_A,R.V.LC_T,R.V.LC_T,R.V.LC_H); -- catch
	LB_RAISE: constant S.Rune_Array := (R.V.LC_R,R.V.LC_A,R.V.LC_I,R.V.LC_S,R.V.LC_E); -- raise

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

	function Is_Line_Break(Code: in R.Code) return Boolean is
	begin
		return R.Is_Rune(Code, R.V.LF); -- TODO: consider different line end convention
	end Is_Line_Break;

	function Is_Ident_Starter(Code: in R.Code) return Boolean is
	begin
		return R.Is_Alnum(Code) or else
		       R.Is_Rune(Code, R.V.Underline) or else
		       R.Is_Rune(Code, R.V.Minus_Sign);
	end Is_Ident_Starter;

	function Is_Ident_Char(Code: in R.Code) return Boolean is
	begin
		return Is_Ident_Starter(Code); -- or else R.Is_Rune(Code, R.V.Underline); -- or else R.Is_Rune(C, ...);
	end Is_Ident_Char;

	-- -------------------------------------------------------------------

	package body Feeder is
		procedure Switch_To_Start (C: in out Compiler) is
		begin
			C.F.Lx.Data := Lex_Data'(State => LX_START);
		end Switch_To_Start;

		procedure Start (C: in out Compiler; Code: in R.Code; Consumed: out Boolean) is
		begin
			null;
		end Start;

		procedure Comment (C: in out Compiler; Code: in R.Code; Consumed: out Boolean) is
		begin
			if Is_Line_Break(Code) then
				Switch_To_Start (C);
			end if;
		end Comment;

		procedure Delim_Token (C: in out Compiler; Code: in R.Code; Consumed: out Boolean) is
		begin
			null;
		end Delim_Token;

		procedure Hmarked_Token (C: in out Compiler; Code: in R.Code; Consumed: out Boolean) is
		begin
			null;
		end Hmarked_Token;

		procedure Update_Location (C: in out Compiler; Code: in R.Code) is
		begin
			if Is_Line_Break(Code) then
				C.F.Lx.Loc.Line := C.F.Lx.Loc.Line + 1;
				C.F.Lx.Loc.Colm := 1;
			else
				C.F.Lx.Loc.Colm := C.F.Lx.Loc.Colm + 1;
			end if;
		end Update_Location;

		procedure Begin_Include (C: in out Compiler) is
		begin
			null;
		end Begin_Include;

		procedure Feed_From_Includee (C: in out Compiler) is
		begin
			null;
		end Feed_From_Includee;
	end Feeder;


	-- -------------------------------------------------------------------

	procedure Feed_Char_Code (C: in out Compiler; Code: in R.Code; Consumed: out Boolean) is
	begin
		case C.F.Lx.Data.State is
			when Feeder.LX_START =>
				Feeder.Start(C, Code, Consumed);
			when Feeder.LX_COMMENT =>
				Feeder.Comment(C, Code, Consumed);
			when Feeder.LX_DT =>
				Feeder.Delim_Token(C, Code, Consumed);
			when Feeder.LX_HC =>
				Feeder.Hmarked_Token(C, Code, Consumed);
		end case;

		--raise Internal_Error with "internal error - unknown flx state";
	end Feed_Char_Code;

	procedure Feed (C: in out Compiler; Data: in S.Rune_Array) is
		Consumed: Boolean;
		CC: R.Code;
		I: System_Index := Data'First;
	begin
		while I <= Data'Last loop
			CC := R.To_Code(Data(I));
			Feed_Char_Code (C, CC, consumed);
			if Consumed then
				Feeder.Update_Location (C, CC);
				I := I + 1;
			end if;

			if C.F.Rd.Do_Include_File then
				Feeder.Begin_Include (C);
				C.F.Rd.Do_Include_File := False;
			end if;

			--if C->Cur then
			--	Feeder.Feed_From_Includee (C);
			--end if;
		end loop;
	end Feed;

	procedure End_Feed (C: in out Compiler) is
		Consumed: Boolean;
	begin
		begin
			loop
				Feed_Char_Code(C, R.P.EOF, Consumed);
				exit when Consumed;
			end loop;
		exception
			when others =>
				--if C.Feed.Rd.Level <= 0 and then C.Get_Error_Number = HCL_ERR_SYNTAX and then C.Get_Syntax_Error_Number = HCL_SYNERR_EOF then
				--	null; -- normal EOF
				--else
					raise;
				--end if;
		end;
	end End_Feed;

	-- -------------------------------------------------------------------
	procedure Initialize (C: in out Compiler) is
	begin
		--Push_Parse_State (C, PS_START);

		declare
			X: H3.Trees.Tree;
		begin
			H3.Trees.New_Node (X);
		end;
	end Initialize;

	procedure Finalize (C: in out Compiler) is
	begin
		--while C.Inc.Top > 0 loop
		--	Pop_Inclusion (C, False);
		--end loop;
		--while C.Prs.Top > 0 loop
		--	Pop_Parse_State (C);
		--end loop;
		null;
	end Finalize;

end H3.Compilers;
