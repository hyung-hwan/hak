/*
    Copyright (c) 2016-2018 Chung, Hyung-Hwan. All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
    IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "hak-prv.h"
#include <stdio.h>
#define HAK_LANG_ENABLE_WIDE_DELIM
#define HAK_LANG_AUTO_FORGE_XLIST_ALWAYS

#define BUFFER_ALIGN 128
#define BALIT_BUFFER_ALIGN 128
#define SALIT_BUFFER_ALIGN 128
#define ARLIT_BUFFER_ALIGN 128

static struct voca_t
{
	hak_oow_t len;
	hak_ooch_t str[11];
} vocas[] =
{
/* TODO: change $include and $pragma to #\include or #\pragma or use some other prefix like #^ -> $ to use really for variable reference or something...
 * TODO: change #\ to something else... */
	{  8, { '$','i','n','c','l','u','d','e'                               } },
	{  7, { '$','p','r','a','g','m','a'                                   } },

	{ 11, { '#','\\','b','a','c','k','s','p','a','c','e'                  } },
	{ 10, { '#','\\','l','i','n','e','f','e','e','d'                      } },
	{  9, { '#','\\','n','e','w','l','i','n','e'                          } },
	{  5, { '#','\\','n','u','l'                                          } },
	{  6, { '#','\\','p','a','g','e'                                      } },
	{  8, { '#','\\','r','e','t','u','r','n'                              } },
	{  8, { '#','\\','r','u','b','o','u','t'                              } },
	{  7, { '#','\\','s','p','a','c','e'                                  } },
	{  5, { '#','\\','t','a','b'                                          } },
	{  6, { '#','\\','v','t','a','b'                                      } },

	{  3, { 'n','i','l'                                                   } },
	{  4, { 't','r','u','e'                                               } },
	{  5, { 'f','a','l','s','e'                                           } },
	{  4, { 's','e','l','f'                                               } },
	{  5, { 's','u','p','e','r'                                           } },

	{  5, { 'c','l','a','s','s'                                           } },
	{  3, { 'f','u','n'                                                   } },
	{  3, { 'v','a','r'                                                   } },
	{  2, { 'd','o'                                                       } },
	{  2, { 'i','f'                                                       } },
	{  4, { 'e','l','i','f'                                               } },
	{  4, { 'e','l','s','e'                                               } },
	{  5, { 't','h','r','o','w'                                           } },
	{  3, { 't','r','y'                                                   } },
	{  5, { 'c','a','t','c','h'                                           } },
	{  5, { 'b','r','e','a','k'                                           } },
	{  8, { 'c','o','n','t','i','n','u','e'                               } },
	{  5, { 'u','n','t','i','l'                                           } },
	{  5, { 'w','h','i','l','e'                                           } },
	{  6, { 'r','e','t','u','r','n'                                       } },
	{  6, { 'r','e','v','e','r','t'                                       } },
	{  3, { 'a','n','d'                                                   } },
	{  2, { 'o','r',                                                      } },
#if defined(USE_KW_PLUS)
	{  4, { 'p','l','u','s'                                               } },
#endif
	{  3, { 's','e','t'                                                   } },
	{  5, { 's','e','t','-','r'                                           } },

	{  3, { '(',' ',')'      /* XLIST */                                  } },
	{  4, { '(',':',' ',')'  /* MLIST */                                  } },
	{  4, { '(',':','=',')'  /* ALIST - x := y */                         } },
	{  4, { '(','B','O',')'  /* BLIST - x binop y */                      } },
	{  3, { '{',' ','}'      /* BLOCK */                                  } },
	{  4, { '#','[',' ',']'  /* ARRAY */                                  } },
	{  5, { '#','b','[',' ',']' /* BYTE ARRAY */                          } },
	{  5, { '#','c','[',' ',']' /* CHAR ARRAY */                          } },
	{  4, { '#','{',' ','}' /* DICTIONARY */                              } },
	{  4, { '#','(',' ',')' /* QLIST */                                   } },
	{  3, { '[',' ',']' /* TUPLE */                                       } },
	{  3, { '|',' ','|' /* VLIST */                                       } },

	{  7, { 'l','i','b','e','r','a','l'                                   } },
	{  2, { 'o','n'                                                       } },
	{  3, { 'o','f','f'                                                   } },

	{  5, { '<','E','O','L','>'                                           } },
	{  5, { '<','E','O','F','>'                                           } }
};

enum voca_id_t
{
	VOCA_INCLUDE,
	VOCA_PRAGMA,

	VOCA_CHAR_BACKSPACE,
	VOCA_CHAR_LINEFEED,
	VOCA_CHAR_NEWLINE,
	VOCA_CHAR_NUL,
	VOCA_CHAR_PAGE,
	VOCA_CHAR_RETURN,
	VOCA_CHAR_RUBOUT,
	VOCA_CHAR_SPACE,
	VOCA_CHAR_TAB,
	VOCA_CHAR_VTAB,

	VOCA_KW_NIL,
	VOCA_KW_TRUE,
	VOCA_KW_FALSE,
	VOCA_KW_SELF,
	VOCA_KW_SUPER,

	VOCA_KW_CLASS,
	VOCA_KW_FUN,
	VOCA_KW_VAR,
	VOCA_KW_DO,
	VOCA_KW_IF,
	VOCA_KW_ELIF,
	VOCA_KW_ELSE,
	VOCA_KW_THROW,
	VOCA_KW_TRY,
	VOCA_KW_CATCH,
	VOCA_KW_BREAK,
	VOCA_KW_CONTINUE,
	VOCA_KW_UNTIL,
	VOCA_KW_WHILE,
	VOCA_KW_RETURN,
	VOCA_KW_REVERT,
	VOCA_KW_AND,
	VOCA_KW_OR,
#if defined(USE_KW_PLUS)
	VOCA_KW_PLUS,
#endif
	VOCA_KW_SET,
	VOCA_KW_SET_R,

	VOCA_XLIST,
	VOCA_MLIST,
	VOCA_ALIST, /* assignment list */
	VOCA_BLIST, /* just fake entry */
	VOCA_BLOCK,
	VOCA_ARRAY,
	VOCA_BYTEARRAY,
	VOCA_CHARARRAY,
	VOCA_DIC,
	VOCA_QLIST,
	VOCA_TUPLE,
	VOCA_VLIST,

	VOCA_PRG_LIBERAL,
	VOCA_PRG_ON,
	VOCA_PRG_OFF,

	VOCA_EOL,
	VOCA_EOF
};
typedef enum voca_id_t voca_id_t;

enum list_flag_t
{
	QUOTED       = (1 << 0),
	DOTTED       = (1 << 1),

	COMMAED      = (1 << 2),
	COLONED      = (1 << 3),
	COLONEQED    = (1 << 4),
	BINOPED      = (1 << 5),

	CLOSED       = (1 << 6),
	JSON         = (1 << 7),
	DATA_LIST    = (1 << 8),
	AUTO_FORGED  = (1 << 9),  /* automatically added list. only applicable to XLIST */
	AT_BEGINNING = (1 << 10)

	/* TOTOAL 16 items are allowed for LIST_FLAG_GET_CONCODE() and LIST_FLAG_SET_CONCODE().
	 * they reserve lower 16 bits as flag bits.*/
};

#define LIST_FLAG_GET_CONCODE(x) (((x) >> 16) & 0xFFFF)
#define LIST_FLAG_SET_CONCODE(x,type) ((x) = ((x) & ~0xFF0000) | ((type) << 16))

static struct
{
	int             closer;
	hak_synerrnum_t synerr;
	int             voca_id;
} cons_info[] =
{
	HAK_AID(HAK_CONCODE_XLIST)     { HAK_TOK_RPAREN, HAK_SYNERR_RPAREN, VOCA_XLIST }, /* XLIST     ( )  */
	HAK_AID(HAK_CONCODE_MLIST)     { HAK_TOK_RPAREN, HAK_SYNERR_RPAREN, VOCA_MLIST }, /* MLIST     (obj:message) */
	HAK_AID(HAK_CONCODE_ALIST)     { HAK_TOK_RPAREN, HAK_SYNERR_RPAREN, VOCA_ALIST }, /* ALIST     (var:=value) */
	HAK_AID(HAK_CONCODE_BLIST)     { HAK_TOK_RPAREN, HAK_SYNERR_RPAREN, VOCA_BLIST }, /* BLIST     (x + y) */
	HAK_AID(HAK_CONCODE_BLOCK)     { HAK_TOK_RBRACE, HAK_SYNERR_RBRACE, VOCA_BLOCK }, /* BLOCK     { } */
	HAK_AID(HAK_CONCODE_ARRAY)     { HAK_TOK_RBRACK, HAK_SYNERR_RBRACK, VOCA_ARRAY }, /* ARRAY     #[ ] */
	HAK_AID(HAK_CONCODE_BYTEARRAY) { HAK_TOK_RBRACK, HAK_SYNERR_RBRACK, VOCA_BYTEARRAY }, /* BYTEARRAY #b[ ] */
	HAK_AID(HAK_CONCODE_CHARARRAY) { HAK_TOK_RBRACK, HAK_SYNERR_RBRACK, VOCA_CHARARRAY }, /* CHARARRAY #c[ ] */
	HAK_AID(HAK_CONCODE_DIC)       { HAK_TOK_RBRACE, HAK_SYNERR_RBRACE, VOCA_DIC }, /* DIC       #{ } */
	HAK_AID(HAK_CONCODE_QLIST)     { HAK_TOK_RPAREN, HAK_SYNERR_RPAREN, VOCA_QLIST }, /* QLIST     #( )  */
	HAK_AID(HAK_CONCODE_TUPLE)     { HAK_TOK_RBRACK, HAK_SYNERR_RBRACK, VOCA_TUPLE }, /* TUPLE [] */

	/* VLIST's closer and synerr are not used. there is dedicated logic in feed_process_token(). only voca_id is used */
	HAK_AID(HAK_CONCODE_VLIST)     { HAK_TOK_VBAR,   HAK_SYNERR_VBAR,   VOCA_VLIST }  /* VLIST     | |  */
};

/* ----------------------------------------------------------------- */

static int init_compiler (hak_t* hak);
static void feed_continue (hak_t* hak, hak_flx_state_t state);
static int is_at_block_beginning (hak_t* hak);
static int flx_plain_ident (hak_t* hak, hak_ooci_t c);

/* ----------------------------------------------------------------- */

static HAK_INLINE int is_spacechar (hak_ooci_t c)
{
#if 0
	switch (c)
	{
		case ' ':
		case '\f': /* formfeed */
		case '\n': /* linefeed */
		case '\r': /* carriage return */
		case '\t': /* horizon tab */
		case '\v': /* vertical tab */
			return 1;

		default:
			return 0;
	}
#else
	return c != HAK_OOCI_EOF && hak_is_ooch_space(c);
#endif
}

static HAK_INLINE int is_linebreak (hak_ooci_t c)
{
	/* TODO: different line end conventions? */
	return c == '\n'; /* make sure this is one of the space chars in is_spacechar() */
}

static HAK_INLINE int is_digit_char (hak_ooci_t c)
{
	return (c >= '0' && c <= '9');
}

static HAK_INLINE int is_radixed_digit_char (hak_ooci_t c, int radix)
{
	if (c >= '0' && c <= '9') return (c - '0') < radix;
	if (c >= 'a' && c <= 'z') return (c - 'a' + 10) < radix;
	if (c >= 'A' && c <= 'Z') return (c - 'A' + 10) < radix;
	return 0;
}

static HAK_INLINE int is_xdigit_char (hak_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

#if 0
static HAK_INLINE int is_alphachar (hak_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static HAK_INLINE int is_alnumchar (hak_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
}
#endif

static HAK_INLINE int is_delim_char (hak_ooci_t c)
{
	return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' ||
	       c == '|' || c == ',' || c == '.' || c == ':' || c == ';' ||
	       /* the first characters of tokens in delim_token_tab up to this point */
#if defined(HAK_OOCH_IS_UCH) && defined(HAK_LANG_ENABLE_WIDE_DELIM)
	       c == L'\u201C' || c == L'\u201D' ||  /* “ ” */
	       c == L'\u2018' || c == L'\u2019' ||  /* ‘ ’ */
#endif
	       c == '#' || c == '\"' || c == '\'' || c == '\\' || is_spacechar(c) || c == HAK_OOCI_EOF;
}

static HAK_INLINE int is_binop_char (hak_ooci_t c)
{
	return c == '&' || c == '*' || c == '+' || c == '-' || c == '/' || c == '%' ||
	       c == '<' || c == '>' || c == '=' || c == '@' || c == '|' || c == '~';
}

static HAK_INLINE int is_pure_lead_ident_char (hak_ooci_t c)
{
	return hak_is_ooch_alpha(c) || c == '_';
}

static HAK_INLINE int is_pure_ident_char (hak_ooci_t c)
{
	/* ? is still allowed at the back. see classify_ident_token() */
	return hak_is_ooch_alnum(c) || c == '_' || c == '-';
}

static HAK_INLINE int is_lead_ident_char (hak_ooci_t c)
{
	return hak_is_ooch_alpha(c) || c == '_' || c == '-' || c == '?' || is_binop_char(c);
}

static HAK_INLINE int is_ident_char (hak_ooci_t c)
{
	return hak_is_ooch_alnum(c) || c == '_' || c == '-' || c == '?' || is_binop_char(c);
}

/* TODO: remove GET_CHAR(), GET_CHAR_TO(), get_char(), _get_char() */
#define GET_CHAR(hak) \
	do { if (get_char(hak) <= -1) return -1; } while (0)

#define GET_CHAR_TO(hak,c) \
	do { \
		if (get_char(hak) <= -1) return -1; \
		c = (hak)->c->lxc.c; \
	} while (0)


#define ADD_TOKEN_STR(hak,s,l) \
	do { if (add_token_str(hak, s, l) <= -1) return -1; } while (0)

#define ADD_TOKEN_CHAR(hak,c) \
	do { if (add_token_char(hak, c) <= -1) return -1; } while (0)

#define CLEAR_TOKEN_NAME(hak) ((hak)->c->tok.name.len = 0)
#define SET_TOKEN_TYPE(hak,tv) ((hak)->c->tok.type = (tv))
#define SET_TOKEN_LOC(hak,locv) ((hak)->c->tok.loc = *(locv))

#define TOKEN_TYPE(hak) ((hak)->c->tok.type)
#define TOKEN_NAME(hak) (&(hak)->c->tok.name)
#define TOKEN_NAME_CAPA(hak) ((hak)->c->tok.name_capa)
#define TOKEN_NAME_LEN(hak) ((hak)->c->tok.name.len)
#define TOKEN_NAME_PTR(hak) ((hak)->c->tok.name.ptr)
#define TOKEN_NAME_CHAR(hak,index) ((hak)->c->tok.name.ptr[index])
#define TOKEN_LOC(hak) (&(hak)->c->tok.loc)

static HAK_INLINE int add_token_str (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	hak_oocs_t tmp;
	tmp.ptr = (hak_ooch_t*)ptr;
	tmp.len = len;
	return hak_copy_string_to(hak, &tmp, TOKEN_NAME(hak), &TOKEN_NAME_CAPA(hak), 1, '\0');
}

static HAK_INLINE int does_token_name_match (hak_t* hak, voca_id_t id)
{
	return hak->c->tok.name.len == vocas[id].len &&
	       hak_equal_oochars(hak->c->tok.name.ptr, vocas[id].str, vocas[id].len);
}

static HAK_INLINE int add_token_char (hak_t* hak, hak_ooch_t c)
{
	hak_oocs_t tmp;

	tmp.ptr = &c;
	tmp.len = 1;
	return hak_copy_string_to(hak, &tmp, TOKEN_NAME(hak), &TOKEN_NAME_CAPA(hak), 1, '\0');
}

static HAK_INLINE void unget_char (hak_t* hak, const hak_lxc_t* c)
{
	/* Make sure that the unget buffer is large enough */
	HAK_ASSERT(hak, hak->c->nungots < HAK_COUNTOF(hak->c->ungot));
	hak->c->ungot[hak->c->nungots++] = *c;
}

static int get_directive_token_type (hak_t* hak, hak_tok_type_t* tok_type)
{
	if (does_token_name_match(hak, VOCA_INCLUDE))
	{
		*tok_type = HAK_TOK_INCLUDE;
		return 0;
	}
	else if (does_token_name_match(hak, VOCA_PRAGMA))
	{
		*tok_type = HAK_TOK_PRAGMA;
		return 0;
	}

	return -1;
}

static int _get_char (hak_t* hak, hak_io_cciarg_t* inp)
{
	hak_ooci_t lc;

	if (inp->b.pos >= inp->b.len)
	{
		if (hak->c->cci_rdr(hak, HAK_IO_READ, inp) <= -1) return -1;

		if (inp->xlen <= 0)
		{
			inp->lxc.c = HAK_OOCI_EOF;
			inp->lxc.l.line = inp->line;
			inp->lxc.l.colm = inp->colm;
			inp->lxc.l.file = inp->name;
			/* indicate that EOF has been read. lxc.c is also set to EOF. */
			return 0;
		}

		inp->b.pos = 0;
		inp->b.len = inp->xlen;
	}

	if (inp->lxc.c == '\n' || inp->lxc.c == '\r')
	{
		/* inp->lxc.c is a previous character. the new character
		 * to be read is still in the buffer (inp->buf).
		 * hak->cu->curinp->colm has been incremented when the previous
		 * character has been read. */
		if (inp->line > 1 && inp->colm == 2 && inp->nl != inp->lxc.c)
		{
			/* most likely, it's the second character in '\r\n' or '\n\r'
			 * sequence. let's not update the line and column number. */
			/*inp->colm = 1;*/
		}
		else
		{
			/* if the previous charater was a newline,
			 * increment the line counter and reset column to 1.
			 * incrementing the line number here instead of
			 * updating inp->lxc causes the line number for
			 * TOK_EOF to be the same line as the lxc newline. */
			inp->line++;
			inp->colm = 1;
			inp->nl = inp->lxc.c;
		}
	}

	lc = inp->buf.c[inp->b.pos++];

	inp->lxc.c = lc;
	inp->lxc.l.line = inp->line;
	inp->lxc.l.colm = inp->colm++;
	inp->lxc.l.file = inp->name;

	return 1; /* indicate that a normal character has been read */
}

static int get_char (hak_t* hak)
{
	int n;

	if (hak->c->nungots > 0)
	{
		/* something in the unget buffer */
		hak->c->lxc = hak->c->ungot[--hak->c->nungots];
		return 0;
	}

	n = _get_char(hak, hak->c->curinp);
	if (n >= 0) hak->c->lxc = hak->c->curinp->lxc;
	return n;
}

static int is_pure_ident (hak_t* hak, const hak_oocs_t* v)
{
	HAK_ASSERT(hak, v->len > 0); /* you must not pass the zero-length value */

	if (is_pure_lead_ident_char(v->ptr[0]))
	{
		/* check if the word conforms to pure identifier rules:
		 *  begins with alnum or _
		 *  may contains a dash or dashes in between
		 *  ends with alnum or _ or ?
		 */
		hak_oow_t i;
		hak_oow_t wc = 1;
		int q = 0;
		for (i = 1; i < v->len; i++)
		{
			if (q && v->ptr[i] != '?') goto not_ident;

			if (v->ptr[i] == '-')
			{
				/*if (wc == 0) goto not_ident;*/
				wc = 0;
			}
			else if (v->ptr[i] == '?') q = 1;
			else if (!is_pure_ident_char(v->ptr[i])) goto not_ident;
			else wc++;
		}

		if (wc > 0) return 1;
	}

not_ident:
	return 0;
}

int hak_is_binop_string (const hak_oocs_t* v)
{
	hak_oow_t i;

	if (v->len <= 0) return 0; /* you can pass the zero-length value */

	for (i = 0; i < v->len; i++)
	{
		if (!is_binop_char(v->ptr[i])) return 0;
	}
	return 1;
}

static int classify_ident_token (hak_t* hak, const hak_oocs_t* v, const hak_loc_t* errloc, hak_tok_type_t* tok_type)
{
	hak_oow_t i;
	int binop_char_count;
	static struct
	{
		int voca_id;
		hak_tok_type_t type;
	} tab[] =
	{
		{ VOCA_KW_NIL,      HAK_TOK_NIL      },
		{ VOCA_KW_TRUE,     HAK_TOK_TRUE     },
		{ VOCA_KW_FALSE,    HAK_TOK_FALSE    },
		{ VOCA_KW_SELF,     HAK_TOK_SELF     },
		{ VOCA_KW_SUPER,    HAK_TOK_SUPER    },

		{ VOCA_KW_CLASS,    HAK_TOK_CLASS    },
		{ VOCA_KW_FUN,      HAK_TOK_FUN      },
		{ VOCA_KW_VAR,      HAK_TOK_VAR      },
		{ VOCA_KW_DO,       HAK_TOK_DO       },
		{ VOCA_KW_IF,       HAK_TOK_IF       },
		{ VOCA_KW_ELIF,     HAK_TOK_ELIF     },
		{ VOCA_KW_ELSE,     HAK_TOK_ELSE     },
		{ VOCA_KW_THROW,    HAK_TOK_THROW    },
		{ VOCA_KW_TRY,      HAK_TOK_TRY      },
		{ VOCA_KW_CATCH,    HAK_TOK_CATCH    },
		{ VOCA_KW_BREAK,    HAK_TOK_BREAK    },
		{ VOCA_KW_CONTINUE, HAK_TOK_CONTINUE },
		{ VOCA_KW_UNTIL,    HAK_TOK_UNTIL    },
		{ VOCA_KW_WHILE,    HAK_TOK_WHILE    },
		{ VOCA_KW_RETURN,   HAK_TOK_RETURN   },
		{ VOCA_KW_REVERT,   HAK_TOK_REVERT   },
		{ VOCA_KW_AND,      HAK_TOK_AND      },
		{ VOCA_KW_OR,       HAK_TOK_OR       },
#if defined(USE_KW_PLUS)
		{ VOCA_KW_PLUS,     HAK_TOK_PLUS     },
#endif
		{ VOCA_KW_SET,      HAK_TOK_SET      },
		{ VOCA_KW_SET_R,    HAK_TOK_SET_R    }
	};

	for (i = 0; i < HAK_COUNTOF(tab); i++)
	{
		int vid = tab[i].voca_id;
		if (hak_comp_oochars(v->ptr, v->len, vocas[vid].str, vocas[vid].len) == 0)
		{
			*tok_type = tab[i].type;
			return 0;
		}
	}

	if (is_pure_ident(hak, v))
	{
		*tok_type = HAK_TOK_IDENT;
		return 0;
	}

	binop_char_count = 0;
	for (i = 0; i < v->len; i++)
	{
		if (is_binop_char(v->ptr[i])) binop_char_count++;
	}

	if (binop_char_count == v->len)
	{
		*tok_type = HAK_TOK_BINOP;
		return 0;
	}

	if (binop_char_count > 0 && !(hak->option.trait & HAK_TRAIT_LANG_LIBERAL))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, errloc, HAK_NULL,
			"illegal identifier '%.*js'", v->len, v->ptr);
		return -1;
	}

	/**tok_type = HAK_TOK_SYMLIT;*/
	*tok_type = HAK_TOK_STRLIT;
	return 0;
}

static int is_sr_name_in_use (hak_t* hak, const hak_ooch_t* sr_name)
{
	/* [NOTE]
	 *  this is very error prone. if there are changes in refernece
	 *  points of this sr_name in the source code, this function also
	 *  must be modifed. */
	hak_io_cciarg_t* cur;

	if (hak->c->synerr.loc.file == sr_name) return 1;

	cur = hak->c->curinp;
	while (cur)
	{
		if (cur->lxc.l.file == sr_name) return 1;
		cur = cur->includer;
	}
	return 0;
}

static void clear_sr_names (hak_t* hak)
{
	hak_link_t* cur;

	HAK_ASSERT(hak, hak->c != HAK_NULL);

	while (hak->c->sr_names)
	{
		cur = hak->c->sr_names;
		hak->c->sr_names = cur->link;
		hak_freemem(hak, cur);
	}
}

static const hak_ooch_t* add_sr_name (hak_t* hak, const hak_oocs_t* name)
{
	hak_link_t* link;
	hak_ooch_t* nptr;

	/* TODO: make search faster */
	link = hak->c->sr_names;
	while (link)
	{
		nptr = (hak_ooch_t*)(link + 1);
		if (hak_comp_oochars_oocstr(name->ptr, name->len, nptr) == 0) return nptr;
		link = link->link;
	}

	link = (hak_link_t*)hak_callocmem(hak, HAK_SIZEOF(*link) + HAK_SIZEOF(hak_ooch_t) * (name->len + 1));
	if (HAK_UNLIKELY(!link))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to source name [%.*js] - %js", name->len, name->ptr, orgmsg);
		return HAK_NULL;
	}

	nptr = (hak_ooch_t*)(link + 1);

	hak_copy_oochars(nptr, name->ptr, name->len);
	nptr[name->len] = '\0';

	link->link = hak->c->sr_names;
	hak->c->sr_names = link;

	return nptr;
}

/* -------------------------------------------------------------------------- */

static HAK_INLINE int enter_list (hak_t* hak, const hak_loc_t* loc, int flagv)
{
	hak_rstl_t* rstl;
	rstl = (hak_rstl_t*)hak_callocmem(hak, HAK_SIZEOF(*rstl));
	if (HAK_UNLIKELY(!rstl))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to allocate reader stack node - %js", orgmsg);
		return -1;
	}
	rstl->loc = *loc;
	rstl->flagv = flagv;
	rstl->prev = hak->c->r.st; /* push */
	hak->c->r.st = rstl;
	return 0;
}

static HAK_INLINE hak_cnode_t* leave_list (hak_t* hak, hak_loc_t* list_loc, int* flagv, int* oldflagv)
{
	hak_rstl_t* rstl;
	hak_cnode_t* head, * tail;
	hak_oow_t count;
	hak_loc_t loc;
	int fv;
	hak_concode_t concode;

	/* the stack must not be empty - cannot leave a list without entering it */
	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st; /* get the stack top */

	head = rstl->head;
	tail = rstl->tail;
	count = rstl->count;
	fv = rstl->flagv;
	loc = rstl->loc;
	concode = (hak_concode_t)LIST_FLAG_GET_CONCODE(fv);

	hak->c->r.st = rstl->prev; /* pop off  */
	hak_freemem(hak, rstl); /* dispose of the stack node */

	if (fv & (COMMAED | COLONED | COLONEQED | BINOPED))
	{
		/* no item after , : := or various binary operators */
		if (concode == HAK_CONCODE_MLIST)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_CALLABLE, TOKEN_LOC(hak), HAK_NULL, "missing message after receiver");
		}
		else if (concode == HAK_CONCODE_ALIST)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_RVALUE, TOKEN_LOC(hak), HAK_NULL, "missing rvalue after :=");
		}
		else if (concode == HAK_CONCODE_BLIST)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_NOVALUE, TOKEN_LOC(hak), HAK_NULL, "missing expression after binary selector");
		}
		else
		{
			if (fv & COMMAED)
				hak_setsynerrbfmt(hak, HAK_SYNERR_COMMANOVALUE, TOKEN_LOC(hak), HAK_NULL, "no valid token after comma");
			else
				hak_setsynerrbfmt(hak, HAK_SYNERR_COMMANOVALUE, TOKEN_LOC(hak), HAK_NULL, "no valid token after colon");
		}
		goto oops;
	}

	*list_loc = loc;
	*oldflagv = fv;
	if (!hak->c->r.st)
	{
		/* the stack is empty after popping.
		 * it is back to the top level.
		 * the top level can never be quoted. */
		*flagv = 0;
	}
	else
	{
		/* restore the flag for the outer returning level */
		*flagv = hak->c->r.st->flagv;
	}

	if (head)
	{
		HAK_ASSERT(hak, HAK_CNODE_IS_CONS(head));

		if (concode == HAK_CONCODE_ALIST) /* assignment list */
		{
			/* sanitize/tranform (var := val) to (set var val)
			 * - note ALIST doesn't contain the := symbol */
			hak_cnode_t* lval;
		#if defined(TRANSFORM_ALIST)
			hak_cnode_t* sym, * newhead;
			hak_oocs_t fake_tok, * fake_tok_ptr = HAK_NULL;
		#endif

			lval = HAK_CNODE_CONS_CAR(head);
			if (lval && HAK_CNODE_IS_ELIST(lval))
			{
				/* invalid lvalue - for example, () := 20 */
				hak_setsynerrbfmt(hak, HAK_SYNERR_LVALUE, HAK_CNODE_GET_LOC(lval), HAK_NULL, "bad lvalue - blank expression");
				goto oops;
			}
			else if (lval && HAK_CNODE_IS_CONS_CONCODED(lval, HAK_CONCODE_TUPLE))
			{
				/*
				 * fun f(a :: b c) { b := (a + 10); c := (a + 20) }
				 * [x, y] := (f 9) ## this kind of expression - translate to set-r x y (f 9)
				 */
				hak_cnode_t* tmp;
		#if defined(TRANSFORM_ALIST)
				hak_cnode_t* rval;
		#endif

		#if defined(TRANSFORM_ALIST)
				fake_tok.ptr = vocas[VOCA_SYM_SET_R].str;
				fake_tok.len = vocas[VOCA_SYM_SET_R].len;
				fake_tok_ptr = &fake_tok;
		#endif

				for (tmp = lval; tmp && HAK_CNODE_IS_CONS(tmp); tmp = HAK_CNODE_CONS_CDR(tmp))
				{
					/* check in advance if the array members are all plain symbols */
					hak_cnode_t* lcar;
					lcar = HAK_CNODE_CONS_CAR(tmp);
					if (!HAK_CNODE_IS_SYMBOL(lcar) && !HAK_CNODE_IS_DSYMBOL_CLA(lcar))
					{
						hak_setsynerrbfmt(hak, HAK_SYNERR_LVALUE, HAK_CNODE_GET_LOC(lcar), HAK_NULL,
							"bad lvalue - invalid token%hs%.*js in tuple",
							(HAK_CNODE_GET_TOKLEN(lcar) > 0? " ": ""),
							HAK_CNODE_GET_TOKLEN(lcar), HAK_CNODE_GET_TOKPTR(lcar));
						goto oops;
					}
				}

		#if defined(TRANSFORM_ALIST)
				/* move the array item up to the main list and join the original lval to the end of it
				 * For [x, y] := (f 9), x and y must be in the same level as set-r after translation.
				 * so make it 'x y (f 9)' first and place set-r in front of it later. */
				rval = HAK_CNODE_CONS_CDR(head);
				hak_freesinglecnode(hak, head);
				head = lval;
				for (tmp = lval; tmp && HAK_CNODE_IS_CONS(tmp); tmp = HAK_CNODE_CONS_CDR(tmp))
				{
					if (!HAK_CNODE_CONS_CDR(tmp))
					{
						HAK_CNODE_CONS_CDR(tmp) = rval;
						break;
					}
				}
		#endif
			}
			else
			{
				if (!HAK_CNODE_IS_SYMBOL(lval) && !HAK_CNODE_IS_DSYMBOL_CLA(lval))
				{
					/* for example, 1 := 20 */
					hak_setsynerrbfmt(hak, HAK_SYNERR_LVALUE, HAK_CNODE_GET_LOC(lval), HAK_NULL,
						"bad lvalue - invalid identifier '%.*js'",
						HAK_CNODE_GET_TOKLEN(lval), HAK_CNODE_GET_TOKPTR(lval));
					goto oops;
				}
		#if defined(TRANSFORM_ALIST)
				fake_tok.ptr = vocas[VOCA_SYM_SET].str;
				fake_tok.len = vocas[VOCA_SYM_SET].len;
				fake_tok_ptr = &fake_tok;
		#endif
			}

			HAK_ASSERT(hak, count >= 2); /* the missing rvalue check has been done above */
			if (count != 2)
			{
				hak_cnode_t* rval;
				rval = HAK_CNODE_CONS_CDR(head);
				rval = HAK_CNODE_CONS_CDR(rval);
				rval = HAK_CNODE_CONS_CAR(rval);

				hak_setsynerrbfmt(hak, HAK_SYNERR_RVALUE, HAK_CNODE_GET_LOC(rval), HAK_NULL,
					"too many rvalues after := around '%.*js'",
					HAK_CNODE_GET_TOKLEN(rval), HAK_CNODE_GET_TOKPTR(rval));
				goto oops;
			}

		#if defined(TRANSFORM_ALIST)
			sym = hak_makecnodesymbol(hak, 0, &loc, fake_tok_ptr);
			if (HAK_UNLIKELY(!sym))
			{
				const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
				hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to create symbol cnode for := - %js", orgmsg);
				goto oops;
			}

			/* create a new head joined with set or set-r */
			newhead = hak_makecnodecons(hak, 0, &loc, fake_tok_ptr, sym, head);
			if (HAK_UNLIKELY(!newhead))
			{
				const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
				hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to create cons cnode for := - %js", orgmsg);
				hak_freecnode(hak, sym);
				goto oops;
			}

			head = newhead;
			concode = HAK_CONCODE_XLIST; /* switch back to XLIST */
		#endif
		}
		else if (concode == HAK_CONCODE_BLIST)
		{
			/* x binop y -> binop x y - BLIST contains BINOP in it */
			hak_cnode_t* x, * binop;

			/*x = HAK_CNODE_CONS_CAR(head);*/
			binop = HAK_CNODE_CONS_CDR(head);

			HAK_ASSERT(hak, binop && HAK_CNODE_IS_CONS(binop));
			HAK_ASSERT(hak, count >= 2); /* the code in can_binop_list() ensures this condition */

			if (count < 3)
			{
				/* for example, 1 + */
				x = HAK_CNODE_CONS_CAR(binop);
				/* with the transformation, the implementation supports two operands and a single binop in an expression. */
				hak_setsynerrbfmt(hak, HAK_SYNERR_NOVALUE, HAK_CNODE_GET_LOC(x), HAK_NULL,
					"no operand after binary selector '%.*js'", HAK_CNODE_GET_TOKLEN(x), HAK_CNODE_GET_TOKPTR(x));
				goto oops;
			}
			else if (count > 3)
			{
				/* for example, 1 + 1 1 */
				x = HAK_CNODE_CONS_CAR(tail);
				/* with the transformation, the implementation supports two operands and a single binop in an expression. */
				hak_setsynerrbfmt(hak, HAK_SYNERR_NOVALUE, HAK_CNODE_GET_LOC(x), HAK_NULL,
					"redundant operand '%.*js'", HAK_CNODE_GET_TOKLEN(x), HAK_CNODE_GET_TOKPTR(x));
				goto oops;
			}

		#if defined(TRANSFORM_BLIST)
			/* this part is to transform (x binop y) to (binop x y).
			 * if transformation is done, it is a normal executable expression
			 * where the binary operator is a primitive function.
			 *
			 * alternatively, the compiler treat this as a message send expression
			 * if the reader skips this transformation.
			 *
			 * We keep this part commented out to have this trated as a message
			 * send expression. */
			HAK_CNODE_CONS_CDR(head) = HAK_CNODE_CONS_CDR(binop);
			HAK_CNODE_CONS_CDR(binop) = head;
			head = binop;
			concode = HAK_CONCODE_XLIST;
		#endif
		}

		HAK_CNODE_CONS_CONCODE(head) = concode;
		if (fv & AUTO_FORGED) HAK_CNODE_GET_FLAGS(head) |= HAK_CNODE_AUTO_FORGED;
	}
	else
	{
		/* the list is empty */
		head = hak_makecnodeelist(hak, ((fv & AUTO_FORGED)? HAK_CNODE_AUTO_FORGED: 0), &loc, concode);
		if (HAK_UNLIKELY(!head))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to create empty list - %js", orgmsg);
		}
	}

	return head;

oops:
	if (head) hak_freecnode(hak, head);
	return HAK_NULL;
}

static HAK_INLINE int can_dot_list (hak_t* hak)
{
	hak_rstl_t* rstl;

	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;

	/* mark the state that a dot has appeared in the list */
	if (rstl->count <= 0) return 0;
	if (LIST_FLAG_GET_CONCODE(rstl->flagv) != HAK_CONCODE_QLIST) return 0;

	rstl->flagv |= DOTTED;
	return 1;
}

static HAK_INLINE int can_comma_list (hak_t* hak)
{
	hak_rstl_t* rstl;
	hak_concode_t cc;

	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;

	if (rstl->count <= 0) return 0;

	if (rstl->count == 1) rstl->flagv |= JSON;
	else if (!(rstl->flagv & JSON)) return 0;

	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	cc = (hak_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);
	if (cc == HAK_CONCODE_DIC)
	{
		if (rstl->count & 1) return 0;
	}
	else if (cc != HAK_CONCODE_ARRAY && cc != HAK_CONCODE_BYTEARRAY &&
	         cc != HAK_CONCODE_CHARARRAY && cc != HAK_CONCODE_TUPLE)
	{
		return 0;
	}

	rstl->flagv |= COMMAED;
	return 1;
}

static HAK_INLINE int can_colon_list (hak_t* hak)
{
	hak_rstl_t* rstl;
	hak_concode_t cc;

	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;
	cc = (hak_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);

	if (rstl->count <= 0) return 0; /* not allowed at the list beginning  */

	/* mark the state that a colon has appeared in the list */
	if (cc == HAK_CONCODE_XLIST && HAK_CNODE_IS_FOR_LANG(HAK_CNODE_CONS_CAR(rstl->head)))
	{
		/* allow a colon if the first element is 'class', 'fun', or some other keywords:
		 *   class :superclassame ...
		 *   class name:superclassname ...
		 *   fun X:abc ... */
		return 2;
	}

	if (rstl->count == 1) rstl->flagv |= JSON; /* mark that the first key is colon-delimited */
	else if (!(rstl->flagv & JSON))
	{
#if 0

/* this strict check is not needed as it returns 2 above if the first element is a language keyword element */
		/* handling of a colon sign in out-of-class instance method definition.
		 * e.g. fun String:length() { return (str.length self). }
		 * TODO: inject a symbol ':' to differentiate form '::' or ':*' methods.
		 *       these class methods and class instantiation methods are supposed to be
		 *       implemented elsewhere because ':' has dual use while '::' or ':*' are
		 *       independent tokens  */
		if (HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(rstl->head), HAK_CNODE_FUN))
		{
			hak_cnode_t* tmp, * next;
			next = HAK_CNODE_CONS_CDR(rstl->head);
			HAK_ASSERT(hak, next != HAK_NULL);
			tmp = HAK_CNODE_CONS_CAR(next); /* second item */
			if (rstl->count == 2)
			{
				/* fun class:name() *... */
				if (HAK_CNODE_IS_SYMBOL(tmp)) return 2;
			}
			else if (rstl->count == 3)
			{
				/* fun(#c) class:name() ... */
				if (HAK_CNODE_IS_CONS_CONCODED(tmp, HAK_CONCODE_XLIST) ||
				    HAK_CNODE_IS_ELIST_CONCODED(tmp, HAK_CONCODE_XLIST))
				{
					next = HAK_CNODE_CONS_CDR(next);
					HAK_ASSERT(hak, next != HAK_NULL);
					tmp = HAK_CNODE_CONS_CAR(next); /* third item */
					if (HAK_CNODE_IS_SYMBOL(tmp)) return 2;
				}
			}
		}
#endif

		return 0; /* the first key is not colon-delimited. so not allowed to colon-delimit other keys  */
	}

	/* multiple single-colons  - e.g. #{ "abc": : 20 } */
	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	if (cc == HAK_CONCODE_XLIST)
	{
		hak_cnode_t* tmp;

		/* method defintion with fun  - e.g. fun String:length()
		 * ugly that this reader must know about the meaning of fun */
		if (rstl->count > 1) return 0;

		/* ugly dual use of a colon sign. switch to MLIST if the first element
		 * is delimited by a colon. e.g. (obj:new 10 20 30)  */
		tmp = HAK_CNODE_CONS_CAR(rstl->head);
		if (!HAK_CNODE_IS_FOR_DATA(tmp))
		{
			/* check if the first element can refer to or represent an object.
			 * for example, '#[1 2 3]:at 1' is proper message send.
			 * while 'class:xxx {}' is not a method call. it is unamed class
			 * that inherits from xxx */
			return 0;
		}

		LIST_FLAG_SET_CONCODE(rstl->flagv, HAK_CONCODE_MLIST);
		rstl->flagv &= ~JSON;
	}
	else if (cc != HAK_CONCODE_DIC) return 0; /* no allowed if not in a dictionary */

	/* dictionary */
	if (!(rstl->count & 1)) return 0; /* not allwed after the value in a dictionary */

	/* mark that it's coloned. this is to be cleared when clear_comma_colon_binop_flag() is called */
	rstl->flagv |= COLONED;
	return 1;
}

static HAK_INLINE int can_coloneq_list (hak_t* hak)
{
	hak_rstl_t* rstl;
	hak_concode_t cc;

	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;

	if (rstl->count <= 0 || rstl->count >= 2) return 0; /* allowed after the first item only */

	/* repeated delimiters - e.g (a := := ...)   (a : := ... )  */
	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	cc = (hak_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);

	/* assignment only in XLIST */
	if (cc != HAK_CONCODE_XLIST) return 0;

	LIST_FLAG_SET_CONCODE(rstl->flagv, HAK_CONCODE_ALIST);
	rstl->flagv |= COLONEQED;
	return 1;
}

static HAK_INLINE int can_binop_list (hak_t* hak)
{
	hak_rstl_t* rstl;
	hak_concode_t cc;

	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;
	cc = (hak_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);

	if (rstl->count <= 0 || cc == HAK_CONCODE_TUPLE || is_at_block_beginning(hak))
	{
		/* allowed but it must be treated like a normal identifier.
		 * in case of the tuple, chain_to_list() rejects binop symbols.
		 * so let this routine to allow it as a normal indentifier.
		 *
		 * when the expression is inside a block enclosed in {},
		 * rstl->count is the number of preceding expression.
		 * call is_at_block_begigging() separately to check if it is
		 * at the beging of the sub-expression. For example,
		 *  { a := 10; b := 20; + a b }
		 * when this function is called for '+' above, rstl->count is 2.
		 */
		return 1;
	}

	if (rstl->count >= 1 && cc == HAK_CONCODE_XLIST)
	{
		/* special case:
		 *   fun xxx::+() { }
		 *   fun + () {}
		 */

		/* TODO: this whole block is hacky. we may do proper parsing instead of checking the first element is 'fun' */
		if (HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(rstl->head), HAK_CNODE_FUN)) return 1;
	}

	/* repeated delimiters - e.g (a ++ ++ ...)   (a : := ... )  */
	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	if (cc == HAK_CONCODE_BLIST)
	{
		hak_cnode_t* wrap;
		hak_oocs_t fake_tok, * fake_tok_ptr = HAK_NULL;

		/* revised to BLIST in earlier call.
		 * three elements before this binop.
		 * xxx (1 + 2 + 3)
		 *            ^
		 *            here
		 * in the list are '1', '+', '2', tallying to 3.
		 */

		/* unable to support operator precedence as the meaning
		 * of binary operators is not pre-defined in this language */

		/* [NOTE] - something wrong
		 *   the repeated delimiters check above can't catch BINOP repetition
		 *   because BINOP itself is added to the list and flagv is cleared
		 *   with the call to clear_comma_colon_binop_flag().
		 * [TODO]
		 *   review this implentation such that the BINOPED flag isn't needed
		 */
		/*HAK_ASSERT(hak, rstl->count == 2 || rstl->count == 3); this condition is wrong, for say, 1 + 2 3 + 4 */
		HAK_ASSERT(hak, rstl->count >= 2);

		/* [TODO] this condition is a workaround for the failing repetition check.
		 *   1 + + */
		if (rstl->count == 2) return 0;

		/* 1 + 2 3 + 4 */
		if (rstl->count > 3) return 0;

		fake_tok.ptr = vocas[VOCA_BLIST].str;
		fake_tok.len = vocas[VOCA_BLIST].len;
		fake_tok_ptr = &fake_tok;

		/* dirty hack to create a wrapper cell containing the first three items.
		 * TODO: do i have to do this on the caller side of can_binop_list()? */
		HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(rstl->head, HAK_CNODE_CONS));
		wrap = hak_makecnodecons(hak, 0, HAK_CNODE_GET_LOC(rstl->head), fake_tok_ptr, rstl->head, HAK_NULL);
		if (HAK_UNLIKELY(!wrap)) return  -1;
		HAK_CNODE_CONS_CONCODE(rstl->head) = HAK_CONCODE_BLIST;

		rstl->head = wrap;
		rstl->tail = wrap;
		rstl->count = 1; /* force adjust it to 1 */
	}
	else
	{
		if (rstl->count >= 2) return 0; /* allowed after the first item only */
		if (cc != HAK_CONCODE_XLIST) return 0;
	}

	LIST_FLAG_SET_CONCODE(rstl->flagv, HAK_CONCODE_BLIST); /* switch to BLIST as long as a binary operator is seen */
	rstl->flagv |= BINOPED;
/* TODO: must remember the actual binop operator token */
	return 2;
}

static HAK_INLINE void clear_comma_colon_binop_flag (hak_t* hak)
{
	hak_rstl_t* rstl;
	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;
	rstl->flagv &= ~(COMMAED | COLONED | COLONEQED | BINOPED);
}

static int chain_to_list (hak_t* hak, hak_cnode_t* obj, hak_loc_t* loc)
{
	hak_rstl_t* rstl;
	int flagv;
	/*int list_concode;*/

	HAK_ASSERT(hak, hak->c->r.st != HAK_NULL);
	rstl = hak->c->r.st;
	flagv = rstl->flagv;
	/*list_concode = (hak_concode_t)LIST_FLAG_GET_CONCODE(flagv);*/

	if (flagv & CLOSED)
	{
		/* the list has already been closed and cannot add more items
		 * for instance,  see this faulty expression #(1 2 . 3 4 ).
		 * you can have only 1 item  after the period. this condition
		 * can only be triggered by a wrong qlist where a period is
		 * allowed. so i can safely hard-code the error code to
		 * HAK_SYNERR_RPAREN */
		hak_setsynerrbfmt(hak, HAK_SYNERR_RPAREN, TOKEN_LOC(hak), HAK_NULL, ") expected around '%.*js'", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
		return -1;
	}
	else if (flagv & DOTTED)
	{
		hak_cnode_t* tail;
		/* the list must not be empty to have reached the dotted state */
		HAK_ASSERT(hak, rstl->head != HAK_NULL);
		HAK_ASSERT(hak, rstl->tail != HAK_NULL);
		HAK_ASSERT(hak, rstl->count > 0);

		/* chain the object via 'cdr' of the tail cell */
		tail = rstl->tail;
		HAK_ASSERT(hak, tail != HAK_NULL);
		HAK_ASSERT(hak, HAK_CNODE_IS_CONS(tail));

		if (HAK_CNODE_IS_CONS(obj) && HAK_CNODE_CONS_CONCODE(obj) != HAK_CONCODE_QLIST)
		{
			hak_cnode_t* shell;
			/* if the last element is another non-data list
			 * for example, #( 1 2 . #[ 3 4 5  ])
			 * use a shell node to wrap the actual object list node head
			 * for the compiler.
			 */
			shell = hak_makecnodeshell(hak, 0, HAK_CNODE_GET_LOC(obj), obj);
			if (HAK_UNLIKELY(!shell)) return -1;
			tail->u.cons.cdr = shell;
		}
		else
		{
			tail->u.cons.cdr = obj;
		}

		/* update the flag to CLOSED so that you can have more than
		 * one item after the dot. */
		flagv |= CLOSED;
		rstl->flagv = flagv;
/* TODO: check overflow on count??? */
		rstl->count++;
	}
	else
	{
		hak_cnode_t* cons, * tail;
		hak_oocs_t fake_tok, * fake_tok_ptr = HAK_NULL;
		int concode;

		if ((flagv & JSON) && rstl->count > 0 && !(flagv & (COMMAED | COLONED)))
		{
			/* there is no separator between array/dictionary elements
			 * for instance, #{10:20, 30:40 40:50} */
			hak_setsynerrbfmt(hak, HAK_SYNERR_NOSEP, TOKEN_LOC(hak), HAK_NULL,
				"no separator between array/dictionary elements around '%.*js",
				TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			return -1;
		}

		/* `loc` may be passed in if the added `obj` is a cons cell for another list */
		HAK_ASSERT(hak, (loc && (HAK_CNODE_IS_CONS(obj) || HAK_CNODE_IS_ELIST(obj))) || (!loc && !HAK_CNODE_IS_CONS(obj)));
		concode = HAK_CNODE_IS_CONS(obj)? HAK_CNODE_CONS_CONCODE(obj):
		          HAK_CNODE_IS_ELIST(obj)? HAK_CNODE_ELIST_CONCODE(obj): -1;

		if (concode >= 0)
		{
			int vid = cons_info[concode].voca_id;
			fake_tok.ptr = vocas[vid].str;
			fake_tok.len = vocas[vid].len;
			fake_tok_ptr = &fake_tok;
		}

		cons = hak_makecnodecons(hak, 0, (loc? loc: HAK_CNODE_GET_LOC(obj)), fake_tok_ptr, obj, HAK_NULL);
		if (HAK_UNLIKELY(!cons)) return -1;

		if (rstl->count <= 0)
		{
			/* the list head is not set yet. it is the first
			 * element added to the list. let both head and tail
			 * point to the new cons cell */
			HAK_ASSERT(hak, rstl->tail == HAK_NULL);
			HAK_ASSERT(hak, rstl->head == HAK_NULL);

			rstl->head = cons;
			rstl->tail = cons;
		}
		else
		{
			/* the new cons cell is not the first element. append it to the list */
			tail = rstl->tail;
			HAK_ASSERT(hak, HAK_CNODE_IS_CONS(tail));
			tail->u.cons.cdr = cons;
			rstl->tail = cons;
		}

/* TODO: check overflow on count??? */
		rstl->count++;
	}

	return 0;
}

/* ------------------------------------------------------------------------ */

/* TODO:
hak_cnodetoobj (hak_t* hak, hak_cnode_t* x)
{
 * drop location information and compose object ??
 * is it doable? can convert a dotted symbol to a proper value?
}
*/

/* ---------------------------------------------------------------------- */

static int on_fed_cnode (hak_t* hak, hak_cnode_t* obj)
{
	/* the default handler for a cnode composed via feeding - just compile the object node. */
	return hak_compile(hak, obj, 0);
}

/* ---------------------------------------------------------------------- */

static void init_feed (hak_t* hak)
{
	HAK_MEMSET(&hak->c->feed, 0, HAK_SIZEOF(hak->c->feed));
	hak->c->feed.lx.state = HAK_FLX_START;
	hak->c->feed.lx.loc.line = 1;
	hak->c->feed.lx.loc.colm = 1;
	hak->c->feed.lx.loc.file = HAK_NULL;
	hak->c->feed.on_cnode = on_fed_cnode;
}

/* ------------------------------------------------------------------------ */

static int feed_begin_include (hak_t* hak)
{
	hak_io_cciarg_t* arg;
	const hak_ooch_t* io_name;

	io_name = add_sr_name(hak, TOKEN_NAME(hak));
	if (HAK_UNLIKELY(!io_name))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to include %.*js for name registration failure - %js", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak), orgmsg);
		return -1;
	}

	arg = (hak_io_cciarg_t*)hak_callocmem(hak, HAK_SIZEOF(*arg));
	if (HAK_UNLIKELY(!arg))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to include %.*js for memory allocation failure - %js", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak), orgmsg);
		goto oops;
	}

	arg->name = io_name;
	arg->line = 1;
	arg->colm = 1;
	/*arg->nl = '\0';*/
	/*arg->byte_oriented = 0;*/
	arg->includer = hak->c->curinp;

	if (hak->c->cci_rdr(hak, HAK_IO_OPEN, arg) <= -1)
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_setsynerrbfmt(hak, HAK_SYNERR_INCLUDE, TOKEN_LOC(hak), HAK_NULL, "unable to include %js - %js", io_name, orgmsg);
		goto oops;
	}

	if (arg->includer == &hak->c->cci_arg) /* top-level include */
	{
		/* TODO: remove hak_readbasesrchar() and clean up this part.
		 * hak_readbasesrchar(), if called in the middle of feeds,
		 * updates hak->c->cci_arg's line and colm. so use a separate
		 * field to store the current feed location for now */
		hak->c->feed.lx._oloc = hak->c->feed.lx.loc;
	}
	else
	{
		arg->includer->name = hak->c->feed.lx.loc.file;
		arg->includer->line = hak->c->feed.lx.loc.line;
		arg->includer->colm = hak->c->feed.lx.loc.colm;
	}
	hak->c->feed.lx.loc.file = arg->name;
	hak->c->feed.lx.loc.line = arg->line;
	hak->c->feed.lx.loc.colm = arg->colm;

	/* switch to the includee's stream */
	hak->c->curinp = arg;
	/* hak->c->depth.incl++; */

	return 0;

oops:
	if (arg)
	{
		hak_freemem(hak, arg);
	}
	return -1;
}

static int feed_end_include (hak_t* hak)
{
	int x;
	hak_io_cciarg_t* cur;

	if (hak->c->curinp == &hak->c->cci_arg) return 0; /* no include */

	/* if it is an included file, close it and
	 * retry to read a character from an outer file */

	x = hak->c->cci_rdr(hak, HAK_IO_CLOSE, hak->c->curinp);

	/* if closing has failed, still destroy the sio structure
	 * first as normal and return the failure below. this way,
	 * the caller doesn't call HAK_IO_CLOSE on hak->c->curinp again. */

	cur = hak->c->curinp;
	hak->c->curinp = hak->c->curinp->includer;

	if (hak->c->curinp == &hak->c->cci_arg)
	{
		hak->c->feed.lx.loc = hak->c->feed.lx._oloc;
	}
	else
	{
		hak->c->feed.lx.loc.file = hak->c->curinp->name;
		hak->c->feed.lx.loc.line = hak->c->curinp->line;
		hak->c->feed.lx.loc.colm = hak->c->curinp->colm;
	}

	HAK_ASSERT(hak, cur->name != HAK_NULL);
	hak_freemem(hak, cur);
	/* hak->parse.depth.incl--; */

	if (x != 0)
	{
		/* the failure mentioned above is returned here */
		return -1;
	}

	hak->c->lxc = hak->c->curinp->lxc;
	return 1; /* ended the included file successfully */
}

static void feed_reset_reader_state (hak_t* hak)
{
	hak_frd_t* frd = &hak->c->feed.rd;

	if (frd->obj)
	{
		hak_freecnode(hak, frd->obj);
		frd->obj = HAK_NULL;
	}
	HAK_MEMSET(frd, 0, HAK_SIZEOF(*frd));
}

static void feed_clean_up_reader_stack (hak_t* hak)
{
	/* clean up the reader stack for a list */
	while (hak->c->r.st)
	{
		hak_rstl_t* rstl;
		rstl = hak->c->r.st;
		hak->c->r.st = rstl->prev;
		if (rstl->head) hak_freecnode(hak, rstl->head);
		hak_freemem(hak, rstl);
	}
}

static int is_at_block_beginning (hak_t* hak)
{
	hak_rstl_t* rstl;
	rstl = hak->c->r.st;
	return !rstl || (LIST_FLAG_GET_CONCODE(rstl->flagv) == HAK_CONCODE_BLOCK && (hak->c->feed.rd.flagv & AT_BEGINNING));
}

static int auto_forge_xlist_if_at_block_beginning (hak_t* hak, hak_frd_t* frd)
{
	if (is_at_block_beginning(hak))
	{
		int forged_flagv;

		/* both MLIST and ALIST begin as XLIST and get converted to MLIST
		 * or ALIST after more tokens are processed. so handling of MLIST
		 * or ALIST is needed at this phase */
		forged_flagv = AUTO_FORGED;
		LIST_FLAG_SET_CONCODE(forged_flagv, HAK_CONCODE_XLIST);

		/* this portion is similar to the code below the start_list label */
		if (frd->level >= HAK_TYPE_MAX(int)) /* the nesting level too deep */
		{
			hak_setsynerr(hak, HAK_SYNERR_NESTING, TOKEN_LOC(hak), TOKEN_NAME(hak));
			return -1;
		}

		/* since the actual list opener doesn't exist, the location of the
		 * first element wil be the location of the list */
		if (enter_list(hak, TOKEN_LOC(hak), forged_flagv) <= -1) return -1;
		frd->level++; /* level after the forged list has been added */
		/* a new list has been created automatically. unlike normal list creation
		 * by an explicit symbol such as a left parenthesis, a left brace, etc,
		 * the first element opens up this new list. so the AT_BEGINNING bit is
		 * turned off here */
		frd->flagv &= ~AT_BEGINNING;
	}

	return 0;
}

static hak_cnode_type_t kw_to_cnode_type (int tok_type)
{
	static hak_cnode_type_t mapping[] = {
		HAK_CNODE_NIL,
		HAK_CNODE_TRUE,
		HAK_CNODE_FALSE,
		HAK_CNODE_SELF,
		HAK_CNODE_SUPER,

		HAK_CNODE_CLASS,
		HAK_CNODE_FUN,
		HAK_CNODE_VAR,
		HAK_CNODE_DO,
		HAK_CNODE_IF,
		HAK_CNODE_ELIF,
		HAK_CNODE_ELSE,
		HAK_CNODE_THROW,
		HAK_CNODE_TRY,
		HAK_CNODE_CATCH,
		HAK_CNODE_BREAK,
		HAK_CNODE_CONTINUE,
		HAK_CNODE_UNTIL,
		HAK_CNODE_WHILE,
		HAK_CNODE_RETURN,
		HAK_CNODE_REVERT,
		HAK_CNODE_AND,
		HAK_CNODE_OR,
#if defined(USE_KW_PLUS)
		HAK_CNODE_PLUS,
#endif
		HAK_CNODE_SET,
		HAK_CNODE_SET_R
	};

	return mapping[tok_type - HAK_TOK_NIL];
}

static int feed_process_token (hak_t* hak)
{
	hak_frd_t* frd = &hak->c->feed.rd;
	hak_loc_t* list_loc = HAK_NULL;
	int rbrace_again = 0;
	int oops_ret = -1;
	/* TODO: frd->obj and frd->list_loc can become local variables in this function.. */

	/* this function composes an s-expression non-recursively
	 * by manipulating its own stack. */

/*hak_logbfmt(hak, HAK_LOG_STDERR, "TOKEN [%d] EOL[%d]=> [%.*js] type=%d LOC=%d.%d\n", TOKEN_TYPE(hak), HAK_TOK_EOL, TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak), TOKEN_TYPE(hak), TOKEN_LOC(hak)->line, TOKEN_LOC(hak)->colm);*/
	if (frd->expect_pragma_item)
	{
		/* the pragmas changes the behavior of the reader and the compiler */
		if (frd->expect_pragma_item <= -1) /* eol expected */
		{
			if (TOKEN_TYPE(hak) != HAK_TOK_EOL && TOKEN_TYPE(hak) != HAK_TOK_SEMICOLON)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
					"redundant token '%.*js' for '%.*js'",
					TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak),
					vocas[VOCA_PRAGMA].len, vocas[VOCA_PRAGMA].str);
				goto oops;
			}
			frd->expect_pragma_item = 0;
		}
		else
		{
			if (TOKEN_TYPE(hak) == HAK_TOK_EOL || TOKEN_TYPE(hak) == HAK_TOK_SEMICOLON)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_IDENT, TOKEN_LOC(hak), HAK_NULL,
					"'%.*js' %hs not specified",
					vocas[VOCA_PRAGMA].len, vocas[VOCA_PRAGMA].str,
					(frd->expect_pragma_item == 1? "name": "value"));
				goto oops;
			}
			else if (TOKEN_TYPE(hak) != HAK_TOK_IDENT)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_IDENT, TOKEN_LOC(hak), HAK_NULL,
					"'%.*js' %hs expected in place of '%.*js'",
					vocas[VOCA_INCLUDE].len, vocas[VOCA_INCLUDE].str,
					(frd->expect_pragma_item == 1? "name": "value"),
					TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
				goto oops;
			}

			if (frd->expect_pragma_item == 1)
			{
				if (does_token_name_match(hak, VOCA_PRG_LIBERAL))
				{
					frd->expect_pragma_item = 501; /* expect value */
				}
				/* TODO: more pragmas */
				else
				{
					hak_setsynerrbfmt(hak, HAK_SYNERR_IDENT, TOKEN_LOC(hak), HAK_NULL,
						"invalid pragma name '%.*js'",
						TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
					goto oops;
				}
			}
			else if (frd->expect_pragma_item == 501)
			{
				if (does_token_name_match(hak, VOCA_PRG_ON))
				{
					hak->option.trait |= HAK_TRAIT_LANG_LIBERAL;
				}
				else if (does_token_name_match(hak, VOCA_PRG_OFF))
				{
					hak->option.trait &= ~HAK_TRAIT_LANG_LIBERAL;
				}
				else
				{
					hak_setsynerrbfmt(hak, HAK_SYNERR_IDENT, TOKEN_LOC(hak), HAK_NULL,
						"invalid pragma value '%.*js'",
						TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
					goto oops;
				}

				frd->expect_pragma_item = -1; /* expect eol */
			}
		}
		goto ok;
	}

	if (frd->expect_include_file)
	{
		/* the #include directive is an exception to the general expression rule.
		 * use this exceptional code block to divert the major token processing */
		if (TOKEN_TYPE(hak) == HAK_TOK_EOL || TOKEN_TYPE(hak) == HAK_TOK_SEMICOLON)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_STRING, TOKEN_LOC(hak), HAK_NULL,
				"'%.*js' target not specified",
				vocas[VOCA_INCLUDE].len, vocas[VOCA_INCLUDE].str);
			goto oops;
		}
		else if (TOKEN_TYPE(hak) != HAK_TOK_STRLIT)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_STRING, TOKEN_LOC(hak), HAK_NULL,
				"'%.*js' target expected in place of '%.*js'",
				vocas[VOCA_INCLUDE].len, vocas[VOCA_INCLUDE].str,
				TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			goto oops;
		}

		frd->expect_include_file = 0;

		/* indicate that the file inclusion should be performed soon.
		 * don't perform actual inclusion here so that the return value of
		 * feed_char() advances the input pointers properly. */
		frd->do_include_file = 1;

		goto ok;
	}

	if (frd->expect_vlist_item && TOKEN_TYPE(hak) != HAK_TOK_IDENT && TOKEN_TYPE(hak) != HAK_TOK_VBAR)
	{
		if (TOKEN_TYPE(hak) == HAK_TOK_EOL) goto ok; /* ignore EOL inside vlist */

		/* vlist also has special requirement that it can only contain variable names. */
		hak_setsynerr(hak, HAK_SYNERR_VARNAME, TOKEN_LOC(hak), TOKEN_NAME(hak));
		goto oops;
	}

	switch (TOKEN_TYPE(hak))
	{
		default:
			hak_setsynerr(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto oops;

		case HAK_TOK_EOF:
			if (hak_feedpending(hak))
			{
				hak_setsynerr(hak, HAK_SYNERR_EOF, TOKEN_LOC(hak), TOKEN_NAME(hak));
			}
			else
			{
				/* ugly hacking to return success intead while performing clean-up */
				oops_ret = 0;
			}
			goto oops;

		case HAK_TOK_INCLUDE:
			/* TODO: should i limit where #include can be specified?
			 *       disallow it inside a list literal or an array literal? */
			frd->expect_include_file = 1;
			goto ok;

		case HAK_TOK_PRAGMA:
			/*hak_setsynerr(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto oops;*/
/* TODO: check if pragma is the first word in the line */
			frd->expect_pragma_item = 1;
			goto ok;

		case HAK_TOK_VBAR:
			if (frd->expect_vlist_item)
			{
				/* closer */
				int oldflagv;
				frd->expect_vlist_item = 0;
				frd->obj = leave_list(hak, &frd->list_loc, &frd->flagv, &oldflagv);
				frd->level--;
				frd->flagv |= AT_BEGINNING;
				list_loc = &frd->list_loc;
				break;
			}
			else
			{
				/* opener */

				/* the vlist is different from other lists in that
				 *   it uses the same opener and the closer
				 *   it allows only variable names.
				 *   it prohibits nesting of other lists
				 */
				if (hak->c->r.st && (hak->c->r.st->flagv & DATA_LIST))
				{
					/* if the outer list is a data list */
					hak_setsynerr(hak, HAK_SYNERR_VBARBANNED, TOKEN_LOC(hak), TOKEN_NAME(hak));
					goto oops;
				}

				/* neither a data list nor an executable list. handle this specially using
				 * a dedicated frd->expect_vlist_item variable */
				frd->flagv = 0;
				LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_VLIST);
				frd->expect_vlist_item = 1;
				goto start_list;
			}

		case HAK_TOK_LBRACK: /* [ */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_TUPLE);
			goto start_list;

		case HAK_TOK_APAREN: /* #[ */
			/* #[] is a data list. so let's treat it like other literal
			 * expressions(e.g. 1, "abc"). when it's placed at the block beginning,
			 * create the outer XLIST. */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_ARRAY);
			goto start_list;

		case HAK_TOK_BAPAREN: /* #b[ */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_BYTEARRAY);
			goto start_list;

		case HAK_TOK_CAPAREN: /* #c[ */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_CHARARRAY);
			goto start_list;

		case HAK_TOK_LBRACE: /* { */
			/* this is a block opener itself. auto xlist forge at the block beginning only */
			frd->flagv = 0;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_BLOCK);
			goto start_list;

		case HAK_TOK_DLPAREN: /* #{ */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_DIC);
			goto start_list;

		case HAK_TOK_QLPAREN: /* #( */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_QLIST);
			goto start_list;

		#if defined(HAK_TOK_LPARCOLON)
		case HAK_TOK_LPARCOLON: /* (: */
			frd->flagv = 0;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_MLIST);
			goto start_list;
		#endif

		case HAK_TOK_LPAREN: /* ( */
#if defined(HAK_LANG_AUTO_FORGE_XLIST_ALWAYS)
			/* with this feature on, you must not enclose an expression with ()
			 * at the beginning of the top-level or at the beginning of a the block level.
			 * If you do enclose an expression with an outer () there, it is interpreted as
			 * double calls, meaning the return value of the expression is called again.
			 * for example, '(+ 10 20)' as a leading expression is like '((+ 10 20))'.
			 * -------------------------------------------------------------
			 * It is useful but a bit confusing:
			 *   fun x(a) { return (fun(b) { return (+ a b) }) }
			 *   printf "%d\n" ((x 10) 20)   ## excplicit outer () is required here
			 *   (x 10) 30  ## explicit outer () must not be used here
			 *   j := ((x 10) 40) ## explicit outer () is required here
			 *   k := {
			 *     (x 10) 50  ## explicit outer () must not be used here
			 *   }
			 */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
#endif
			frd->flagv = 0;
			LIST_FLAG_SET_CONCODE(frd->flagv, HAK_CONCODE_XLIST);
		start_list:
			if (frd->level >= HAK_TYPE_MAX(int))
			{
				/* the nesting level has become too deep */
				hak_setsynerr(hak, HAK_SYNERR_NESTING, TOKEN_LOC(hak), TOKEN_NAME(hak));
				goto oops;
			}

			/* push some data to simulate recursion into
			 * a list literal or an array literal */
			if (enter_list(hak, TOKEN_LOC(hak), frd->flagv) <= -1) goto oops;
			frd->level++;
			frd->flagv |= AT_BEGINNING; /* the reader is now at the beginning of a list */

			/* read the next token */
			goto ok;

		case HAK_TOK_DOT:
			if (frd->level <= 0 || !can_dot_list(hak))
			{
				/* cannot have a period:
				 *   1. at the top frd->level - not inside ()
				 *   2. at the beginning of a list
				 *   3. inside an array, byte-array, dictionary, xlist */
				hak_setsynerr(hak, HAK_SYNERR_DOTBANNED, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}
			goto ok;

		case HAK_TOK_COLON:
		{
			int n;
			if (frd->level <= 0 || !(n = can_colon_list(hak)))
			{
				hak_setsynerr(hak, HAK_SYNERR_COLONBANNED, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}
			if (n == 2)
			{
				/* this is colon between the class name and the function name for
				 * out-of-class method defintion */
				frd->obj = hak_makecnodecolon(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
				goto auto_xlist;
			}

			goto ok;
		}

		case HAK_TOK_COLONEQ:
			if (frd->level <= 0 || !can_coloneq_list(hak))
			{
				hak_setsynerr(hak, HAK_SYNERR_COLONEQBANNED, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}
			goto ok;

		case HAK_TOK_BINOP:
		{
			int can = 0;
			if (frd->level <= 0)
			{
				HAK_ASSERT(hak, hak->c->r.st == HAK_NULL);
				/*if (hak->c->r.st) goto banned_binop;*/
				/* very first even before entering a list including an auto-forged list */
				can = 1;
			}
			else if (!(can = can_binop_list(hak)))
			{
			/*banned_binop:*/
				hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, TOKEN_LOC(hak), HAK_NULL,
					"prohibited binary selector '%.*js'", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
				goto oops;
			}
			else if (can <= -1) goto oops;

			HAK_ASSERT(hak, can == 1 || can == 2);
			frd->obj = hak_makecnodebinop(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;
		}

		case HAK_TOK_COMMA:
			if (frd->level <= 0 || !can_comma_list(hak))
			{
				hak_setsynerr(hak, HAK_SYNERR_COMMABANNED, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}
			goto ok;

		case HAK_TOK_EOL: /* EOL returned only under a certain condition */
		case HAK_TOK_SEMICOLON:
		{
			int oldflagv;
			int concode;
			hak_rstl_t* rstl;

		semicolon:
			/* the parent list(rstl) must be inspected instead of the current
			 * feed/read status pointed to by frd. */
			rstl = hak->c->r.st;
			if (!rstl) goto ok; /* redundant eol/semicolon */

			concode = LIST_FLAG_GET_CONCODE(rstl->flagv);
			if (!(rstl->flagv & AUTO_FORGED))
			{
				if (TOKEN_TYPE(hak) == HAK_TOK_EOL) goto ok;
				if (concode == HAK_CONCODE_BLOCK) goto ok;

				hak_setsynerr(hak, HAK_SYNERR_SEMICOLON, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}

			/* if auto-forged */
#if 0
/* TODO: remove this part if the assertion is confirmed true in the #else part... */
			if (concode != HAK_CONCODE_XLIST && concode != HAK_CONCODE_MLIST && concode != HAK_CONCODE_ALIST && concode != HAK_CONCODE_BLIST)
			{
				hak_setsynerr(hak, HAK_SYNERR_UNBALPBB, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}
#else
			HAK_ASSERT(hak, concode == HAK_CONCODE_XLIST || concode == HAK_CONCODE_MLIST || concode == HAK_CONCODE_ALIST || concode == HAK_CONCODE_BLIST);
#endif

			frd->obj = leave_list(hak, &frd->list_loc, &frd->flagv, &oldflagv);
			frd->level--;
			frd->flagv |= AT_BEGINNING; /* the current one is over. move on the beginning for the next expression */
			list_loc = &frd->list_loc;
			break;
		}

		case HAK_TOK_RPAREN: /* xlist (), qlist #() */
		case HAK_TOK_RBRACK: /* bytearray #b[], array #[] */
		case HAK_TOK_RBRACE: /* dictionary #{}, block {} */
		{
			int oldflagv;
			int concode;
			hak_rstl_t* rstl;

			if (frd->level <= 0)
			{
				hak_setsynerr(hak, HAK_SYNERR_UNBALPBB, TOKEN_LOC(hak), HAK_NULL);
				goto oops;
			}

			if (TOKEN_TYPE(hak) == HAK_TOK_RBRACE)
			{
				rstl = hak->c->r.st; /* check the parent, not the current */
				if (rstl && (rstl->flagv & AUTO_FORGED))
				{
				#if 0
					/* the auto-forged list has not been terminated. it must be terminated closed first */
					hak_setsynerrbfmt(hak, HAK_SYNERR_SEMICOLON, TOKEN_LOC(hak), TOKEN_NAME(hak), "semicolon expected");
					goto oops;
				#else
					/* if the expression inside {} is an auto-forged xlist expression and there is no semicolon provided,
					 * treat it as if the semicolon is placed before }. e.g. { printf "hello\n" } */
					rbrace_again = 1;
					goto semicolon;
				#endif
				}
			}

		rbrace_ok:
			concode = LIST_FLAG_GET_CONCODE(frd->flagv);
			if (concode == HAK_CONCODE_XLIST && (frd->flagv & AUTO_FORGED))
			{
				/* the auto-created xlist can't be terminated with the regular closing symbol
				 * it must end with the semicolon */
				hak_setsynerr(hak, HAK_SYNERR_UNBALPBB, TOKEN_LOC(hak), TOKEN_NAME(hak));
				goto oops;
			}

			if (cons_info[concode].closer != TOKEN_TYPE(hak))
			{
				hak_setsynerr(hak, cons_info[concode].synerr, TOKEN_LOC(hak), TOKEN_NAME(hak));
				goto oops;
			}
#if 0
			if ((flagv & QUOTED) || frd->level <= 0)
			{
				/* the right parenthesis can never appear while
				 * 'quoted' is true. 'quoted' is set to false when
				 * entering a normal list. 'quoted' is set to true
				 * when entering a quoted list. a quoted list does
				 * not have an explicit right parenthesis.
				 * so the right parenthesis can only pair up with
				 * the left parenthesis for the normal list.
				 *
				 * For example, '(1 2 3 ') 5 6)
				 *
				 * this condition is triggerred when the first ) is
				 * met after the second quote.
				 *
				 * also it is illegal to have the right parenthesis
				 * with no opening(left) parenthesis, which is
				 * indicated by frd->level<=0.
				 */
				hak_setsynerr(hak, HAK_SYNERR_LPAREN, TOKEN_LOC(hak), TOKEN_NAME(hak));
				goto oops;
			}
#endif
			frd->obj = leave_list(hak, &frd->list_loc, &frd->flagv, &oldflagv);
			if (HAK_LIKELY(frd->obj))
			{
				frd->level--;
				frd->flagv |= AT_BEGINNING;
				list_loc = &frd->list_loc;
			}
			break;
		}

		case HAK_TOK_NIL:
		case HAK_TOK_TRUE:
		case HAK_TOK_FALSE:
		case HAK_TOK_SELF:
		case HAK_TOK_SUPER:

		case HAK_TOK_CLASS:
		case HAK_TOK_FUN:
		case HAK_TOK_VAR:
		case HAK_TOK_DO:
		case HAK_TOK_IF:
		case HAK_TOK_ELIF:
		case HAK_TOK_ELSE:
		case HAK_TOK_THROW:
		case HAK_TOK_TRY:
		case HAK_TOK_CATCH:
		case HAK_TOK_BREAK:
		case HAK_TOK_CONTINUE:
		case HAK_TOK_UNTIL:
		case HAK_TOK_WHILE:
		case HAK_TOK_RETURN:
		case HAK_TOK_REVERT:
		case HAK_TOK_AND:
		case HAK_TOK_OR:
#if defined(USE_KW_PLUS)
		case HAK_TOK_PLUS:
#endif
		case HAK_TOK_SET:
		case HAK_TOK_SET_R:
			frd->obj = hak_makecnode(hak, kw_to_cnode_type(TOKEN_TYPE(hak)), 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_ELLIPSIS:
			frd->obj = hak_makecnodeellipsis(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_TRPCOLONS:
			frd->obj = hak_makecnodetrpcolons(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_DBLCOLONS:
			frd->obj = hak_makecnodedblcolons(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_SMPTRLIT:
		{
			hak_oow_t i;
			hak_oow_t v = 0;

			/* 0pNNNN */
			HAK_ASSERT(hak, TOKEN_NAME_LEN(hak) >= 3);
			for (i = 2; i < TOKEN_NAME_LEN(hak); i++)
			{
				HAK_ASSERT(hak, is_xdigit_char(TOKEN_NAME_CHAR(hak, i)));
				v = v * 16 + HAK_CHAR_TO_NUM(TOKEN_NAME_CHAR(hak, i), 16);
			}

			if (!HAK_IN_SMPTR_RANGE(v))
			{
				hak_setsynerr(hak, HAK_SYNERR_SMPTRLIT, TOKEN_LOC(hak), TOKEN_NAME(hak));
				goto oops;
			}

			frd->obj = hak_makecnodesmptrlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak), v);
			goto auto_xlist;
		}

		case HAK_TOK_ERRLIT:
		{
			hak_oow_t i;
			hak_ooi_t v = 0;

			HAK_ASSERT(hak, TOKEN_NAME_LEN(hak) >= 3);
			for (i = 2; i < TOKEN_NAME_LEN(hak); i++)
			{
				HAK_ASSERT(hak, is_digit_char(TOKEN_NAME_CHAR(hak, i)));
				v = v * 10 + HAK_CHAR_TO_NUM(TOKEN_NAME_CHAR(hak, i), 10);

				if (v > HAK_ERROR_MAX)
				{
					hak_setsynerr(hak, HAK_SYNERR_ERRLIT, TOKEN_LOC(hak), TOKEN_NAME(hak));
					goto oops;
				}
			}

			frd->obj = hak_makecnodeerrlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak), v);
			goto auto_xlist;
		}

		case HAK_TOK_CHARLIT:
			frd->obj = hak_makecnodecharlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak), TOKEN_NAME_CHAR(hak, 0));
			goto auto_xlist;

		case HAK_TOK_BCHRLIT:
			frd->obj = hak_makecnodebchrlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak), (hak_oob_t)TOKEN_NAME_CHAR(hak, 0));
			goto auto_xlist;

		case HAK_TOK_STRLIT:
			frd->obj = hak_makecnodestrlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_BSTRLIT:
			frd->obj = hak_makecnodebstrlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_SYMLIT:
			frd->obj = hak_makecnodesymlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_NUMLIT:
			frd->obj = hak_makecnodenumlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_RADNUMLIT:
			frd->obj = hak_makecnoderadnumlit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_FPDECLIT:
			frd->obj = hak_makecnodefpdeclit(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		/*
		case HAK_TOK_REAL:
			frd->obj = hak_makecnoderealnum(hak, HAK_TOK_RVAL(hak));
			break;
		*/

		case HAK_TOK_IDENT:
		ident:
			frd->obj = hak_makecnodesymbol(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak));
			goto auto_xlist;

		case HAK_TOK_IDENT_DOTTED:
			frd->obj = hak_makecnodedsymbol(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak), 0);
			goto auto_xlist;

		case HAK_TOK_IDENT_DOTTED_CLA:
			frd->obj = hak_makecnodedsymbol(hak, 0, TOKEN_LOC(hak), TOKEN_NAME(hak), 1);
			goto auto_xlist;

		auto_xlist:
			if (!frd->obj) goto oops; /* TODO: ugly... resturcture this check and the check 5 lines down  */
			if (auto_forge_xlist_if_at_block_beginning(hak, frd) <= -1) goto oops;
			break;
	}

	if (!frd->obj) goto oops; /* TODO: this doesn't have to be checked if jump has been made to auto_xlist... so restructure the flow */

	frd->obj->cn_llvl = frd->level; /* list level */

#if 0
	/* check if the element is read for a quoted list */
	while (flagv & QUOTED)
	{
		int oldflagv;

		HAK_ASSERT(hak, frd->level > 0);

		/* if so, append the element read into the quote list */
		if (chain_to_list(hak, obj) <= -1) goto oops;

		/* exit out of the quoted list. the quoted list can have one element only. */
		obj = leave_list(hak, &flagv, &oldflagv);

		/* one frd->level up toward the top */
		frd->level--;
	}
#endif

	/* check if we are at the top frd->level */
	if (frd->level <= 0)
	{
		int n;

		/* upon exit, we must be at the top level */
		HAK_ASSERT(hak, frd->flagv & AT_BEGINNING);

		HAK_ASSERT(hak, hak->c->r.st == HAK_NULL);
		HAK_ASSERT(hak, frd->obj != HAK_NULL);

		n = hak->c->feed.on_cnode(hak, frd->obj);
		hak_freecnode(hak, frd->obj); /* not needed any more */
		frd->obj = HAK_NULL;
		if (n <= -1) goto oops;
	}
	else
	{
		/* if not, append the element read into the current list.
		 * if we are not at the top frd->level, we must be in a list */
		if (chain_to_list(hak, frd->obj, list_loc) <= -1) goto oops;

		/* because it has been chained to the list, it belongs to the current stack top.
		 * mark that obj is not stand-alone by nullifying it. without this, if a jump
		 * is made to oops, the momory block pointed to by obj may get freed twice. */
		frd->obj = HAK_NULL;

		clear_comma_colon_binop_flag(hak);
	}

ok:
	if (rbrace_again)
	{
		rbrace_again = 0;
		list_loc = HAK_NULL;
		goto rbrace_ok;
	}

	return 0;

oops:
	feed_reset_reader_state(hak);

	/* clean up the reader stack for a list */
	feed_clean_up_reader_stack(hak);
	feed_continue(hak, HAK_FLX_START);
	return oops_ret;
}

/* ------------------------------------------------------------------------ */

struct delim_token_t
{
	const char*      t_value;
	hak_oow_t        t_len;
	hak_tok_type_t t_type;
};
typedef struct delim_token_t delim_token_t;

static delim_token_t delim_token_tab[] =
{
	/* [NOTE 1]
	 *  if you add a new token, ensure the first character is listed in is_delim_char()
	 *
	 * [NOTE 2]
	 *  for the implementation limitation in find_delim_token_char(),
	 *  the entries in this table must be laid out in a certain way.
	 *
	 *    Group the items with the same prefix together.
	 *    List the shorter before the longer items in the same group.
	 *    The length must not differ by greater than 1 between 2 items in the same group.
	 *
	 * [NOTE 3]
	 *  don't list #(, #[, #{ here because of overlapping use of # for various purposes.
	 *  however, # is included in is_delim_char().
	 */

	{ "(",        1, HAK_TOK_LPAREN },
#if defined(HAK_TOK_LPARCOLON)
	{ "(:",       2, HAK_TOK_LPARCOLON },
#endif
	{ ")",        1, HAK_TOK_RPAREN },

	{ "[",        1, HAK_TOK_LBRACK },
	{ "]",        1, HAK_TOK_RBRACK },

	{ "{",        1, HAK_TOK_LBRACE },
	{ "}",        1, HAK_TOK_RBRACE },

	{ "|",        1, HAK_TOK_VBAR },
	{ ",",        1, HAK_TOK_COMMA },

	{ ".",        1, HAK_TOK_DOT },
	{ "..",       2, HAK_TOK_DBLDOTS },
	{ "...",      3, HAK_TOK_ELLIPSIS }, /* for variable arguments */

	/* the tokens beginning with a colon must be handled separately in flx_colon_token()
	 * becuase some characters(= only as of now) after the colon are binop charaters, i
	 * don't want to find the match from the beginning. if a colon is followed by binop
	 * characters, i want to read all of them first and determin the token types */
	{ ":",        1, HAK_TOK_COLON }, /* key-value separator in dictionary or for method call or definition */
	{ ":=",       2, HAK_TOK_COLONEQ }, /* assignment */
	{ "::",       2, HAK_TOK_DBLCOLONS }, /* superclass, class variables, class methods */
	{ ":::",      3, HAK_TOK_TRPCOLONS },

	{ ";",        1, HAK_TOK_SEMICOLON }
};

static int find_delim_token_char (hak_t* hak, const hak_ooci_t c, int row_start, int row_end, int col, hak_flx_dt_t* dt)
{
	int found = 0, i;

	for (i = row_start; i <= row_end; i++)
	{
		if (col < delim_token_tab[i].t_len && c == delim_token_tab[i].t_value[col])
		{
			if (!found) dt->row_start = i;
			dt->row_end = i;
			found = 1;
		}
		else if (found) break;
	}

	if (found) dt->col_next = col + 1;
	return found;
}

static HAK_INLINE int feed_wrap_up (hak_t* hak, hak_tok_type_t type)
{
	int n;
	SET_TOKEN_TYPE(hak, type);

	n = feed_process_token(hak);

	hak->c->feed.lx.state = HAK_FLX_START;
	return n;
}

static int feed_wrap_up_with_char (hak_t* hak, hak_ooci_t c, hak_tok_type_t type)
{
	ADD_TOKEN_CHAR(hak, c);
	return feed_wrap_up(hak, type);
}

static int feed_wrap_up_with_str (hak_t* hak, const hak_ooch_t* str, hak_oow_t len, hak_tok_type_t type)
{
	ADD_TOKEN_STR(hak, str, len);
	return feed_wrap_up(hak, type);
}

static void feed_continue (hak_t* hak, hak_flx_state_t state)
{
	hak->c->feed.lx.state = state;
}

static int feed_continue_with_char (hak_t* hak, hak_ooci_t c, hak_flx_state_t state)
{
	ADD_TOKEN_CHAR(hak, c);
	hak->c->feed.lx.state = state;
	return 0;
}

#define FEED_WRAP_UP(hak, type) do { if (feed_wrap_up(hak, type) <= -1) return -1; } while (0)
#define FEED_WRAP_UP_WITH_CHAR(hak, c, type) do { if (feed_wrap_up_with_char(hak, c, type) <= -1) return -1; } while (0)
#define FEED_WRAP_UP_WITH_CHARS(hak, str, len, type) do { if (feed_wrap_up_with_str(hak, str, len, type) <= -1) return -1; } while (0)
#define FEED_CONTINUE(hak, state) (feed_continue(hak, state))
#define FEED_CONTINUE_WITH_CHAR(hak, c, state) do { if (feed_continue_with_char(hak, c, state) <= -1) return -1; } while (0)

/* ------------------------------------------------------------------------ */

/* short-cuts to basic lexer data */
#define FLX_STATE(hak) ((hak)->c->feed.lx.state)
#define FLX_LOC(hak) (&((hak)->c->feed.lx.loc))

/* short-cuts to lexer state data */
#define FLX_DT(hak) (&((hak)->c->feed.lx.u.dt))
#define FLX_DI(hak) (&((hak)->c->feed.lx.u.di))
#define FLX_HC(hak) (&((hak)->c->feed.lx.u.hc))
#define FLX_HBC(hak) (&((hak)->c->feed.lx.u.hbc))
#define FLX_HN(hak) (&((hak)->c->feed.lx.u.hn))
#define FLX_HI(hak) (&((hak)->c->feed.lx.u.hi))
#define FLX_PI(hak) (&((hak)->c->feed.lx.u.pi))
#define FLX_PN(hak) (&((hak)->c->feed.lx.u.pn))
#define FLX_QT(hak) (&((hak)->c->feed.lx.u.qt))
#define FLX_ST(hak) (&((hak)->c->feed.lx.u.st))
#define FLX_BCP(hak) (&((hak)->c->feed.lx.u.bcp))

static HAK_INLINE void init_flx_di (hak_flx_di_t* di)
{
	HAK_MEMSET(di, 0, HAK_SIZEOF(*di));
}

static HAK_INLINE void init_flx_hc (hak_flx_hc_t* hc)
{
	HAK_MEMSET(hc, 0, HAK_SIZEOF(*hc));
}

static HAK_INLINE void init_flx_hi (hak_flx_hi_t* hi)
{
	HAK_MEMSET(hi, 0, HAK_SIZEOF(*hi));
}

static HAK_INLINE void init_flx_hbc (hak_flx_hbc_t* hbc, hak_ooch_t start_c)
{
	HAK_MEMSET(hbc, 0, HAK_SIZEOF(*hbc));
	hbc->start_c = start_c;
}

static HAK_INLINE void init_flx_qt (hak_flx_qt_t* qt, hak_tok_type_t tok_type, hak_synerrnum_t synerr_code, hak_ooch_t end_char, hak_ooch_t esc_char, hak_oow_t min_len, hak_oow_t max_len, int is_byte)
{
	HAK_MEMSET(qt, 0, HAK_SIZEOF(*qt));
	qt->tok_type = tok_type;
	qt->synerr_code = synerr_code;
	qt->end_char = end_char;
	qt->esc_char = esc_char;
	qt->min_len = min_len;
	qt->max_len = max_len;
	qt->is_byte = is_byte;
}

static HAK_INLINE void init_flx_pi (hak_flx_pi_t* pi)
{
	HAK_MEMSET(pi, 0, HAK_SIZEOF(*pi));
}

static HAK_INLINE void init_flx_pn (hak_flx_pn_t* pn, hak_ooch_t start_digit)
{
	HAK_MEMSET(pn, 0, HAK_SIZEOF(*pn));
	pn->start_digit = start_digit;
	pn->radix = 10;
	pn->radix_cand = 0;
	pn->radix_cand_overflown = 0;
	pn->tok_type = HAK_TOK_NUMLIT;
}

static HAK_INLINE void init_flx_st (hak_flx_st_t* st, hak_ooch_t sign_c)
{
	HAK_MEMSET(st, 0, HAK_SIZEOF(*st));
	st->sign_c = sign_c;
}

static HAK_INLINE void init_flx_bcp (hak_flx_bcp_t* bcp, hak_ooch_t start_c)
{
	HAK_MEMSET(bcp, 0, HAK_SIZEOF(*bcp));
	bcp->start_c = start_c;
}

static void reset_flx_token (hak_t* hak)
{
	/* clear the token name, reset its location */
	SET_TOKEN_TYPE(hak, HAK_TOK_EOF); /* is it correct? */
	CLEAR_TOKEN_NAME(hak);
	SET_TOKEN_LOC(hak, &hak->c->feed.lx.loc);
}

static int flx_start (hak_t* hak, hak_ooci_t c)
{
	HAK_ASSERT(hak, FLX_STATE(hak) == HAK_FLX_START);

	if (is_spacechar(c))
	{
		if ((hak->option.trait & HAK_TRAIT_LANG_ENABLE_EOL) && is_linebreak(c))
		{
			reset_flx_token(hak);
			FEED_WRAP_UP_WITH_CHAR(hak, c, HAK_TOK_EOL);
		}

		goto consumed; /* skip spaces */
	}

	reset_flx_token(hak);

	if (c == ':')
	{
		FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_COLON_TOKEN);
		goto consumed;
	}
	else if (find_delim_token_char(hak, c, 0, HAK_COUNTOF(delim_token_tab) - 1, 0, FLX_DT(hak)))
	{
		/* the character is one of the first character of a delimiter token such as (, [, :, etc */
		if (FLX_DT(hak)->row_start == FLX_DT(hak)->row_end &&
		    FLX_DT(hak)->col_next == delim_token_tab[FLX_DT(hak)->row_start].t_len)
		{
			/* single character delimiter token */
			FEED_WRAP_UP_WITH_CHAR(hak, c, delim_token_tab[FLX_DT(hak)->row_start].t_type);
		}
		else
		{
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_DELIM_TOKEN); /* consume c and move to HAK_FLX_DELIM_TOKEN state */
		}
		goto consumed;
	}

	switch (c)
	{
		case HAK_OOCI_EOF:
			/* only EOF of the top-level stream is supposed to be fed in.
			 * the internal logic discard EOFs of included streams */
			FEED_WRAP_UP_WITH_CHARS(hak, vocas[VOCA_EOF].str, vocas[VOCA_EOF].len, HAK_TOK_EOF);
			goto consumed;

		case '\\':
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_BACKSLASHED);
			goto consumed;

		/* this part is never hit because the semicolon sign is part of delim_tok_tab{}
		   TODO: remove this part once the language spec is finalized to not require this
		case ';':
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_COMMENT);
			goto consumed;
		*/

		case '$':
			init_flx_di(FLX_DI(hak));
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_DOLLARED_IDENT);
			goto consumed;

		case '#':
			/* no state date to initialize. just change the state */
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_HMARKED_TOKEN);
			goto consumed;

		case '\"':
			init_flx_qt(FLX_QT(hak), HAK_TOK_STRLIT, HAK_SYNERR_STRLIT, c, '\\', 0, HAK_TYPE_MAX(hak_oow_t), 0);
			FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;

		case '\'':
			init_flx_qt(FLX_QT(hak), HAK_TOK_CHARLIT, HAK_SYNERR_CHARLIT, c, '\\', 1, 1, 0);
			FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;

#if defined(HAK_OOCH_IS_UCH) && defined(HAK_LANG_ENABLE_WIDE_DELIM)
		case L'\u201C': /* “ ” */
			init_flx_qt(FLX_QT(hak), HAK_TOK_STRLIT, HAK_SYNERR_STRLIT, L'\u201D', '\\', 0, HAK_TYPE_MAX(hak_oow_t), 0);
			FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;
		case L'\u2018': /* ‘ ’ */
			init_flx_qt(FLX_QT(hak), HAK_TOK_CHARLIT, HAK_SYNERR_CHARLIT, L'\u2019', '\\', 1, 1, 0);
			FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;
#endif

		case '+':
		case '-':
			init_flx_st(FLX_ST(hak), c);
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_SIGNED_TOKEN);
			goto consumed;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			init_flx_pn(FLX_PN(hak), c);
			FEED_CONTINUE(hak, HAK_FLX_PLAIN_NUMBER);
			goto not_consumed;

		case 'B': /* for charcter/string prefixed with B,b,C,c */
		case 'b':
		case 'C':
		case 'c':
			init_flx_bcp(FLX_BCP(hak), c);
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_BC_PREFIX);
			goto consumed;

		default:
			if (is_lead_ident_char(c))
			{
				init_flx_pi(FLX_PI(hak));
				FEED_CONTINUE(hak, HAK_FLX_PLAIN_IDENT);
			}
			else
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_BACKSLASH, TOKEN_LOC(hak), HAK_NULL, "invalid token character - %c", c);
				return -1;
			}
			goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_backslashed (hak_t* hak, hak_ooci_t c)
{
	if (is_linebreak(c))
	{
		FEED_CONTINUE(hak, HAK_FLX_START);
		return 1; /* consumed */
	}

	hak_setsynerrbfmt(hak, HAK_SYNERR_BACKSLASH, TOKEN_LOC(hak), TOKEN_NAME(hak), "stray backslash");
	return -1;
}

static int flx_comment (hak_t* hak, hak_ooci_t c)
{
	if (is_linebreak(c))
	{
		FEED_CONTINUE(hak, HAK_FLX_START);
		/* don't consume the line break together with the comment text
		 * if a comment text is located at the back of the line in the
		 * LANG_ENABLE_EOL mode.
		 * TODO: Consider removing this check because not consuming it
		 *       in another mode doesn't cause a problem. */
		if (hak->option.trait & HAK_TRAIT_LANG_ENABLE_EOL) return 0; /* not consumed */
	}
	return 1; /* consumed */
}

static int flx_colon_token (hak_t* hak, hak_ooci_t c)
{
	if (c == '=')
	{
		FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_COLONEQ_TOKEN);
		goto consumed;
	}
	else
	{
		/* as if it's called in flx_start() */
		find_delim_token_char(hak, ':', 0, HAK_COUNTOF(delim_token_tab) - 1, 0, FLX_DT(hak)); /* this must succeed */
		FEED_CONTINUE(hak, HAK_FLX_DELIM_TOKEN);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_coloneq_token (hak_t* hak, hak_ooci_t c)
{
	if (is_binop_char(c))
	{
		/* := followed by another binop char */
		TOKEN_NAME_LEN(hak)--; /* as if = after : is not in the token buffer */
		FEED_WRAP_UP(hak, HAK_TOK_COLON);

		/* as if feed_char('=') has been called. super ugly!! it plays trick to make this part just work. i hate this part  */
		reset_flx_token(hak);
		TOKEN_LOC(hak)->colm--; /* since the actual starting = is one character before c */
		init_flx_pi(FLX_PI(hak));
		if (flx_plain_ident(hak, '=') <= -1) return -1;
		FEED_CONTINUE(hak, HAK_FLX_PLAIN_IDENT);
		goto not_consumed;
	}
	else
	{
		FEED_WRAP_UP(hak, HAK_TOK_COLONEQ);
		goto not_consumed;
	}

not_consumed:
	return 0;
}

static int flx_delim_token (hak_t* hak, hak_ooci_t c)
{
	if (find_delim_token_char(hak, c, FLX_DT(hak)->row_start, FLX_DT(hak)->row_end, FLX_DT(hak)->col_next, FLX_DT(hak)))
	{
		if (FLX_DT(hak)->row_start == FLX_DT(hak)->row_end &&
		    FLX_DT(hak)->col_next == delim_token_tab[FLX_DT(hak)->row_start].t_len)
		{
			/* complete token and switch to the HAK_FLX_START state */
			FEED_WRAP_UP_WITH_CHAR(hak, c, delim_token_tab[FLX_DT(hak)->row_start].t_type);
		}
		else
		{
			ADD_TOKEN_CHAR(hak, c);
		}
		goto consumed;
	}
	else
	{
		/* the longest match so far */
		FEED_WRAP_UP(hak, delim_token_tab[FLX_DT(hak)->row_start].t_type);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_dollared_ident (hak_t* hak, hak_ooci_t c)
{
	hak_flx_di_t* di = FLX_DI(hak);

	/* di->char_count doesn't include the first '$' */

	if (is_delim_char(c))
	{
		hak_tok_type_t tok_type;

		if (di->char_count == 0)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, FLX_LOC(hak), HAK_NULL,
				"no valid character after dollar sign");
			return -1;
		}

		if (get_directive_token_type(hak, &tok_type) <= -1)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), TOKEN_NAME(hak),
				"invalid dollar-prefixed identifier '%.*js'", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			return -1;
		}
		else
		{
			FEED_WRAP_UP(hak, tok_type);
			goto not_consumed;
		}
	}
	else if (is_ident_char(c))
	{
		if (di->char_count == 0)
		{
			if (!is_lead_ident_char(c))
			{
				/* some character can't placed immediately after '$'. e.g '?' */
				hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
					"'%c' prohibited as first character after '%.*js'",
					c, TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
				return -1;
			}
		}

		ADD_TOKEN_CHAR(hak, c);
		di->char_count++;
		goto consumed;
	}
	else
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
			"invalid dollar-prefixed identifier character '%jc' after '%.*js'", c,
			TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_token (hak_t* hak, hak_ooci_t c)
{
	/*
	 * #xXXXX   hexadecimal
	 * #oOOOO   octal
	 * #bBBBB   binary
	 * #eDDD    error
	 * #pHHH    smptr
	 * #\C      character
	 * #\xHHHH  unicode character
	 * #\UHHHH  unicode character
	 * #\uHHHH  unicode character
	 * #\backspace
	 * #\linefeed
	 * #\newline
	 * #\nul
	 * #\page
	 * #\return
	 * #\rubout
	 * #\space
	 * #\tab
	 * #\vtab
	 * #include
	 * #[ ]     array
	 * #b[ ]    byte array
	 * #c[ ]    character array
	 * #( )     qlist
	 * #{ }     dictionary
	 * #"..."   symbol literal
	 */

	switch (c)
	{
		case '#':
		case '!':
			/* ## comment start
			 * #! also comment start.
			 * ; comment start */
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_COMMENT);
			goto consumed;

		/* --------------------------- */

	#if 0
		case 'x': /* hexadecimal number */
			init_flx_hn(FLX_HN(hak), HAK_TOK_RADNUMLIT, HAK_SYNERR_NUMLIT, 16);
			goto radixed_number;

		case 'o': /* octal number */
			init_flx_hn(FLX_HN(hak), HAK_TOK_RADNUMLIT, HAK_SYNERR_NUMLIT, 8);
			goto radixed_number;
	#endif

		case 'b': /* binary number or byte array */
		case 'B':
		case 'c': /* character array */
		case 'C':
			/* #b[ -> byte array, #c[ -> character array */
			init_flx_hbc(FLX_HBC(hak), c);
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_HMARKED_BC);
			break;

		/* --------------------------- */
		case '\\':
			init_flx_hc(FLX_HC(hak));
			FEED_CONTINUE_WITH_CHAR(hak, c, HAK_FLX_HMARKED_CHAR);
			goto consumed;

		/* --------------------------- */
		case '[': /* #[ */
			FEED_WRAP_UP_WITH_CHAR(hak, c, HAK_TOK_APAREN);
			goto consumed;

		case '(': /* #( */
			FEED_WRAP_UP_WITH_CHAR(hak, c, HAK_TOK_QLPAREN);
			goto consumed;

		case '{': /* #{ */
			FEED_WRAP_UP_WITH_CHAR(hak, c, HAK_TOK_DLPAREN);
			goto consumed;

		case '"': /* #" - double-quoted symbol */
			reset_flx_token(hak);
			init_flx_qt(FLX_QT(hak), HAK_TOK_SYMLIT, HAK_SYNERR_SYMLIT, c, '\\', 0, HAK_TYPE_MAX(hak_oow_t), 0);
			FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* discard prefix, quote and move on */
			goto consumed;

		/* --------------------------- */
		default:
			init_flx_hi(FLX_HI(hak));
			reset_flx_token(hak); /* to discard the leading '#' */
			FEED_CONTINUE(hak, HAK_FLX_HMARKED_IDENT);
			goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_char (hak_t* hak, hak_ooci_t c)
{
	hak_flx_hc_t* hc = FLX_HC(hak);

	if (is_delim_char(c))
	{
		if (hc->char_count == 0)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_CHARLIT, TOKEN_LOC(hak), TOKEN_NAME(hak),
				"no valid character in character literal %.*js", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			return -1;
		}

		if (TOKEN_NAME_LEN(hak) >= 4)
		{
			int max_digit_count = 0;

			if (TOKEN_NAME_CHAR(hak, 2) == 'x')
			{
				hak_oow_t i;
				max_digit_count = 2;

			hexcharlit:
				if (TOKEN_NAME_LEN(hak) - 3 > max_digit_count)
				{
					hak_setsynerrbfmt(hak, HAK_SYNERR_CHARLIT, TOKEN_LOC(hak), TOKEN_NAME(hak),
						"invalid hexadecimal character character literal %.*js", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
					return -1;
				}
				c = 0;
				for (i = 3; i < TOKEN_NAME_LEN(hak); i++)
				{
					if (!is_xdigit_char(TOKEN_NAME_CHAR(hak, i)))
					{
						hak_setsynerrbfmt(hak, HAK_SYNERR_CHARLIT, TOKEN_LOC(hak), TOKEN_NAME(hak),
							"invalid hexadecimal character character literal %.*js", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
						return -1;
					}
					c = c * 16 + HAK_CHAR_TO_NUM(TOKEN_NAME_CHAR(hak, i), 16); /* don't care if it is for 'p' */
				}
			}
		#if (HAK_SIZEOF_OOCH_T >= 2)
			else if (TOKEN_NAME_CHAR(hak, 2) == 'u')
			{
				max_digit_count = 4;
				goto hexcharlit;
			}
		#endif
		#if (HAK_SIZEOF_OOCH_T >= 4)
			else if (TOKEN_NAME_CHAR(hak, 2) == 'U')
			{
				max_digit_count = 8;
				goto hexcharlit;
			}
		#endif
			else if (does_token_name_match(hak, VOCA_CHAR_BACKSPACE)) c = '\b';
			else if (does_token_name_match(hak, VOCA_CHAR_LINEFEED))  c = '\n';
			else if (does_token_name_match(hak, VOCA_CHAR_NEWLINE))   c = '\n'; 	/* TODO: convert it to host newline convention. how to handle if it's composed of 2 letters like \r\n? */
			else if (does_token_name_match(hak, VOCA_CHAR_NUL))       c = '\0';  /* null character. not the object null */
			else if (does_token_name_match(hak, VOCA_CHAR_PAGE))      c = '\f';
			else if (does_token_name_match(hak, VOCA_CHAR_RETURN))    c = '\r';
			else if (does_token_name_match(hak, VOCA_CHAR_RUBOUT))    c = '\x7F'; /* DEL */
			else if (does_token_name_match(hak, VOCA_CHAR_SPACE))     c = ' ';
			else if (does_token_name_match(hak, VOCA_CHAR_TAB))       c = '\t';
			else if (does_token_name_match(hak, VOCA_CHAR_VTAB))      c = '\v';
			else
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_CHARLIT, TOKEN_LOC(hak), TOKEN_NAME(hak),
					"invalid character literal %.*js", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
				return -1;
			}
		}
		else
		{
			HAK_ASSERT(hak, TOKEN_NAME_LEN(hak) == 3);
			c = TOKEN_NAME_CHAR(hak, 2);
		}

		/* reset the token name to the converted character */
		CLEAR_TOKEN_NAME(hak);
		ADD_TOKEN_CHAR(hak, c);
		FEED_WRAP_UP(hak, HAK_TOK_CHARLIT);
		goto not_consumed;
	}
	else
	{
		ADD_TOKEN_CHAR(hak, c);
		hc->char_count++;
		goto consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_bc (hak_t* hak, hak_ooci_t c)
{
	hak_flx_hbc_t* hb = FLX_HBC(hak);

	if (c == '[')
	{
		/* #b[ - byte array starter */
		/* TODO: more types.. #c[  #w[ .. #u32[ ... etc */
		/*                  char-array word-array 32bit-int-array etc */
		hak_tok_type_t tt;
		tt = (hb->start_c == 'b' || hb->start_c == 'B')? HAK_TOK_BAPAREN: HAK_TOK_CAPAREN;
		FEED_WRAP_UP_WITH_CHAR(hak, c, tt);
		goto consumed;
	}
	else
	{
		hak_ooch_t start_c = hb->start_c;
		reset_flx_token(hak);
		FEED_CONTINUE_WITH_CHAR(hak, start_c, HAK_FLX_HMARKED_IDENT);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_binop (hak_t* hak, hak_ooci_t c)
{
	if (is_binop_char(c))
	{
		ADD_TOKEN_CHAR(hak, c);
		goto consumed;
	}
	else if (is_delim_char(c))
	{
		FEED_WRAP_UP(hak, HAK_TOK_SYMLIT);
		goto not_consumed;
	}
	else
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_SYMLIT,
			TOKEN_LOC(hak), HAK_NULL /* no token name as incomplete */,
			"invalid binary selector character '%jc' after #%.*js",
			c, TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_ident (hak_t* hak, hak_ooci_t c)
{
	hak_flx_hi_t* hi = FLX_HI(hak);

	/* hi->char_count doesn't include the first '#' */

	if (is_delim_char(c))
	{
		if (hi->char_count == 0)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_SYMLIT, FLX_LOC(hak), HAK_NULL,
				"no valid character after hash sign");
			return -1;
		}

		FEED_WRAP_UP(hak, HAK_TOK_SYMLIT);
		goto not_consumed;
	}
	else if (is_ident_char(c))
	{
		if (hi->char_count == 0)
		{
			if (!is_lead_ident_char(c))
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
					"'%c' prohibited as first character of symbol", c);
				return -1;
			}
		}

		ADD_TOKEN_CHAR(hak, c);
		hi->char_count++;
		goto consumed;
	}
	else
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
			"invalid symbol character '%jc' after '%.*js'", c,
			TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_plain_ident (hak_t* hak, hak_ooci_t c) /* identifier */
{
	hak_flx_pi_t* pi = FLX_PI(hak);

	if (is_delim_char(c)) /* [NOTE] . is one of the delimiter character */
	{
		hak_oow_t start;
		hak_oocs_t seg;
		hak_tok_type_t tok_type;

		if (pi->seg_len == 0)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
				"blank segment after '%.*js'", TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			return -1;
		}

		start = TOKEN_NAME_LEN(hak) - pi->seg_len;
		seg.ptr = &TOKEN_NAME_CHAR(hak, start);
		seg.len = pi->seg_len;
		if (classify_ident_token(hak, &seg, TOKEN_LOC(hak), &tok_type) <= -1) return -1;
		if (tok_type != HAK_TOK_IDENT)
		{
			if (pi->seg_count == 0 && (tok_type == HAK_TOK_SELF || tok_type == HAK_TOK_SUPER))
			{
				/* allowed if it begins with self. or super. */
				pi->is_cla = 1; /* mark that it's prefixed with self or super */
			}
			else
			{
				/* for example, if.if.abc - flag the error after having consumed all the segments */
				pi->non_ident_seg_count++;
				pi->last_non_ident_type = tok_type;
				pi->last_non_ident_seg = pi->seg_count;
			}
		}

		pi->seg_len = 0; /* the length of the segment to be worked on */
		pi->seg_count++; /* total number of segments completed */

		if (c == '.')
		{
			/* move on to the next segment */
			ADD_TOKEN_CHAR(hak, c);
			pi->char_count++;
			goto consumed;
		}

		/* finish */
		if (pi->non_ident_seg_count > 0)
		{
			if (pi->seg_count == 1)
			{
				FEED_WRAP_UP(hak, pi->last_non_ident_type);
				goto not_consumed;
			}
			else if (!pi->is_cla && pi->seg_count == pi->last_non_ident_seg + 1 && pi->last_non_ident_type == HAK_TOK_BINOP)
			{
				/* allow it if the last segment is binop and not prefixed with self. or super. */
				/* for example, core.+ */
				/* do nothing */
			}
			else
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), TOKEN_NAME(hak), "wrong multi-segment identifier");
				return -1;
			}
		}

		/* if single-segmented, perform classification(call classify_ident_token()) again
		 * because self and super as the first segment have not been marked as a non-identifier above */
		if (pi->seg_count == 1)
		{
			if (classify_ident_token(hak, TOKEN_NAME(hak), TOKEN_LOC(hak), &tok_type) <= -1) return -1;
		}
		else
		{
			tok_type = pi->is_cla? HAK_TOK_IDENT_DOTTED_CLA: HAK_TOK_IDENT_DOTTED;
		}
		FEED_WRAP_UP(hak, tok_type);
		goto not_consumed;
	}
	else if (is_ident_char(c))
	{
		if (pi->seg_len == 0)
		{
			if (!is_lead_ident_char(c))
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
					"'%c' prohibited as first character of identifier or identifier segment after '%.*js'",
					c, TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
				return -1;
			}
		}

		ADD_TOKEN_CHAR(hak, c);
		pi->char_count++;
		pi->seg_len++;
		goto consumed;
	}
	else
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ILTOK, TOKEN_LOC(hak), HAK_NULL,
			"invalid identifier character '%jc' after '%.*js'", c,
			TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_plain_number (hak_t* hak, hak_ooci_t c) /* number */
{
	hak_flx_pn_t* pn = FLX_PN(hak);

	if (is_radixed_digit_char(c, pn->radix))
	{
		ADD_TOKEN_CHAR(hak, c);
		pn->digit_count[pn->fpdec]++;
		if (pn->tok_type == HAK_TOK_NUMLIT)
		{
			hak_oow_t cand = pn->radix_cand * 10 + (c - '0');
			if (cand < pn->radix_cand) pn->radix_cand_overflown = 1;
			pn->radix_cand = cand;
		}
		goto consumed;
	}
	else if (is_delim_char(c))
	{
		if (!pn->fpdec && c == '.')
		{
			if (pn->radix != 10)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_NUMLIT, FLX_LOC(hak), HAK_NULL,
					"invalid use of decimal point after radixed number '%.*js'",
					TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
				return -1;
			}
			pn->fpdec = 1;
			pn->tok_type = HAK_TOK_FPDECLIT;
			ADD_TOKEN_CHAR(hak, c);
			goto consumed;
		}

		if (pn->digit_count[0] == 0)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_NUMLIT, TOKEN_LOC(hak), HAK_NULL,
				"invalid numeric literal with no digit after '%.*js'",
				TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			return -1;
		}
		else if (pn->fpdec && pn->digit_count[1] == 0)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_NUMLIT, TOKEN_LOC(hak), HAK_NULL,
				"invalid numeric literal with no digit after decimal point '%.*js'",
				TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
			return -1;
		}

		FEED_WRAP_UP(hak, pn->tok_type);
		goto not_consumed;
	}
	else
	{
		if (!pn->fpdec && pn->digit_count[0] == 1 && pn->start_digit == '0' && pn->tok_type == HAK_TOK_NUMLIT)
		{
			/* prefixed with 0 */
			switch (c)
			{
				case 'x':
					pn->tok_type = HAK_TOK_RADNUMLIT;
					pn->radix = 16;
					break;

				case 'o':
					pn->tok_type = HAK_TOK_RADNUMLIT;
					pn->radix = 8;
					break;

				case 'b':
					pn->tok_type = HAK_TOK_RADNUMLIT;
					pn->radix = 2;
					break;

				case 'p':
					pn->tok_type = HAK_TOK_SMPTRLIT;
					pn->radix = 16;
					break;

				case 'e':
					pn->tok_type = HAK_TOK_ERRLIT;
					pn->radix = 10;
					break;

				default:
					goto other_char;
			}

			ADD_TOKEN_CHAR(hak, c);
			pn->digit_count[0] = 0;
			goto consumed;
		}

	other_char:
		if (!pn->fpdec && pn->tok_type == HAK_TOK_NUMLIT && pn->digit_count[0] > 0 && c == 'r')
		{
			/* 16rABCD */
			if (pn->radix_cand_overflown || pn->radix_cand < 2 || pn->radix_cand > 36)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_NUMLIT, TOKEN_LOC(hak), HAK_NULL,
					"unsupported radix '%.*js' before '%jc'",
					TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak), c);
				return -1;
			}

			pn->tok_type = HAK_TOK_RADNUMLIT;
			pn->radix = pn->radix_cand;
			ADD_TOKEN_CHAR(hak, c);
			pn->digit_count[0] = 0;
			goto consumed;
		}

		hak_setsynerrbfmt(hak, HAK_SYNERR_NUMLIT, TOKEN_LOC(hak), HAK_NULL,
			"invalid numeric literal character '%jc' after '%.*js'",
			c, TOKEN_NAME_LEN(hak), TOKEN_NAME_PTR(hak));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_quoted_token (hak_t* hak, hak_ooci_t c) /* string, character */
{
	hak_flx_qt_t* qt = FLX_QT(hak);
	hak_loc_t synerr_loc = *TOKEN_LOC(hak);

	if (c == HAK_OOCI_EOF) goto invalid_token;

	if (qt->is_byte && c > 0xFF)
	{
		synerr_loc = *FLX_LOC(hak);
		goto invalid_token;
	}

	if (qt->escaped == 3)
	{
		if (c >= '0' && c <= '7')
		{
			/* more octal digits */
			qt->c_acc = qt->c_acc * 8 + c - '0';
			qt->digit_count++;
			if (qt->digit_count >= qt->escaped)
			{
				/* should i limit the max to 0xFF/0377?
				 * if (qt->c_acc > 0377) qt->c_acc = 0377;*/
				ADD_TOKEN_CHAR(hak, qt->c_acc);
				qt->escaped = 0;
			}
			goto consumed;
		}
		else
		{
			ADD_TOKEN_CHAR(hak, qt->c_acc);
			qt->escaped = 0;
		}
	}
	else if (qt->escaped == 2 || qt->escaped == 4 || qt->escaped == 8)
	{
		if (c >= '0' && c <= '9')
		{
			qt->c_acc = qt->c_acc * 16 + c - '0';
			qt->digit_count++;
			if (qt->digit_count >= qt->escaped)
			{
				ADD_TOKEN_CHAR(hak, qt->c_acc);
				qt->escaped = 0;
			}
			goto consumed;
		}
		else if (c >= 'A' && c <= 'F')
		{
			qt->c_acc = qt->c_acc * 16 + c - 'A' + 10;
			qt->digit_count++;
			if (qt->digit_count >= qt->escaped)
			{
				ADD_TOKEN_CHAR(hak, qt->c_acc);
				qt->escaped = 0;
			}
			goto consumed;
		}
		else if (c >= 'a' && c <= 'f')
		{
			qt->c_acc = qt->c_acc * 16 + c - 'a' + 10;
			qt->digit_count++;
			if (qt->digit_count >= qt->escaped)
			{
				ADD_TOKEN_CHAR(hak, qt->c_acc);
				qt->escaped = 0;
			}
			goto consumed;
		}
		else
		{
			hak_ooch_t rc;
			rc = (qt->escaped == 2)? 'x':
			     (qt->escaped == 4)? 'u': 'U';
			if (qt->digit_count == 0)
				ADD_TOKEN_CHAR(hak, rc);
			else ADD_TOKEN_CHAR(hak, qt->c_acc);

			qt->escaped = 0;
		}
	}

	if (qt->escaped == 0 && c == qt->end_char)
	{
		/* terminating quote */

		/* qt->tok_type + qt->is_byte assumes that the token types for
		 * byte-string and byte-character literals are 1 greater than
		 * string and character literals. see the definition of
		 * hak_tok_type_t in hak-prv.h.
		 * qt->is_byte is always 0 for HAK_TOK_SYMLIT. */
		FEED_WRAP_UP(hak, (hak_tok_type_t)(qt->tok_type + qt->is_byte)); /* HAK_TOK_STRLIT or HAK_TOK_CHARLIT */
		if (TOKEN_NAME_LEN(hak) < qt->min_len) goto invalid_token;
		goto consumed;
	}

	if (qt->escaped == 0 && c == qt->esc_char)
	{
		qt->escaped = 1;
		goto consumed;
	}

	if (qt->escaped == 1)
	{
		if (c == 'a') c = '\a';
		else if (c == 'b') c = '\b';
		else if (c == 'f') c = '\f';
		else if (c == 'n') c = '\n';
		else if (c == 'r') c = '\r';
		else if (c == 't') c = '\t';
		else if (c == 'v') c = '\v';
		else if (c >= '0' && c <= '7' && !qt->regex)
		{
			/* i don't support the octal notation for a regular expression.
			 * it conflicts with the backreference notation between \1 and \7 inclusive. */
			qt->escaped = 3;
			qt->digit_count = 1;
			qt->c_acc = c - '0';
			goto consumed;
		}
		else if (c == 'x')
		{
			qt->escaped = 2;
			qt->digit_count = 0;
			qt->c_acc = 0;
			goto consumed;
		}
	#if (HAK_SIZEOF_OOCH_T >= 2)
		else if (c == 'u' && !qt->is_byte)
		{
			#if 0
			if (qt->is_byte)
			{
				synerr_loc = *FLX_LOC(hak);
				goto invalid_token;
			}
			#endif
			qt->escaped = 4;
			qt->digit_count = 0;
			qt->c_acc = 0;
			goto consumed;
		}
	#endif
	#if (HAK_SIZEOF_OOCH_T >= 4)
		else if (c == 'U' && !qt->is_byte)
		{
			#if 0
			if (qt->is_byte)
			{
				synerr_loc = *FLX_LOC(hak);
				goto invalid_token;
			}
			#endif
			qt->escaped = 8;
			qt->digit_count = 0;
			qt->c_acc = 0;
			goto consumed;
		}
	#endif
		else if (qt->regex)
		{
			/* if the following character doesn't compose a proper
			 * escape sequence, keep the escape character.
			 * an unhandled escape sequence can be handled
			 * outside this function since the escape character
			 * is preserved.*/
			ADD_TOKEN_CHAR(hak, qt->esc_char);
		}

		qt->escaped = 0;
	}

	ADD_TOKEN_CHAR(hak, c);

consumed:
	if (TOKEN_NAME_LEN(hak) > qt->max_len) goto invalid_token;
	return 1;

invalid_token:
	hak_setsynerr(hak, qt->synerr_code, &synerr_loc, HAK_NULL);
	return -1;
}

static int flx_signed_token (hak_t* hak, hak_ooci_t c)
{
	hak_flx_st_t* st = FLX_ST(hak);

	HAK_ASSERT(hak, st->char_count == 0);
	if (is_digit_char(c))
	{
		/* the sign is not part of the pn->digit_count[0] but is
		 * in the current token buffer. pn->digit_count[0] doesn't
		 * include the sign and calling init_flx_pn() to make it 0
		 * is good enough. */
		init_flx_pn(FLX_PN(hak), c);
		FEED_CONTINUE(hak, HAK_FLX_PLAIN_NUMBER);
		goto not_consumed;
	}
	else
	{
		init_flx_pi(FLX_PI(hak));

		/* the sign is already in the token name buffer.
		 * adjust the state data for the sign. */
		HAK_ASSERT(hak, TOKEN_NAME_LEN(hak) == 1);
		FLX_PI(hak)->char_count++;
		FLX_PI(hak)->seg_len++;

		/* let refeeding of 'c' happen at the next iteration */
		FEED_CONTINUE(hak, HAK_FLX_PLAIN_IDENT);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_bc_prefix (hak_t* hak, hak_ooci_t c)
{
	hak_flx_bcp_t* bcp = FLX_BCP(hak);


	if (c == '\"') /* b" B" c" C" */
	{
		int is_byte = (bcp->start_c == 'b' || bcp->start_c == 'B');
		reset_flx_token(hak);
		init_flx_qt(FLX_QT(hak), HAK_TOK_STRLIT, HAK_SYNERR_STRLIT, c, '\\', 0, HAK_TYPE_MAX(hak_oow_t), is_byte);
		FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* discard prefix, quote and move on */
		goto consumed;
	}
	else if (c == '\'') /* b' B' c' C' */
	{
		int is_byte = (bcp->start_c == 'b' || bcp->start_c == 'B');
		reset_flx_token(hak);
		init_flx_qt(FLX_QT(hak), HAK_TOK_CHARLIT, HAK_SYNERR_CHARLIT, c, '\\', 1, 1, is_byte);
		FEED_CONTINUE(hak, HAK_FLX_QUOTED_TOKEN); /* dicard prefix, quote, and move on */
		goto consumed;
	}
	else
	{
		/* not followed by a quote. switch to the plain identifier */
		init_flx_pi(FLX_PI(hak));

		/* the prefix is already in the token buffer. just adjust state data */
		FLX_PI(hak)->char_count++;
		FLX_PI(hak)->seg_len++;

		/* refeed c */
		FEED_CONTINUE(hak, HAK_FLX_PLAIN_IDENT);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}


/* ------------------------------------------------------------------------ */


static int feed_char (hak_t* hak, hak_ooci_t c)
{
/*hak_logbfmt(hak, HAK_LOG_STDERR, "FEED->[%jc] %d STATE->%d\n", c, c, FLX_STATE(hak));*/

	switch (FLX_STATE(hak))
	{
		case HAK_FLX_START:            return flx_start(hak, c);
		case HAK_FLX_BACKSLASHED:      return flx_backslashed(hak, c);
		case HAK_FLX_COMMENT:          return flx_comment(hak, c);
		case HAK_FLX_COLON_TOKEN:      return flx_colon_token(hak, c);
		case HAK_FLX_COLONEQ_TOKEN:    return flx_coloneq_token(hak, c);
		case HAK_FLX_DELIM_TOKEN:      return flx_delim_token(hak, c);
		case HAK_FLX_DOLLARED_IDENT:   return flx_dollared_ident(hak, c);
		case HAK_FLX_HMARKED_TOKEN:    return flx_hmarked_token(hak, c);
		case HAK_FLX_HMARKED_BC:       return flx_hmarked_bc(hak, c);
		case HAK_FLX_HMARKED_BINOP:    return flx_hmarked_binop(hak, c);
		case HAK_FLX_HMARKED_CHAR:     return flx_hmarked_char(hak, c);
		case HAK_FLX_HMARKED_IDENT:    return flx_hmarked_ident(hak, c);

		case HAK_FLX_PLAIN_IDENT:      return flx_plain_ident(hak, c);
		case HAK_FLX_PLAIN_NUMBER:     return flx_plain_number(hak, c);
		case HAK_FLX_QUOTED_TOKEN:     return flx_quoted_token(hak, c);
		case HAK_FLX_SIGNED_TOKEN:     return flx_signed_token(hak, c);
		case HAK_FLX_BC_PREFIX:        return flx_bc_prefix(hak, c);

		default:
			/* unknown state */
			break;
	}

	HAK_ASSERT(hak, !"internal error - this must never happen");
	hak_seterrbfmt(hak, HAK_EINTERN, "internal error - unknown flx state - %d", FLX_STATE(hak));
	return -1;
}

static void feed_update_lx_loc (hak_t* hak, hak_ooci_t ch)
{
	if (is_linebreak(ch))
	{
		hak->c->feed.lx.loc.line++;
		hak->c->feed.lx.loc.colm = 1;
	}
	else
	{
		hak->c->feed.lx.loc.colm++;
	}
}

static hak_oow_t move_cci_residue_bytes (hak_io_cciarg_t* curinp)
{
	hak_oow_t cpl;

	cpl = HAK_COUNTOF(curinp->rsd.buf) - curinp->rsd.len;
	if (cpl > 0)
	{
		hak_oow_t avail;
		avail = curinp->b.len - curinp->b.pos; /* available in the read buffer */
		if (cpl > avail) cpl = avail;
		HAK_MEMCPY(&curinp->rsd.buf[curinp->rsd.len], &curinp->buf.b[curinp->b.pos], cpl);
		curinp->rsd.len += cpl;
		curinp->b.pos += cpl; /* advance the position because the bytes moved to the residue buffer */
	}
	return curinp->rsd.len;
}

static int feed_from_includee (hak_t* hak)
{
	int x;
	hak_ooch_t c;
	hak_io_cciarg_t* curinp;

	HAK_ASSERT(hak, hak->c->curinp != HAK_NULL && hak->c->curinp != &hak->c->cci_arg);

	curinp = hak->c->curinp;
	do
	{
		hak_oow_t taken;

	#if defined(HAK_OOCH_IS_UCH)
		if (curinp->byte_oriented)
		{
			hak_cmgr_t* cmgr;
			const hak_uint8_t* inpptr;
			hak_oow_t inplen, n;

			cmgr = HAK_CMGR(hak);

		start_over:
			if (curinp->b.pos >= curinp->b.len)
			{
				x = hak->c->cci_rdr(hak, HAK_IO_READ_BYTES, curinp);
				if (x <= -1)
				{
					const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
					hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to read bytes from %js - %js", curinp->name, orgmsg);
					goto oops;
				}

				if (curinp->xlen <= 0)
				{
					/* got EOF from an included stream */
					if (curinp->rsd.len > 0)
					{
						hak_seterrbfmt(hak, HAK_EECERR, "incomplete byte sequence in %js", curinp->name);
						goto oops;
					}
					feed_end_include(hak);
					curinp = hak->c->curinp;
					continue;
				}

				curinp->b.pos = 0;
				curinp->b.len = curinp->xlen;
			}

			if (curinp->rsd.len > 0)
			{
				/* there is data in the residue buffer. use the residue buffer to
				 * locate a proper multi-byte sequence */
				HAK_ASSERT(hak, curinp->b.pos == 0);
				inplen = move_cci_residue_bytes(curinp);
				inpptr = &curinp->rsd.buf[0];
			}
			else
			{
				inplen = curinp->b.len - curinp->b.pos;
				inpptr = &curinp->buf.b[curinp->b.pos];
			}

			n = cmgr->bctouc((const hak_bch_t*)inpptr, inplen, &c);
			if (n == 0) /* invalid sequence */
			{
				/* TODO: more accurate location of the invalid byte sequence */
				hak_seterrbfmt(hak, HAK_EECERR, "invalid byte sequence in %js", curinp->name);
				goto oops;
			}
			if (n > inplen) /* incomplete sequence */
			{
				HAK_ASSERT(hak, curinp->rsd.len < HAK_COUNTOF(curinp->rsd.buf));
				move_cci_residue_bytes (curinp);
				goto start_over;
			}

			if (curinp->rsd.len > 0)
			{
				/* move_cci_residue_bytes() advanced curinp->b.pos without checking
				 * the needed number of bytes to form a character. it must backoff by
				 * the number of excessive bytes moved to the residue buffer */
				curinp->b.pos -= curinp->rsd.len - n;
				taken = 0;  /* treat it as if no bytes are taken in this case */
			}
			else
			{
				taken = n;
			}
		}
		else
		{
	#endif
			if (curinp->b.pos >= curinp->b.len)
			{
				x = hak->c->cci_rdr(hak, HAK_IO_READ, curinp);
				if (x <= -1)
				{
					/* TODO: more accurate location of failure */
					const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
					hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to read %js - %js", curinp->name, orgmsg);
					goto oops;
				}
				if (curinp->xlen <= 0)
				{
					/* got EOF from an included stream */
					feed_end_include(hak);
					curinp = hak->c->curinp;
					continue;
				}

				curinp->b.pos = 0;
				curinp->b.len = curinp->xlen;
			}

			c = curinp->buf.c[curinp->b.pos];
			taken = 1;
	#if defined(HAK_OOCH_IS_UCH)
		}
	#endif

		x = feed_char(hak, c);
		if (x <= -1) goto oops;
		if (x >= 1)
		{
			/* consumed */
			feed_update_lx_loc(hak, c);
			curinp->b.pos += taken;
	#if defined(HAK_OOCH_IS_UCH)
			curinp->rsd.len = 0; /* clear up the residue byte buffer. needed for byte reading only */
	#endif
		}

		if (hak->c->feed.rd.do_include_file)
		{
			/* feed_process_token(), called for the "filename" token for the #include
			 * directive, sets hak->c->feed.rd.do_include_file to 1 instead of attepmting
			 * to include the file. the file inclusion is attempted here after the return
			 * value of feed_char() is used to advance the hak->c->curinp->b.pos pointer. */
			hak->c->feed.rd.do_include_file = 0; /* clear this regardless of inclusion result */
			if (feed_begin_include(hak) <= -1) goto oops;
			curinp = hak->c->curinp;
		}
	}
	while (curinp != &hak->c->cci_arg);

	return 0;

oops:
	while (feed_end_include(hak) >= 1) /* loop */; /* roll back the entire inclusion chain */
	return -1;
}

int hak_beginfeed (hak_t* hak, hak_on_cnode_t on_cnode)
{
	/* if the fed data contains #include, you must call hak_attachccio() first.
	 * file includsion requires the ccio handler to be implemented. */

	if (!hak->c && init_compiler(hak) <= -1) return -1;

	init_feed(hak);
	if (on_cnode) hak->c->feed.on_cnode = on_cnode;
	/* if you pass HAK_NULL for on_cnode, hak->c->feed.on_cnode resets
	 * back to the default handler in init_feed() */

	return 0;
}

int hak_endfeed (hak_t* hak)
{
	return hak_feed(hak, HAK_NULL, 0);
}

int hak_feedpending (hak_t* hak)
{
	return !(hak->c->r.st == HAK_NULL && FLX_STATE(hak) == HAK_FLX_START);
}

void hak_getfeedloc (hak_t* hak, hak_loc_t* loc)
{
	*loc = hak->c->feed.lx.loc;
}

void hak_resetfeedloc (hak_t* hak)
{
	hak->c->feed.lx.loc.line = 1;
	hak->c->feed.lx.loc.colm = 1;
	hak->c->feed.lx.loc.file = HAK_NULL;
}

void hak_resetfeed (hak_t* hak)
{
	feed_reset_reader_state(hak);
	feed_clean_up_reader_stack(hak);
	feed_continue(hak, HAK_FLX_START);
	hak_resetfeedloc(hak);
}

int hak_feed (hak_t* hak, const hak_ooch_t* data, hak_oow_t len)
{
/* TODO: need to return the number of processed characters?
 *       need to stop after the first complete expression? */
	hak_oow_t i;
	int x;

	HAK_ASSERT(hak, hak->c != HAK_NULL);

#if defined(HAK_OOCH_IS_UCH)
	if (hak->c->feed.rsd.len > 0 && !hak->c->feed.rsd.no_check)
	{
		hak_seterrbfmt(hak, HAK_EPERM, "feed disallowed for incomplete sequence pending more feeding");
		return -1;
	}
#endif

	if (data)
	{
		for (i = 0; i < len; )
		{
			x = feed_char(hak, data[i]);
			if (x <= -1)
			{
				feed_update_lx_loc(hak, data[i]); /* update the location upon an error too */
				goto oops; /* TODO: return the number of processed characters via an argument? */
			}

			if (x > 0)
			{
				/* consumed */
				feed_update_lx_loc(hak, data[i]);
				i += x; /* x is supposed to be 1. otherwise, some characters may get skipped. */
			}

			if (hak->c->feed.rd.do_include_file)
			{
				hak->c->feed.rd.do_include_file = 0; /* done regardless of inclusion result */
				if (feed_begin_include(hak) <= -1) goto oops;
			}

			if (hak->c->curinp && hak->c->curinp != &hak->c->cci_arg && feed_from_includee(hak) <= -1)
			{
				/* TODO: return the number of processed characters via an argument? */
				goto oops;
			}

			/* feed data[i] again if not consumed */
		}
	}
	else
	{
		for (i = 0; i < 1;) /* weird loop in case feed_char() returns 0 */
		{
			x = feed_char(hak, HAK_OOCI_EOF);
			if (x <= -1) goto oops;
			i += x;
		}
	}

	return 0;

oops:
	feed_reset_reader_state(hak);

	/* if enter_list() is in feed_process_token(), the stack grows.
	 * leave_list() pops an element off the stack. the stack can be
	 * not empty if an error occurs outside feed_process_token() after
	 * leave_list() in it. for example,
	 *
	 *  (              #aaa
	 *  ^              ^
	 *  leave_list()   error in flx_hmarked_ident() before a full cnode is processed
	 */
	feed_clean_up_reader_stack(hak);
	feed_continue(hak, HAK_FLX_START);
	return -1;
}

int hak_feedbchars (hak_t* hak, const hak_bch_t* data, hak_oow_t len)
{
#if defined(HAK_OOCH_IS_UCH)
	hak_uch_t outbuf[128];
	hak_oow_t inlen, outlen, inpos, brwlen;
	int n;

	HAK_ASSERT(hak, hak->c != HAK_NULL);

	inpos = 0;

	if (hak->c->feed.rsd.len > 0) /* residue length greater than 0 */
	{
		hak_oow_t rsdlen;

		/* handle the residue bytes from the previous feeding */
		rsdlen = hak->c->feed.rsd.len; /* original residue length*/
		brwlen = HAK_COUNTOF(hak->c->feed.rsd.buf) - rsdlen;
		if (len < brwlen) brwlen = len;
		HAK_MEMCPY(&hak->c->feed.rsd.buf[rsdlen], data, brwlen);
		hak->c->feed.rsd.len += brwlen;

		inlen = hak->c->feed.rsd.len;
		outlen = 1; /* ensure that it can only convert 1 character */
		n = hak_conv_bchars_to_uchars_with_cmgr(hak->c->feed.rsd.buf, &inlen, outbuf, &outlen, HAK_CMGR(hak), 0);

		if (outlen > 0)
		{
			int x;
			hak->c->feed.rsd.no_check = 1;
			x = hak_feed(hak, outbuf, outlen);
			hak->c->feed.rsd.no_check = 0;
			if (x <= -1) return -1;
		}

		if (n <= -1)
		{
			if (n == -3 || (n == -2 && outlen > 0))
			{
				/* n == -3. invalid sequence. more feeding is required */
				/* n == -2. there were extra bytes for the second character in the input */
				HAK_ASSERT(hak, (n == -3 && inlen == 0 && outlen == 0) || (n == -2 && inlen > 0));
				/* nothing to do. carry on */
			}
			else
			{
				hak_seterrnum(hak, (n == -2)? HAK_EBUFFULL: HAK_EECERR);
				return -1;
			}
		}

		/*
		 * | rsdlen   |  brwlen  |
		 * | inlen        |
		 */
		if (inlen < rsdlen)
		{
			HAK_ASSERT(hak, inlen == 0);
			HAK_ASSERT(hak, brwlen ==  len);
			/* brwlen needs no change */
			/* hak->c->feed.rsd.len nees no change */
		}
		else
		{
			HAK_ASSERT(hak, inlen > rsdlen);
			brwlen = inlen - rsdlen; /* actual bytes borrowed and converted */
			hak->c->feed.rsd.len = 0;
		}
		inpos += brwlen;
		len -= brwlen;
	}

	while (len > 0)
	{
		inlen = len;
		outlen = HAK_COUNTOF(outbuf);

		/* hak_convbtouchars() does not differentiate between illegal character and incomplete sequence.
		 * use a lower-level function that hak_convbtouchars() uses */
		n = hak_conv_bchars_to_uchars_with_cmgr(&data[inpos], &inlen, outbuf, &outlen, HAK_CMGR(hak), 0);
		if (outlen > 0 && hak_feed(hak, outbuf, outlen) <= -1) return -1;

		if (n <= -1)
		{
			if (n == -2 && outlen > 0) goto ok;

			if (n == -2 || n == -3)
			{
				hak_oow_t rsdlen;
				HAK_ASSERT(hak, len > inlen);
				rsdlen = len - inlen;
				HAK_ASSERT(hak, rsdlen <= HAK_COUNTOF(hak->c->feed.rsd.buf));
				HAK_MEMCPY(hak->c->feed.rsd.buf, &data[inpos + inlen], rsdlen);
				hak->c->feed.rsd.len = len - inlen;
				break;
			}

			hak_seterrnum(hak, HAK_EECERR);
			return -1;
		}

	ok:
		inpos += inlen;
		len -= inlen;
	}

	return 0;
#else
	return hak_feed(hak, data, len);
#endif
}

int hak_feeduchars (hak_t* hak, const hak_uch_t* data, hak_oow_t len)
{
#if defined(HAK_OOCH_IS_UCH)
	return hak_feed(hak, data, len);
#else
	hak_bch_t outbuf[HAK_BCSIZE_MAX * 128];
	hak_oow_t inlen, outlen, inpos;
	int n;

	inpos = 0;
	while (len > 0)
	{
		inlen = len;
		outlen = HAK_COUNTOF(outbuf);
		n = hak_convutobchars(hak, &data[inpos], &inlen, outbuf, &outlen);
		if (outlen > 0 && hak_feed(hak, outbuf, outlen) <= -1) return -1;
		inpos += inlen;
		len -= inlen;
		if (n <= -1) return -1;
	}
	return 0;
#endif
}

/*
hak_setopt (ON_EXPRESSION CALLBACK??? );



hak_feed(hak, "(hello) (10)", 12);
	> on_token
	> on_expression
	> on_eof

default callback for on_expression?
	compile
	execute??/ if in the interactive mode? (say it's used as a network protocol. execute each expression when received....)

default callback for on_eof?
 	execute or terminate?


*/


/* ------------------------------------------------------------------------ */

/* TODO: rename compiler to something else that can include reader, udo_wrtr, and compiler
 * move compiler intialization/finalization here to more common place */

static void gc_compiler_cb (hak_t* hak)
{
	if (hak->c)
	{
		hak->c->r.s = hak_moveoop(hak, hak->c->r.s);
		hak->c->r.e = hak_moveoop(hak, hak->c->r.e);
	}
}

static void fini_compiler_cb (hak_t* hak)
{
	/* called before the hak object is closed */
	if (hak->c)
	{
		if (hak->c->cfs.ptr)
		{
			hak_freemem(hak, hak->c->cfs.ptr);
			hak->c->cfs.ptr = HAK_NULL;
			hak->c->cfs.top = -1;
			hak->c->cfs.capa = 0;
		}

		if (hak->c->tv.s.ptr)
		{
			hak_freemem(hak, hak->c->tv.s.ptr);
			hak->c->tv.s.ptr = HAK_NULL;
			hak->c->tv.s.len = 0;
			hak->c->tv.capa = 0;
			hak->c->tv.wcount = 0;
		}
		HAK_ASSERT(hak, hak->c->tv.capa == 0);
		HAK_ASSERT(hak, hak->c->tv.wcount == 0);

		if (hak->c->ctlblk.info)
		{
			hak_freemem(hak, hak->c->ctlblk.info);
			hak->c->ctlblk.info = HAK_NULL;
			hak->c->ctlblk.info_capa = 0;
			hak->c->ctlblk.depth = -1;
		}

		if (hak->c->clsblk.info)
		{
			hak_freemem(hak, hak->c->clsblk.info);
			hak->c->clsblk.info = HAK_NULL;
			hak->c->clsblk.info_capa = 0;
			hak->c->clsblk.depth = -1;
		}

		if (hak->c->funblk.info)
		{
			hak_freemem(hak, hak->c->funblk.info);
			hak->c->funblk.info = HAK_NULL;
			hak->c->funblk.info_capa = 0;
			hak->c->funblk.depth = -1;
		}

		clear_sr_names(hak);
		if (hak->c->tok.name.ptr) hak_freemem(hak, hak->c->tok.name.ptr);

		hak_detachccio(hak);

		hak_freemem(hak, hak->c);
		hak->c = HAK_NULL;
	}
}

static void fini_compiler (hak_t* hak)
{
	/* unlike fini_compiler_cb(), this is to be used in some error handling
	 * between init_compiler success and subquent operation failure */
	if (hak->c)
	{
		hak_deregcb(hak, hak->c->cbp);
		fini_compiler_cb(hak);
	}
}

static int init_compiler (hak_t* hak)
{
	hak_cb_t cb, * cbp = HAK_NULL;

	HAK_ASSERT(hak, hak->c == HAK_NULL);

	HAK_MEMSET(&cb, 0, HAK_SIZEOF(cb));
	cb.on_gc = gc_compiler_cb;
	cb.on_fini = fini_compiler_cb;
	cbp = hak_regcb(hak, &cb);
	if (HAK_UNLIKELY(!cbp)) return -1;

	hak->c = (hak_compiler_t*)hak_callocmem(hak, HAK_SIZEOF(*hak->c));
	if (HAK_UNLIKELY(!hak->c))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to allocate compiler - %js", orgmsg);
		hak_deregcb(hak, cbp);
		return -1;
	}

	hak->c->ilchr_ucs.ptr = &hak->c->ilchr;
	hak->c->ilchr_ucs.len = 1;

	hak->c->r.s = hak->_nil;
	hak->c->r.e = hak->_nil;

	hak->c->cfs.top = -1;
	hak->c->ctlblk.depth = -1;
	hak->c->clsblk.depth = -1;
	hak->c->funblk.depth = -1;

	init_feed(hak);
	hak->c->cbp = cbp;


	/* initialize the internal cons to represent a cell pointing to `null` in the `car` part */
	hak->c->fake_cnode.nil.cn_type = HAK_CNODE_NIL;
	hak->c->fake_cnode.nil.cn_tok.ptr = vocas[VOCA_KW_NIL].str;
	hak->c->fake_cnode.nil.cn_tok.len = vocas[VOCA_KW_NIL].len;

	hak->c->fake_cnode.cons_to_nil.cn_type = HAK_CNODE_CONS;
	hak->c->fake_cnode.cons_to_nil.u.cons.car = &hak->c->fake_cnode.nil;
	hak->c->fake_cnode.cons_to_nil.u.cons.cdr = HAK_NULL;
	return 0;
}

int hak_attachccio (hak_t* hak, hak_io_impl_t cci_rdr)
{
	int n;
	int inited_compiler = 0;
	hak_io_cciarg_t new_cciarg;

	if (!hak->c)
	{
		if (init_compiler(hak) <= -1) return -1;
		inited_compiler = 1;
	}

	if (cci_rdr)
	{
		/* The name field and the includer field are HAK_NULL
		 * for the main stream */
		HAK_MEMSET(&new_cciarg, 0, HAK_SIZEOF(new_cciarg));
		new_cciarg.line = 1;
		new_cciarg.colm = 1;

		/* open the top-level source input stream */
		n = cci_rdr(hak, HAK_IO_OPEN, &new_cciarg);
		if (n <= -1) goto oops;

		if (hak->c->cci_rdr)
		{
			/* close the old source input stream */
			hak->c->cci_rdr(hak, HAK_IO_CLOSE, &hak->c->cci_arg);
		}
		hak->c->cci_rdr = cci_rdr;
		hak->c->cci_arg = new_cciarg;

		/* clear unneeded source stream names */
		/*clear_sr_names(hak); <---- TODO: tricky to clean up here */

		/* initialize some other key fields */
		hak->c->nungots = 0;
		/* the source stream is open. set it as the current input stream */
		hak->c->curinp = &hak->c->cci_arg;
	}

	return 0;

oops:
	if (inited_compiler) fini_compiler(hak);
	return -1;
}

void hak_detachccio (hak_t* hak)
{
	/* an error occurred and control has reached here
	 * probably, some included files might not have been
	 * closed. close them */

	if (hak->c)
	{
		if (hak->c->cci_rdr)
		{
			while (hak->c->curinp != &hak->c->cci_arg)
			{
				hak_io_cciarg_t* prev;

				/* nothing much to do about a close error */
				hak->c->cci_rdr(hak, HAK_IO_CLOSE, hak->c->curinp);

				prev = hak->c->curinp->includer;
				HAK_ASSERT(hak, hak->c->curinp->name != HAK_NULL);
				hak_freemem(hak, hak->c->curinp);
				hak->c->curinp = prev;
			}

			hak->c->cci_rdr(hak, HAK_IO_CLOSE, hak->c->curinp);
			hak->c->cci_rdr = HAK_NULL; /* ready for another attachment */
		}
	}
}

int hak_attachudio (hak_t* hak, hak_io_impl_t udi_rdr, hak_io_impl_t udo_wrtr)
{
	int n;
	hak_io_udiarg_t new_udiarg;
	hak_io_udoarg_t new_udoarg;

	if (udi_rdr)
	{
		HAK_MEMSET(&new_udiarg, 0, HAK_SIZEOF(new_udiarg));
		n = udi_rdr(hak, HAK_IO_OPEN, &new_udiarg);
		if (n <= -1)
		{
			goto oops;
		}
	}

	if (udo_wrtr)
	{
		/* open the new output stream */
		HAK_MEMSET(&new_udoarg, 0, HAK_SIZEOF(new_udoarg));
		n = udo_wrtr(hak, HAK_IO_OPEN, &new_udoarg);
		if (n <= -1)
		{
			if (udi_rdr) udi_rdr(hak, HAK_IO_CLOSE, &new_udiarg);
			goto oops;
		}
	}

	if (udi_rdr)
	{
		if (hak->io.udi_rdr)
		{
			/* close the old input stream */
			hak->io.udi_rdr(hak, HAK_IO_CLOSE, &hak->io.udi_arg);
		}
		hak->io.udi_rdr = udi_rdr;
		hak->io.udi_arg = new_udiarg;
	}

	if (udo_wrtr)
	{
		if (hak->io.udo_wrtr)
		{
			/* close the old output stream */
			hak->io.udo_wrtr(hak, HAK_IO_CLOSE, &hak->io.udo_arg);
		}
		hak->io.udo_wrtr = udo_wrtr;
		hak->io.udo_arg = new_udoarg;
	}

	return 0;

oops:
	return -1;
}


void hak_detachudio (hak_t* hak)
{
	if (hak->io.udi_rdr)
	{
		hak->io.udi_rdr(hak, HAK_IO_CLOSE, &hak->io.udi_arg);
		hak->io.udi_rdr = HAK_NULL; /* ready for another attachment */
	}

	if (hak->io.udo_wrtr)
	{
		hak->io.udo_wrtr(hak, HAK_IO_CLOSE, &hak->io.udo_arg);
		hak->io.udo_wrtr = HAK_NULL; /* ready for another attachment */
	}
}

void hak_flushudio (hak_t* hak)
{
	if (hak->io.udo_wrtr) hak->io.udo_wrtr(hak, HAK_IO_FLUSH, &hak->io.udo_arg);
}

/* TODO: discard the following three functions - hak_setbasesrloc, hak_readbasesrchar */
void hak_setbasesrloc (hak_t* hak, hak_oow_t line, hak_oow_t colm)
{
	hak->c->cci_arg.line = line;
	hak->c->cci_arg.colm = colm;
}

hak_lxc_t* hak_readbasesrchar (hak_t* hak)
{
	/* read a character using the base input stream. the caller must care extra
	 * care when using this function. this function reads the main stream regardless
	 * of the inclusion status and ignores the ungot characters. */
	int n = _get_char(hak, &hak->c->cci_arg);
	if (n <= -1) return HAK_NULL;
	return &hak->c->cci_arg.lxc;
}
