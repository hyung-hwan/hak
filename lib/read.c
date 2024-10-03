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

#include "hcl-prv.h"

#define HCL_LANG_ENABLE_WIDE_DELIM
#define HCL_LANG_AUTO_FORGE_XLIST_ALWAYS

#define BUFFER_ALIGN 128
#define BALIT_BUFFER_ALIGN 128
#define SALIT_BUFFER_ALIGN 128
#define ARLIT_BUFFER_ALIGN 128

static struct voca_t
{
	hcl_oow_t len;
	hcl_ooch_t str[11];
} vocas[] =
{
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
	{  4, { 'p','l','u','s'                                               } },
	{  3, { 's','e','t'                                                   } },
	{  5, { 's','e','t','-','r'                                           } },

	{  3, { '(',' ',')'      /* XLIST */                                  } },
	{  4, { '(',':',' ',')'  /* MLIST */                                  } },
	{  4, { '(',':','=',')'  /* ALIST */                                  } },
	{  4, { '(','B','O',')'  /* BLIST */                                  } },
	{  3, { '{',' ','}'      /* BLOCK */                                  } },
	{  4, { '#','[',' ',']'  /* ARRAY */                                  } },
	{  5, { '#','b','[',' ',']' /* BYTE ARRAY */                          } },
	{  5, { '#','c','[',' ',']' /* CHAR ARRAY */                          } },
	{  4, { '#','{',' ','}' /* DICTIONARY */                              } },
	{  4, { '#','(',' ',')' /* QLIST */                                   } },
	{  3, { '[',' ',']' /* TUPLE */                                       } },
	{  3, { '|',' ','|' /* VLIST */                                       } },

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
	VOCA_KW_PLUS,
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
	hcl_synerrnum_t synerr;
	int             voca_id;
} cons_info[] =
{
	HCL_AID(HCL_CONCODE_XLIST)     { HCL_TOK_RPAREN, HCL_SYNERR_RPAREN, VOCA_XLIST }, /* XLIST     ( )  */
	HCL_AID(HCL_CONCODE_MLIST)     { HCL_TOK_RPAREN, HCL_SYNERR_RPAREN, VOCA_MLIST }, /* MLIST     (obj:message) */
	HCL_AID(HCL_CONCODE_ALIST)     { HCL_TOK_RPAREN, HCL_SYNERR_RPAREN, VOCA_ALIST }, /* ALIST     (var:=value) */
	HCL_AID(HCL_CONCODE_BLIST)     { HCL_TOK_RPAREN, HCL_SYNERR_RPAREN, VOCA_BLIST }, /* BLIST     (x + y) */
	HCL_AID(HCL_CONCODE_BLOCK)     { HCL_TOK_RBRACE, HCL_SYNERR_RBRACE, VOCA_BLOCK }, /* BLOCK     { } */
	HCL_AID(HCL_CONCODE_ARRAY)     { HCL_TOK_RBRACK, HCL_SYNERR_RBRACK, VOCA_ARRAY }, /* ARRAY     #[ ] */
	HCL_AID(HCL_CONCODE_BYTEARRAY) { HCL_TOK_RBRACK, HCL_SYNERR_RBRACK, VOCA_BYTEARRAY }, /* BYTEARRAY #b[ ] */
	HCL_AID(HCL_CONCODE_CHARARRAY) { HCL_TOK_RBRACK, HCL_SYNERR_RBRACK, VOCA_CHARARRAY }, /* CHARARRAY #c[ ] */
	HCL_AID(HCL_CONCODE_DIC)       { HCL_TOK_RBRACE, HCL_SYNERR_RBRACE, VOCA_DIC }, /* DIC       #{ } */
	HCL_AID(HCL_CONCODE_QLIST)     { HCL_TOK_RPAREN, HCL_SYNERR_RPAREN, VOCA_QLIST }, /* QLIST     #( )  */
	HCL_AID(HCL_CONCODE_TUPLE)     { HCL_TOK_RBRACK, HCL_SYNERR_RBRACK, VOCA_TUPLE }, /* TUPLE [] */

	/* VLIST's closer and synerr are not used. there is dedicated logic in feed_process_token(). only voca_id is used */
	HCL_AID(HCL_CONCODE_VLIST)     { HCL_TOK_VBAR,   HCL_SYNERR_VBAR,   VOCA_VLIST }  /* VLIST     | |  */
};

/* ----------------------------------------------------------------- */

static int init_compiler (hcl_t* hcl);
static void feed_continue (hcl_t* hcl, hcl_flx_state_t state);
static int is_at_block_beginning (hcl_t* hcl);

/* ----------------------------------------------------------------- */

static HCL_INLINE int is_spacechar (hcl_ooci_t c)
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
	return c != HCL_OOCI_EOF && hcl_is_ooch_space(c);
#endif
}

static HCL_INLINE int is_linebreak (hcl_ooci_t c)
{
	/* TODO: different line end conventions? */
	return c == '\n'; /* make sure this is one of the space chars in is_spacechar() */
}

static HCL_INLINE int is_digit_char (hcl_ooci_t c)
{
	return (c >= '0' && c <= '9');
}

static HCL_INLINE int is_radixed_digit_char (hcl_ooci_t c, int radix)
{
	if (c >= '0' && c <= '9') return (c - '0') < radix;
	if (c >= 'a' && c <= 'z') return (c - 'a' + 10) < radix;
	if (c >= 'A' && c <= 'Z') return (c - 'A' + 10) < radix;
	return 0;
}

static HCL_INLINE int is_xdigit_char (hcl_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

#if 0
static HCL_INLINE int is_alphachar (hcl_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static HCL_INLINE int is_alnumchar (hcl_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
}
#endif

static HCL_INLINE int is_delim_char (hcl_ooci_t c)
{
	return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' ||
	       c == '|' || c == ',' || c == '.' || c == ':' || c == ';' ||
	       /* the first characters of tokens in delim_token_tab up to this point */
#if defined(HCL_OOCH_IS_UCH) && defined(HCL_LANG_ENABLE_WIDE_DELIM)
	       c == L'\u201C' || c == L'\u201D' ||  /* “ ” */
	       c == L'\u2018' || c == L'\u2019' ||  /* ‘ ’ */
#endif
	       c == '#' || c == '\"' || c == '\'' || c == '\\' || is_spacechar(c) || c == HCL_OOCI_EOF;
}


int hcl_is_binop_char (hcl_ooci_t c) /* not static HCL_INLINE for shared use with comp.c via HCL_CNODE_IS_SYMBOL_PLAIN() */
{
	return c == '&' || c == '*' || c == '+' || c == '-' || c == '/' || c == '%' ||
	       c == '<' || c == '>' || c == '=' || c == '@' || c == '|' || c == '~';
}
#define is_binop_char(c) hcl_is_binop_char(c)

static HCL_INLINE int is_lead_ident_char (hcl_ooci_t c)
{
	return hcl_is_ooch_alpha(c) || c == '_';
}

static HCL_INLINE int is_ident_char (hcl_ooci_t c)
{
	/* [NOTE]
	 *  '-' is prohibited as the last character of an identifier or an identifier segment.
	 *  see flx_plain_ident().
	 */
	return hcl_is_ooch_alnum(c) || c == '_' || c == '-' || c == '?';
}

/* TODO: remove GET_CHAR(), GET_CHAR_TO(), get_char(), _get_char() */
#define GET_CHAR(hcl) \
	do { if (get_char(hcl) <= -1) return -1; } while (0)

#define GET_CHAR_TO(hcl,c) \
	do { \
		if (get_char(hcl) <= -1) return -1; \
		c = (hcl)->c->lxc.c; \
	} while(0)


#define ADD_TOKEN_STR(hcl,s,l) \
	do { if (add_token_str(hcl, s, l) <= -1) return -1; } while (0)

#define ADD_TOKEN_CHAR(hcl,c) \
	do { if (add_token_char(hcl, c) <= -1) return -1; } while (0)

#define CLEAR_TOKEN_NAME(hcl) ((hcl)->c->tok.name.len = 0)
#define SET_TOKEN_TYPE(hcl,tv) ((hcl)->c->tok.type = (tv))
#define SET_TOKEN_LOC(hcl,locv) ((hcl)->c->tok.loc = *(locv))

#define TOKEN_TYPE(hcl) ((hcl)->c->tok.type)
#define TOKEN_NAME(hcl) (&(hcl)->c->tok.name)
#define TOKEN_NAME_CAPA(hcl) ((hcl)->c->tok.name_capa)
#define TOKEN_NAME_LEN(hcl) ((hcl)->c->tok.name.len)
#define TOKEN_NAME_PTR(hcl) ((hcl)->c->tok.name.ptr)
#define TOKEN_NAME_CHAR(hcl,index) ((hcl)->c->tok.name.ptr[index])
#define TOKEN_LOC(hcl) (&(hcl)->c->tok.loc)

static HCL_INLINE int add_token_str (hcl_t* hcl, const hcl_ooch_t* ptr, hcl_oow_t len)
{
	hcl_oocs_t tmp;
	tmp.ptr = (hcl_ooch_t*)ptr;
	tmp.len = len;
	return hcl_copy_string_to(hcl, &tmp, TOKEN_NAME(hcl), &TOKEN_NAME_CAPA(hcl), 1, '\0');
}

static HCL_INLINE int does_token_name_match (hcl_t* hcl, voca_id_t id)
{
	return hcl->c->tok.name.len == vocas[id].len &&
	       hcl_equal_oochars(hcl->c->tok.name.ptr, vocas[id].str, vocas[id].len);
}

static HCL_INLINE int add_token_char (hcl_t* hcl, hcl_ooch_t c)
{
	hcl_oocs_t tmp;

	tmp.ptr = &c;
	tmp.len = 1;
	return hcl_copy_string_to(hcl, &tmp, TOKEN_NAME(hcl), &TOKEN_NAME_CAPA(hcl), 1, '\0');
}

static HCL_INLINE void unget_char (hcl_t* hcl, const hcl_lxc_t* c)
{
	/* Make sure that the unget buffer is large enough */
	HCL_ASSERT (hcl, hcl->c->nungots < HCL_COUNTOF(hcl->c->ungot));
	hcl->c->ungot[hcl->c->nungots++] = *c;
}

static int get_directive_token_type (hcl_t* hcl, hcl_tok_type_t* tok_type)
{
	if (does_token_name_match(hcl, VOCA_INCLUDE))
	{
		*tok_type = HCL_TOK_INCLUDE;
		return 0;
	}
	else if (does_token_name_match(hcl, VOCA_PRAGMA))
	{
		*tok_type = HCL_TOK_PRAGMA;
		return 0;
	}

	return -1;
}

static int _get_char (hcl_t* hcl, hcl_io_cciarg_t* inp)
{
	hcl_ooci_t lc;

	if (inp->b.pos >= inp->b.len)
	{
		if (hcl->c->cci_rdr(hcl, HCL_IO_READ, inp) <= -1) return -1;

		if (inp->xlen <= 0)
		{
			inp->lxc.c = HCL_OOCI_EOF;
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
		 * hcl->cu->curinp->colm has been incremented when the previous
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

static int get_char (hcl_t* hcl)
{
	int n;

	if (hcl->c->nungots > 0)
	{
		/* something in the unget buffer */
		hcl->c->lxc = hcl->c->ungot[--hcl->c->nungots];
		return 0;
	}

	n = _get_char(hcl, hcl->c->curinp);
	if (n >= 0) hcl->c->lxc = hcl->c->curinp->lxc;
	return n;
}

static hcl_tok_type_t classify_ident_token (hcl_t* hcl, const hcl_oocs_t* v)
{
	hcl_oow_t i;
	static struct
	{
		int voca_id;
		hcl_tok_type_t type;
	} tab[] =
	{
		{ VOCA_KW_NIL,      HCL_TOK_NIL      },
		{ VOCA_KW_TRUE,     HCL_TOK_TRUE     },
		{ VOCA_KW_FALSE,    HCL_TOK_FALSE    },
		{ VOCA_KW_SELF,     HCL_TOK_SELF     },
		{ VOCA_KW_SUPER,    HCL_TOK_SUPER    },

		{ VOCA_KW_CLASS,    HCL_TOK_CLASS    },
		{ VOCA_KW_FUN,      HCL_TOK_FUN      },
		{ VOCA_KW_DO,       HCL_TOK_DO       },
		{ VOCA_KW_IF,       HCL_TOK_IF       },
		{ VOCA_KW_ELIF,     HCL_TOK_ELIF     },
		{ VOCA_KW_ELSE,     HCL_TOK_ELSE     },
		{ VOCA_KW_THROW,    HCL_TOK_THROW    },
		{ VOCA_KW_TRY,      HCL_TOK_TRY      },
		{ VOCA_KW_CATCH,    HCL_TOK_CATCH    },
		{ VOCA_KW_BREAK,    HCL_TOK_BREAK    },
		{ VOCA_KW_CONTINUE, HCL_TOK_CONTINUE },
		{ VOCA_KW_UNTIL,    HCL_TOK_UNTIL    },
		{ VOCA_KW_WHILE,    HCL_TOK_WHILE    },
		{ VOCA_KW_RETURN,   HCL_TOK_RETURN   },
		{ VOCA_KW_REVERT,   HCL_TOK_REVERT   },
		{ VOCA_KW_AND,      HCL_TOK_AND      },
		{ VOCA_KW_OR,       HCL_TOK_OR       },
		{ VOCA_KW_PLUS,     HCL_TOK_PLUS     },
		{ VOCA_KW_SET,      HCL_TOK_SET      },
		{ VOCA_KW_SET_R,    HCL_TOK_SET_R    }
	};

	for (i = 0; i < HCL_COUNTOF(tab); i++)
	{
		int vid = tab[i].voca_id;
		if (hcl_comp_oochars(v->ptr, v->len, vocas[vid].str, vocas[vid].len) == 0) return tab[i].type;
	}

	return HCL_TOK_IDENT;
}

static int is_sr_name_in_use (hcl_t* hcl, const hcl_ooch_t* sr_name)
{
	/* [NOTE]
	 *  this is very error prone. if there are changes in refernece
	 *  points of this sr_name in the source code, this function also
	 *  must be modifed. */
	hcl_io_cciarg_t* cur;

	if (hcl->c->synerr.loc.file == sr_name) return 1;

	cur = hcl->c->curinp;
	while (cur)
	{
		if (cur->lxc.l.file == sr_name) return 1;
		cur = cur->includer;
	}
	return 0;
}

static void clear_sr_names (hcl_t* hcl)
{
	hcl_link_t* cur;

	HCL_ASSERT (hcl, hcl->c != HCL_NULL);

	while (hcl->c->sr_names)
	{
		cur = hcl->c->sr_names;
		hcl->c->sr_names = cur->link;
		hcl_freemem (hcl, cur);
	}
}

static const hcl_ooch_t* add_sr_name (hcl_t* hcl, const hcl_oocs_t* name)
{
	hcl_link_t* link;
	hcl_ooch_t* nptr;

	/* TODO: make search faster */
	link = hcl->c->sr_names;
	while (link)
	{
		nptr = (hcl_ooch_t*)(link + 1);
		if (hcl_comp_oochars_oocstr(name->ptr, name->len, nptr) == 0) return nptr;
		link = link->link;
	}

	link = (hcl_link_t*)hcl_callocmem(hcl, HCL_SIZEOF(*link) + HCL_SIZEOF(hcl_ooch_t) * (name->len + 1));
	if (HCL_UNLIKELY(!link))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to source name [%.*js] - %js", name->len, name->ptr, orgmsg);
		return HCL_NULL;
	}

	nptr = (hcl_ooch_t*)(link + 1);

	hcl_copy_oochars (nptr, name->ptr, name->len);
	nptr[name->len] = '\0';

	link->link = hcl->c->sr_names;
	hcl->c->sr_names = link;

	return nptr;
}

/* -------------------------------------------------------------------------- */

static HCL_INLINE int enter_list (hcl_t* hcl, const hcl_loc_t* loc, int flagv)
{
	hcl_rstl_t* rstl;
	rstl = (hcl_rstl_t*)hcl_callocmem(hcl, HCL_SIZEOF(*rstl));
	if (HCL_UNLIKELY(!rstl))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to allocate reader stack node - %js", orgmsg);
		return -1;
	}
	rstl->loc = *loc;
	rstl->flagv = flagv;
	rstl->prev = hcl->c->r.st; /* push */
	hcl->c->r.st = rstl;
	return 0;
}

static HCL_INLINE hcl_cnode_t* leave_list (hcl_t* hcl, hcl_loc_t* list_loc, int* flagv, int* oldflagv)
{
	hcl_rstl_t* rstl;
	hcl_cnode_t* head, * tail;
	hcl_oow_t count;
	hcl_loc_t loc;
	int fv;
	hcl_concode_t concode;

	/* the stack must not be empty - cannot leave a list without entering it */
	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st; /* get the stack top */

	head = rstl->head;
	tail = rstl->tail;
	count = rstl->count;
	fv = rstl->flagv;
	loc = rstl->loc;
	concode = (hcl_concode_t)LIST_FLAG_GET_CONCODE(fv);

	hcl->c->r.st = rstl->prev; /* pop off  */
	hcl_freemem (hcl, rstl); /* dispose of the stack node */

	if (fv & (COMMAED | COLONED | COLONEQED | BINOPED))
	{
		/* no item after , : := or various binary operators */
		if (concode == HCL_CONCODE_MLIST)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_CALLABLE, TOKEN_LOC(hcl), HCL_NULL, "missing message after receiver");
		}
		else if (concode == HCL_CONCODE_ALIST)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_RVALUE, TOKEN_LOC(hcl), HCL_NULL, "missing rvalue after :=");
		}
		else if (concode == HCL_CONCODE_BLIST)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_NOVALUE, TOKEN_LOC(hcl), HCL_NULL, "missing expression after binary selector");
		}
		else
		{
			hcl_synerrnum_t err;
			err = (fv & COMMAED)? HCL_SYNERR_COMMANOVALUE: HCL_SYNERR_COLONNOVALUE;
			hcl_setsynerr (hcl, err, TOKEN_LOC(hcl), HCL_NULL);
		}
		goto oops;
	}

	*list_loc = loc;
	*oldflagv = fv;
	if (!hcl->c->r.st)
	{
		/* the stack is empty after popping.
		 * it is back to the top level.
		 * the top level can never be quoted. */
		*flagv = 0;
	}
	else
	{
		/* restore the flag for the outer returning level */
		*flagv = hcl->c->r.st->flagv;
	}

	if (head)
	{
		HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(head));

		if (concode == HCL_CONCODE_ALIST) /* assignment list */
		{
			/* sanitize/tranform (var := val) to (set var val)
			 * - note ALIST doesn't contain the := symbol */
			hcl_cnode_t* lval;
		#if defined(TRANSFORM_ALIST)
			hcl_cnode_t* sym, * newhead;
			hcl_oocs_t fake_tok, * fake_tok_ptr = HCL_NULL;
		#endif

			lval = HCL_CNODE_CONS_CAR(head);
			if (lval && HCL_CNODE_IS_ELIST(lval))
			{
				/* invalid lvalue - for example, () := 20 */
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_LVALUE, HCL_CNODE_GET_LOC(lval), HCL_CNODE_GET_TOK(lval), "bad lvalue - blank expression");
				goto oops;
			}
			else if (lval && HCL_CNODE_IS_CONS_CONCODED(lval, HCL_CONCODE_TUPLE))
			{
				/*
				 * fun f(a :: b c) { b := (a + 10); c := (a + 20) }
				 * [x, y] := (f 9) ## this kind of expression - translate to set-r x y (f 9)
				 */
				hcl_cnode_t* tmp;
		#if defined(TRANSFORM_ALIST)
				hcl_cnode_t* rval;
		#endif

		#if defined(TRANSFORM_ALIST)
				fake_tok.ptr = vocas[VOCA_SYM_SET_R].str;
				fake_tok.len = vocas[VOCA_SYM_SET_R].len;
				fake_tok_ptr = &fake_tok;
		#endif

				for (tmp = lval; tmp && HCL_CNODE_IS_CONS(tmp); tmp = HCL_CNODE_CONS_CDR(tmp))
				{
					/* check in advance if the array members are all plain symbols */
					hcl_cnode_t* lcar;
					lcar = HCL_CNODE_CONS_CAR(tmp);
					if (!HCL_CNODE_IS_SYMBOL_PLAIN_IDENT(lcar) && !HCL_CNODE_IS_DSYMBOL_CLA(lcar))
					{
						hcl_setsynerrbfmt (hcl, HCL_SYNERR_LVALUE, HCL_CNODE_GET_LOC(lval), HCL_CNODE_GET_TOK(lval), "bad lvalue - invalid element in tuple");
						goto oops;
					}
				}

		#if defined(TRANSFORM_ALIST)
				/* move the array item up to the main list and join the original lval to the end of it
				 * For [x, y] := (f 9), x and y must be in the same level as set-r after translation.
				 * so make it 'x y (f 9)' first and place set-r in front of it later. */
				rval = HCL_CNODE_CONS_CDR(head);
				hcl_freesinglecnode (hcl, head);
				head = lval;
				for (tmp = lval; tmp && HCL_CNODE_IS_CONS(tmp); tmp = HCL_CNODE_CONS_CDR(tmp))
				{
					if (!HCL_CNODE_CONS_CDR(tmp))
					{
						HCL_CNODE_CONS_CDR(tmp) = rval;
						break;
					}
				}
		#endif
			}
			else
			{
				if (!HCL_CNODE_IS_SYMBOL_PLAIN_IDENT(lval) && !HCL_CNODE_IS_DSYMBOL_CLA(lval))
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_LVALUE, HCL_CNODE_GET_LOC(lval), HCL_CNODE_GET_TOK(lval), "bad lvalue - invalid element");
					goto oops;
				}
		#if defined(TRANSFORM_ALIST)
				fake_tok.ptr = vocas[VOCA_SYM_SET].str;
				fake_tok.len = vocas[VOCA_SYM_SET].len;
				fake_tok_ptr = &fake_tok;
		#endif
			}

			HCL_ASSERT (hcl, count >= 2); /* the missing rvalue check has been done above */
			if (count != 2)
			{
				hcl_cnode_t* rval;
				rval = HCL_CNODE_CONS_CDR(head);
				rval = HCL_CNODE_CONS_CDR(rval);
				rval = HCL_CNODE_CONS_CAR(rval);

				hcl_setsynerrbfmt (hcl, HCL_SYNERR_RVALUE, HCL_CNODE_GET_LOC(rval), HCL_CNODE_GET_TOK(rval), "too many rvalues after :=");
				goto oops;
			}

		#if defined(TRANSFORM_ALIST)
			sym = hcl_makecnodesymbol(hcl, 0, &loc, fake_tok_ptr);
			if (HCL_UNLIKELY(!sym))
			{
				const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
				hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to create symbol cnode for := - %js", orgmsg);
				goto oops;
			}

			/* create a new head joined with set or set-r */
			newhead = hcl_makecnodecons(hcl, 0, &loc, fake_tok_ptr, sym, head);
			if (HCL_UNLIKELY(!newhead))
			{
				const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
				hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to create cons cnode for := - %js", orgmsg);
				hcl_freecnode (hcl, sym);
				goto oops;
			}

			head = newhead;
			concode = HCL_CONCODE_XLIST; /* switch back to XLIST */
		#endif
		}
		else if (concode == HCL_CONCODE_BLIST)
		{
			/* x binop y -> binop x y - BLIST contains BINOP in it */
			hcl_cnode_t* x, * binop;

			/*x = HCL_CNODE_CONS_CAR(head);*/
			binop = HCL_CNODE_CONS_CDR(head);

			HCL_ASSERT (hcl, binop && HCL_CNODE_IS_CONS(binop));
			HCL_ASSERT (hcl, count >= 2); /* the code in can_binop_list() ensures this condition */

			if (count < 3)
			{
				/* for example, 1 + */
				x = HCL_CNODE_CONS_CAR(binop);
				/* with the transformation, the implementation supports two operands and a single binop in an expression. */
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_NOVALUE, HCL_CNODE_GET_LOC(x), HCL_NULL,
					"no operand after binary selector '%.*js'", HCL_CNODE_GET_TOKLEN(x), HCL_CNODE_GET_TOKPTR(x));
				goto oops;
			}
			else if (count > 3)
			{
				/* for example, 1 + 1 1 */
				x = HCL_CNODE_CONS_CAR(tail);
				/* with the transformation, the implementation supports two operands and a single binop in an expression. */
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_NOVALUE, HCL_CNODE_GET_LOC(x), HCL_NULL,
					"redundant operand '%.*js'", HCL_CNODE_GET_TOKLEN(x), HCL_CNODE_GET_TOKPTR(x));
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
			HCL_CNODE_CONS_CDR(head) = HCL_CNODE_CONS_CDR(binop);
			HCL_CNODE_CONS_CDR(binop) = head;
			head = binop;
			concode = HCL_CONCODE_XLIST;
		#endif
		}

		HCL_CNODE_CONS_CONCODE(head) = concode;
		if (fv & AUTO_FORGED) HCL_CNODE_GET_FLAGS(head) |= HCL_CNODE_AUTO_FORGED;
	}
	else
	{
		/* the list is empty */
		head = hcl_makecnodeelist(hcl, ((fv & AUTO_FORGED)? HCL_CNODE_AUTO_FORGED: 0), &loc, concode);
		if (HCL_UNLIKELY(!head))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to create empty list - %js", orgmsg);
		}
	}

	return head;

oops:
	if (head) hcl_freecnode (hcl, head);
	return HCL_NULL;
}

static HCL_INLINE int can_dot_list (hcl_t* hcl)
{
	hcl_rstl_t* rstl;

	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;

	/* mark the state that a dot has appeared in the list */
	if (rstl->count <= 0) return 0;
	if (LIST_FLAG_GET_CONCODE(rstl->flagv) != HCL_CONCODE_QLIST) return 0;

	rstl->flagv |= DOTTED;
	return 1;
}

static HCL_INLINE int can_comma_list (hcl_t* hcl)
{
	hcl_rstl_t* rstl;
	hcl_concode_t cc;

	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;

	if (rstl->count <= 0) return 0;

	if (rstl->count == 1) rstl->flagv |= JSON;
	else if (!(rstl->flagv & JSON)) return 0;

	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	cc = (hcl_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);
	if (cc == HCL_CONCODE_XLIST)
	{
		/* fun f(a :: b c) { b := (a + 10); c := (a + 20) }
		 * [x y] := (f 9)
		 * [x,y] := (f 9) */
		LIST_FLAG_SET_CONCODE(rstl->flagv, HCL_CONCODE_ALIST);
	}
	else if (cc == HCL_CONCODE_DIC)
	{
		if (rstl->count & 1) return 0;
	}
	else if (cc != HCL_CONCODE_ARRAY && cc != HCL_CONCODE_BYTEARRAY &&
	         cc != HCL_CONCODE_CHARARRAY && cc != HCL_CONCODE_TUPLE)
	{
		return 0;
	}

	rstl->flagv |= COMMAED;
	return 1;
}

static HCL_INLINE int can_colon_list (hcl_t* hcl)
{
	hcl_rstl_t* rstl;
	hcl_concode_t cc;

	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;

	if (rstl->count <= 0) return 0; /* not allowed at the list beginning  */

	/* mark the state that a colon has appeared in the list */
	if (rstl->count == 1) rstl->flagv |= JSON; /* mark that the first key is colon-delimited */
	else if (!(rstl->flagv & JSON))
	{
		/* handling of a colon sign in out-of-class instance method definition.
		 * e.g. fun String:length() { return (str.length self). }
		 * TODO: inject a symbol ':' to differentiate form '::' or ':*' methods.
		 *       these class methods and class instantiation methods are supposed to be
		 *       implemented elsewhere because ':' has dual use while '::' or ':*' are
		 *       independent tokens  */
		if (HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(rstl->head), HCL_CNODE_FUN))
		{
			hcl_cnode_t* tmp, * next;
			next = HCL_CNODE_CONS_CDR(rstl->head);
			HCL_ASSERT (hcl, next != HCL_NULL);
			tmp = HCL_CNODE_CONS_CAR(next); /* second item */
			if (rstl->count == 2)
			{
				/* fun class:name() *... */
				if (HCL_CNODE_IS_SYMBOL_PLAIN(tmp)) return 2;
			}
			else if (rstl->count == 3)
			{
				/* fun(#c) class:name() ... */
				if (HCL_CNODE_IS_CONS_CONCODED(tmp, HCL_CONCODE_XLIST) ||
				    HCL_CNODE_IS_ELIST_CONCODED(tmp, HCL_CONCODE_XLIST))
				{
					next = HCL_CNODE_CONS_CDR(next);
					HCL_ASSERT (hcl, next != HCL_NULL);
					tmp = HCL_CNODE_CONS_CAR(next); /* third item */
					if (HCL_CNODE_IS_SYMBOL_PLAIN(tmp)) return 2;
				}
			}
		}

		return 0; /* the first key is not colon-delimited. so not allowed to colon-delimit other keys  */
	}

	/* multiple single-colons  - e.g. #{ "abc": : 20 } */
	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	cc = (hcl_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);
	if (cc == HCL_CONCODE_XLIST)
	{
		hcl_cnode_t* tmp;

		/* method defintion with fun  - e.g. fun String:length()
		 * ugly that this reader must know about the meaning of fun */
		if (rstl->count > 1) return 0;

		/* ugly dual use of a colon sign. switch to MLIST if the first element
		 * is delimited by a colon. e.g. (obj:new 10 20 30)  */
		tmp = HCL_CNODE_CONS_CAR(rstl->head);
		if (!HCL_CNODE_IS_FOR_DATA(tmp)) return 0;

		LIST_FLAG_SET_CONCODE(rstl->flagv, HCL_CONCODE_MLIST);
		rstl->flagv &= ~JSON;
	}
	else if (cc != HCL_CONCODE_DIC) return 0; /* no allowed if not in a dictionary */

	if (!(rstl->count & 1)) return 0; /* not allwed after the value in a dictionary */

	/* mark that it's coloned. this is to be cleared when clear_comma_colon_binop_flag() is called */
	rstl->flagv |= COLONED;
	return 1;
}

static HCL_INLINE int can_coloneq_list (hcl_t* hcl)
{
	hcl_rstl_t* rstl;
	hcl_concode_t cc;

	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;

	if (rstl->count <= 0 || rstl->count >= 2) return 0; /* allowed after the first item only */

	/* repeated delimiters - e.g (a := := ...)   (a : := ... )  */
	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	cc = (hcl_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);

	/* assignment only in XLIST */
	if (cc != HCL_CONCODE_XLIST) return 0;

	LIST_FLAG_SET_CONCODE(rstl->flagv, HCL_CONCODE_ALIST);
	rstl->flagv |= COLONEQED;
	return 1;
}

static HCL_INLINE int can_binop_list (hcl_t* hcl)
{
	hcl_rstl_t* rstl;
	hcl_concode_t cc;

	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;
	cc = (hcl_concode_t)LIST_FLAG_GET_CONCODE(rstl->flagv);

	if (rstl->count <= 0 || cc == HCL_CONCODE_TUPLE || is_at_block_beginning(hcl))
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

	if (rstl->count >= 1 && cc == HCL_CONCODE_XLIST)
	{
		/* special case:
		 *   fun xxx::+() { }
		 *   fun + () {}
		 */

		/* TODO: this whole block is hacky. we may do proper parsing instead of checking the first element is 'fun' */
		if (HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(rstl->head), HCL_CNODE_FUN)) return 1;
	}

	/* repeated delimiters - e.g (a ++ ++ ...)   (a : := ... )  */
	if (rstl->flagv & (COMMAED | COLONED | COLONEQED | BINOPED)) return 0;

	if (cc == HCL_CONCODE_BLIST)
	{
		hcl_cnode_t* wrap;
		hcl_oocs_t fake_tok, * fake_tok_ptr = HCL_NULL;

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
		/*HCL_ASSERT (hcl, rstl->count == 2 || rstl->count == 3); this condition is wrong, for say, 1 + 2 3 + 4 */
		HCL_ASSERT (hcl, rstl->count >= 2);

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
		HCL_ASSERT (hcl, HCL_CNODE_IS_TYPED(rstl->head, HCL_CNODE_CONS));
		wrap = hcl_makecnodecons(hcl, 0, HCL_CNODE_GET_LOC(rstl->head), fake_tok_ptr, rstl->head, HCL_NULL);
		if (HCL_UNLIKELY(!wrap)) return  -1;
		HCL_CNODE_CONS_CONCODE(rstl->head) = HCL_CONCODE_BLIST;

		rstl->head = wrap;
		rstl->tail = wrap;
		rstl->count = 1; /* force adjust it to 1 */
	}
	else
	{
		if (rstl->count >= 2) return 0; /* allowed after the first item only */
		if (cc != HCL_CONCODE_XLIST) return 0;
	}

	LIST_FLAG_SET_CONCODE(rstl->flagv, HCL_CONCODE_BLIST); /* switch to BLIST as long as a binary operator is seen */
	rstl->flagv |= BINOPED;
/* TODO: must remember the actual binop operator token */
	return 2;
}

static HCL_INLINE void clear_comma_colon_binop_flag (hcl_t* hcl)
{
	hcl_rstl_t* rstl;
	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;
	rstl->flagv &= ~(COMMAED | COLONED | COLONEQED | BINOPED);
}

static int chain_to_list (hcl_t* hcl, hcl_cnode_t* obj, hcl_loc_t* loc)
{
	hcl_rstl_t* rstl;
	int flagv;
	/*int list_concode;*/

	HCL_ASSERT (hcl, hcl->c->r.st != HCL_NULL);
	rstl = hcl->c->r.st;
	flagv = rstl->flagv;
	/*list_concode = (hcl_concode_t)LIST_FLAG_GET_CONCODE(flagv);*/

	if (flagv & CLOSED)
	{
		/* the list has already been closed and cannot add more items
		 * for instance,  see this faulty expression #(1 2 . 3 4 ).
		 * you can have only 1 item  after the period. this condition
		 * can only be triggered by a wrong qlist where a period is
		 * allowed. so i can safely hard-code the error code to
		 * HCL_SYNERR_RPAREN */
		hcl_setsynerr (hcl, HCL_SYNERR_RPAREN, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
		return -1;
	}
	else if (flagv & DOTTED)
	{
		hcl_cnode_t* tail;
		/* the list must not be empty to have reached the dotted state */
		HCL_ASSERT (hcl, rstl->head != HCL_NULL);
		HCL_ASSERT (hcl, rstl->tail != HCL_NULL);
		HCL_ASSERT (hcl, rstl->count > 0);

		/* chain the object via 'cdr' of the tail cell */
		tail = rstl->tail;
		HCL_ASSERT (hcl, tail != HCL_NULL);
		HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(tail));

		if (HCL_CNODE_IS_CONS(obj) && HCL_CNODE_CONS_CONCODE(obj) != HCL_CONCODE_QLIST)
		{
			hcl_cnode_t* shell;
			/* if the last element is another non-data list
			 * for example, #( 1 2 . #[ 3 4 5  ])
			 * use a shell node to wrap the actual object list node head
			 * for the compiler.
			 */
			shell = hcl_makecnodeshell(hcl, 0, HCL_CNODE_GET_LOC(obj), obj);
			if (HCL_UNLIKELY(!shell)) return -1;
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
		hcl_cnode_t* cons, * tail;
		hcl_oocs_t fake_tok, * fake_tok_ptr = HCL_NULL;
		int concode;

		if ((flagv & JSON) && rstl->count > 0 && !(flagv & (COMMAED | COLONED)))
		{
			/* there is no separator between array/dictionary elements
			 * for instance, [1 2] { 10 20 } */
			hcl_setsynerr (hcl, HCL_SYNERR_NOSEP, TOKEN_LOC(hcl), HCL_NULL);
			return -1;
		}

		/* `loc` may be passed in if the added `obj` is a cons cell for another list */
		HCL_ASSERT (hcl, (loc && (HCL_CNODE_IS_CONS(obj) || HCL_CNODE_IS_ELIST(obj))) || (!loc && !HCL_CNODE_IS_CONS(obj)));
		concode = HCL_CNODE_IS_CONS(obj)? HCL_CNODE_CONS_CONCODE(obj):
		          HCL_CNODE_IS_ELIST(obj)? HCL_CNODE_ELIST_CONCODE(obj): -1;

		if (concode >= 0)
		{
			int vid = cons_info[concode].voca_id;
			fake_tok.ptr = vocas[vid].str;
			fake_tok.len = vocas[vid].len;
			fake_tok_ptr = &fake_tok;
		}

		cons = hcl_makecnodecons(hcl, 0, (loc? loc: HCL_CNODE_GET_LOC(obj)), fake_tok_ptr, obj, HCL_NULL);
		if (HCL_UNLIKELY(!cons)) return -1;

		if (rstl->count <= 0)
		{
			/* the list head is not set yet. it is the first
			 * element added to the list. let both head and tail
			 * point to the new cons cell */
			HCL_ASSERT (hcl, rstl->tail == HCL_NULL);
			HCL_ASSERT (hcl, rstl->head == HCL_NULL);

			rstl->head = cons;
			rstl->tail = cons;
		}
		else
		{
			/* the new cons cell is not the first element. append it to the list */
			tail = rstl->tail;
			HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(tail));
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
hcl_cnodetoobj (hcl_t* hcl, hcl_cnode_t* x)
{
 * drop location information and compose object ??
 * is it doable? can convert a dotted symbol to a proper value?
}
*/

/* ---------------------------------------------------------------------- */

static int on_fed_cnode (hcl_t* hcl, hcl_cnode_t* obj)
{
	/* the default handler for a cnode composed via feeding - just compile the object node. */
	return hcl_compile(hcl, obj, 0);
}

/* ---------------------------------------------------------------------- */

static void init_feed (hcl_t* hcl)
{
	HCL_MEMSET (&hcl->c->feed, 0, HCL_SIZEOF(hcl->c->feed));
	hcl->c->feed.lx.state = HCL_FLX_START;
	hcl->c->feed.lx.loc.line = 1;
	hcl->c->feed.lx.loc.colm = 1;
	hcl->c->feed.lx.loc.file = HCL_NULL;
	hcl->c->feed.on_cnode = on_fed_cnode;
}

/* ------------------------------------------------------------------------ */

static int feed_begin_include (hcl_t* hcl)
{
	hcl_io_cciarg_t* arg;
	const hcl_ooch_t* io_name;

	io_name = add_sr_name(hcl, TOKEN_NAME(hcl));
	if (HCL_UNLIKELY(!io_name))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to include %.*js for name registration failure - %js", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl), orgmsg);
		return -1;
	}

	arg = (hcl_io_cciarg_t*)hcl_callocmem(hcl, HCL_SIZEOF(*arg));
	if (HCL_UNLIKELY(!arg))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to include %.*js for memory allocation failure - %js", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl), orgmsg);
		goto oops;
	}

	arg->name = io_name;
	arg->line = 1;
	arg->colm = 1;
	/*arg->nl = '\0';*/
	/*arg->byte_oriented = 0;*/
	arg->includer = hcl->c->curinp;

	if (hcl->c->cci_rdr(hcl, HCL_IO_OPEN, arg) <= -1)
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_INCLUDE, TOKEN_LOC(hcl), TOKEN_NAME(hcl), "unable to include %js - %js", io_name, orgmsg);
		goto oops;
	}

	if (arg->includer == &hcl->c->cci_arg) /* top-level include */
	{
		/* TODO: remove hcl_readbasesrchar() and clean up this part.
		 * hcl_readbasesrchar(), if called in the middle of feeds,
		 * updates hcl->c->cci_arg's line and colm. so use a separate
		 * field to store the current feed location for now */
		hcl->c->feed.lx._oloc = hcl->c->feed.lx.loc;
	}
	else
	{
		arg->includer->name = hcl->c->feed.lx.loc.file;
		arg->includer->line = hcl->c->feed.lx.loc.line;
		arg->includer->colm = hcl->c->feed.lx.loc.colm;
	}
	hcl->c->feed.lx.loc.file = arg->name;
	hcl->c->feed.lx.loc.line = arg->line;
	hcl->c->feed.lx.loc.colm = arg->colm;

	/* switch to the includee's stream */
	hcl->c->curinp = arg;
	/* hcl->c->depth.incl++; */

	return 0;

oops:
	if (arg)
	{
		hcl_freemem (hcl, arg);
	}
	return -1;
}

static int feed_end_include (hcl_t* hcl)
{
	int x;
	hcl_io_cciarg_t* cur;

	if (hcl->c->curinp == &hcl->c->cci_arg) return 0; /* no include */

	/* if it is an included file, close it and
	 * retry to read a character from an outer file */

	x = hcl->c->cci_rdr(hcl, HCL_IO_CLOSE, hcl->c->curinp);

	/* if closing has failed, still destroy the sio structure
	 * first as normal and return the failure below. this way,
	 * the caller doesn't call HCL_IO_CLOSE on hcl->c->curinp again. */

	cur = hcl->c->curinp;
	hcl->c->curinp = hcl->c->curinp->includer;

	if (hcl->c->curinp == &hcl->c->cci_arg)
	{
		hcl->c->feed.lx.loc = hcl->c->feed.lx._oloc;
	}
	else
	{
		hcl->c->feed.lx.loc.file = hcl->c->curinp->name;
		hcl->c->feed.lx.loc.line = hcl->c->curinp->line;
		hcl->c->feed.lx.loc.colm = hcl->c->curinp->colm;
	}

	HCL_ASSERT (hcl, cur->name != HCL_NULL);
	hcl_freemem (hcl, cur);
	/* hcl->parse.depth.incl--; */

	if (x != 0)
	{
		/* the failure mentioned above is returned here */
		return -1;
	}

	hcl->c->lxc = hcl->c->curinp->lxc;
	return 1; /* ended the included file successfully */
}

static void feed_reset_reader_state (hcl_t* hcl)
{
	hcl_frd_t* frd = &hcl->c->feed.rd;

	if (frd->obj)
	{
		hcl_freecnode (hcl, frd->obj);
		frd->obj = HCL_NULL;
	}
	HCL_MEMSET (frd, 0, HCL_SIZEOF(*frd));
}

static void feed_clean_up_reader_stack (hcl_t* hcl)
{
	/* clean up the reader stack for a list */
	while (hcl->c->r.st)
	{
		hcl_rstl_t* rstl;
		rstl = hcl->c->r.st;
		hcl->c->r.st = rstl->prev;
		if (rstl->head) hcl_freecnode (hcl, rstl->head);
		hcl_freemem (hcl, rstl);
	}
}

static int is_at_block_beginning (hcl_t* hcl)
{
	hcl_rstl_t* rstl;
	rstl = hcl->c->r.st;
	return !rstl || (LIST_FLAG_GET_CONCODE(rstl->flagv) == HCL_CONCODE_BLOCK && (hcl->c->feed.rd.flagv & AT_BEGINNING));
}

static int auto_forge_xlist_if_at_block_beginning (hcl_t* hcl, hcl_frd_t* frd)
{
	if (is_at_block_beginning(hcl))
	{
		int forged_flagv;

		/* both MLIST and ALIST begin as XLIST and get converted to MLIST
		 * or ALIST after more tokens are processed. so handling of MLIST
		 * or ALIST is needed at this phase */
		forged_flagv = AUTO_FORGED;
		LIST_FLAG_SET_CONCODE (forged_flagv, HCL_CONCODE_XLIST);

		/* this portion is similar to the code below the start_list label */
		if (frd->level >= HCL_TYPE_MAX(int)) /* the nesting level too deep */
		{
			hcl_setsynerr (hcl, HCL_SYNERR_NESTING, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			return -1;
		}

		/* since the actual list opener doesn't exist, the location of the
		 * first element wil be the location of the list */
		if (enter_list(hcl, TOKEN_LOC(hcl), forged_flagv) <= -1) return -1;
		frd->level++; /* level after the forged list has been added */
		/* a new list has been created automatically. unlike normal list creation
		 * by an explicit symbol such as a left parenthesis, a left brace, etc,
		 * the first element opens up this new list. so the AT_BEGINNING bit is
		 * turned off here */
		frd->flagv &= ~AT_BEGINNING;
	}

	return 0;
}

static hcl_cnode_type_t kw_to_cnode_type (int tok_type)
{
	static hcl_cnode_type_t mapping[] = {
		HCL_CNODE_NIL,
		HCL_CNODE_TRUE,
		HCL_CNODE_FALSE,
		HCL_CNODE_SELF,
		HCL_CNODE_SUPER,

		HCL_CNODE_CLASS,
		HCL_CNODE_FUN,
		HCL_CNODE_DO,
		HCL_CNODE_IF,
		HCL_CNODE_ELIF,
		HCL_CNODE_ELSE,
		HCL_CNODE_THROW,
		HCL_CNODE_TRY,
		HCL_CNODE_CATCH,
		HCL_CNODE_BREAK,
		HCL_CNODE_CONTINUE,
		HCL_CNODE_UNTIL,
		HCL_CNODE_WHILE,
		HCL_CNODE_RETURN,
		HCL_CNODE_REVERT,
		HCL_CNODE_AND,
		HCL_CNODE_OR,
		HCL_CNODE_PLUS,
		HCL_CNODE_SET,
		HCL_CNODE_SET_R
	};

	return mapping[tok_type - HCL_TOK_NIL];
}

static int feed_process_token (hcl_t* hcl)
{
	hcl_frd_t* frd = &hcl->c->feed.rd;
	hcl_loc_t* list_loc = HCL_NULL;
	int rbrace_again = 0;
	int oops_ret = -1;
	/* TODO: frd->obj and frd->list_loc can become local variables in this function.. */

	/* this function composes an s-expression non-recursively
	 * by manipulating its own stack. */

/*hcl_logbfmt (hcl, HCL_LOG_STDERR, "TOKEN [%d] EOL[%d]=> [%.*js] type=%d LOC=%d.%d\n", TOKEN_TYPE(hcl), HCL_TOK_EOL, TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl), TOKEN_TYPE(hcl), TOKEN_LOC(hcl)->line, TOKEN_LOC(hcl)->colm);*/
	if (frd->expect_include_file)
	{
		/* the #include directive is an exception to the general expression rule.
		 * use this exceptional code block to divert the major token processing */

		if (TOKEN_TYPE(hcl) == HCL_TOK_EOL)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_STRING, TOKEN_LOC(hcl), HCL_NULL,
				"%.*js target not specified",
				vocas[VOCA_INCLUDE].len, vocas[VOCA_INCLUDE].str);
			goto oops;
		}
		else if (TOKEN_TYPE(hcl) != HCL_TOK_STRLIT)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_STRING, TOKEN_LOC(hcl), HCL_NULL,
				"%.*js target expected in place of '%.*js'",
				vocas[VOCA_INCLUDE].len, vocas[VOCA_INCLUDE].str,
				TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
			goto oops;
		}

		frd->expect_include_file = 0;

		/* indicate that the file inclusion should be performed soon.
		 * don't perform actual inclusion here so that the return value of
		 * feed_char() advances the input pointers properly. */
		frd->do_include_file = 1;

		goto ok;
	}

	if (frd->expect_vlist_item && TOKEN_TYPE(hcl) != HCL_TOK_IDENT && TOKEN_TYPE(hcl) != HCL_TOK_VBAR)
	{
		if (TOKEN_TYPE(hcl) == HCL_TOK_EOL) goto ok; /* ignore EOL inside vlist */

		/* vlist also has special requirement that it can only contain variable names. */
		hcl_setsynerr (hcl, HCL_SYNERR_VARNAME, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
		goto oops;
	}

	switch (TOKEN_TYPE(hcl))
	{
		default:
			hcl_setsynerr (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto oops;

		case HCL_TOK_EOF:
			if (hcl_feedpending(hcl))
			{
				hcl_setsynerr (hcl, HCL_SYNERR_EOF, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			}
			else
			{
				/* ugly hacking to return success intead while performing clean-up */
				oops_ret = 0;
			}
			goto oops;

		case HCL_TOK_INCLUDE:
			/* TODO: should i limit where #include can be specified?
			 *       disallow it inside a list literal or an array literal? */
			frd->expect_include_file = 1;
			goto ok;

		case HCL_TOK_PRAGMA:
			/* TODO: implement this */
			hcl_setsynerr (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto oops;

		case HCL_TOK_VBAR:
			if (frd->expect_vlist_item)
			{
				/* closer */
				int oldflagv;
				frd->expect_vlist_item = 0;
				frd->obj = leave_list(hcl, &frd->list_loc, &frd->flagv, &oldflagv);
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
				if (hcl->c->r.st && (hcl->c->r.st->flagv & DATA_LIST))
				{
					/* if the outer list is a data list */
					hcl_setsynerr (hcl, HCL_SYNERR_VBARBANNED, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
					goto oops;
				}

				/* neither a data list nor an executable list. handle this specially using
				 * a dedicated frd->expect_vlist_item variable */
				frd->flagv = 0;
				LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_VLIST);
				frd->expect_vlist_item = 1;
				goto start_list;
			}

		case HCL_TOK_LBRACK: /* [ */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_TUPLE);
			goto start_list;

		case HCL_TOK_APAREN: /* #[ */
			/* #[] is a data list. so let's treat it like other literal
			 * expressions(e.g. 1, "abc"). when it's placed at the block beginning,
			 * create the outer XLIST. */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_ARRAY);
			goto start_list;

		case HCL_TOK_BAPAREN: /* #b[ */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_BYTEARRAY);
			goto start_list;

		case HCL_TOK_CAPAREN: /* #c[ */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_CHARARRAY);
			goto start_list;

		case HCL_TOK_LBRACE: /* { */
			/* this is a block opener itself. auto xlist forge at the block beginning only */
			frd->flagv = 0;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_BLOCK);
			goto start_list;

		case HCL_TOK_DLPAREN: /* #{ */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_DIC);
			goto start_list;

		case HCL_TOK_QLPAREN: /* #( */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			frd->flagv = DATA_LIST;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_QLIST);
			goto start_list;

		#if defined(HCL_TOK_LPARCOLON)
		case HCL_TOK_LPARCOLON: /* (: */
			frd->flagv = 0;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_MLIST);
			goto start_list;
		#endif

		case HCL_TOK_LPAREN: /* ( */
#if defined(HCL_LANG_AUTO_FORGE_XLIST_ALWAYS)
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
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
#endif
			frd->flagv = 0;
			LIST_FLAG_SET_CONCODE (frd->flagv, HCL_CONCODE_XLIST);
		start_list:
			if (frd->level >= HCL_TYPE_MAX(int))
			{
				/* the nesting level has become too deep */
				hcl_setsynerr (hcl, HCL_SYNERR_NESTING, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
				goto oops;
			}

			/* push some data to simulate recursion into
			 * a list literal or an array literal */
			if (enter_list(hcl, TOKEN_LOC(hcl), frd->flagv) <= -1) goto oops;
			frd->level++;
			frd->flagv |= AT_BEGINNING; /* the reader is now at the beginning of a list */

			/* read the next token */
			goto ok;

		case HCL_TOK_DOT:
			if (frd->level <= 0 || !can_dot_list(hcl))
			{
				/* cannot have a period:
				 *   1. at the top frd->level - not inside ()
				 *   2. at the beginning of a list
				 *   3. inside an array, byte-array, dictionary, xlist */
				hcl_setsynerr (hcl, HCL_SYNERR_DOTBANNED, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}
			goto ok;

		case HCL_TOK_COLON:
		{
			int n;
			if (frd->level <= 0 || !(n = can_colon_list(hcl)))
			{
				hcl_setsynerr (hcl, HCL_SYNERR_COLONBANNED, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}
			if (n == 2)
			{
				/* this is colon between the class name and the function name for
				 * out-of-class method defintion */
				frd->obj = hcl_makecnodecolon(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
				goto auto_xlist;
			}

			goto ok;
		}

		case HCL_TOK_COLONEQ:
			if (frd->level <= 0 || !can_coloneq_list(hcl))
			{
				hcl_setsynerr (hcl, HCL_SYNERR_COLONEQBANNED, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}
			goto ok;

		case HCL_TOK_BINOP:
		{
			int can = 0;
			if (frd->level <= 0)
			{
				HCL_ASSERT (hcl, hcl->c->r.st == HCL_NULL);
				/*if (hcl->c->r.st) goto banned_binop;*/
				/* very first even before entering a list including an auto-forged list */
				can = 1;
			}
			else if (!(can = can_binop_list(hcl)))
			{
			/*banned_binop:*/
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, TOKEN_LOC(hcl), HCL_NULL,
					"prohibited binary selector '%.*js'", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
				goto oops;
			}
			else if (can <= -1) goto oops;
			if (can == 1) goto ident; /* if binop is the first in the list */

			HCL_ASSERT (hcl, can == 2);

		#if 0
/* TODO: ... */
			frd->obj = hcl_makecnodebinop(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto ok;
		#else
			goto ident;
		#endif
		}

		case HCL_TOK_COMMA:
			if (frd->level <= 0 || !can_comma_list(hcl))
			{
				hcl_setsynerr (hcl, HCL_SYNERR_COMMABANNED, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}
			goto ok;

		case HCL_TOK_EOL: /* EOL returned only under a certain condition */
		case HCL_TOK_SEMICOLON:
		{
			int oldflagv;
			int concode;
			hcl_rstl_t* rstl;

		semicolon:
			/* the parent list(rstl) must be inspected instead of the current
			 * feed/read status pointed to by frd. */
			rstl = hcl->c->r.st;
			if (!rstl) goto ok; /* redundant eol/semicolon */

			concode = LIST_FLAG_GET_CONCODE(rstl->flagv);
			if (!(rstl->flagv & AUTO_FORGED))
			{
				if (TOKEN_TYPE(hcl) == HCL_TOK_EOL) goto ok;
				if (concode == HCL_CONCODE_BLOCK) goto ok;

				hcl_setsynerr (hcl, HCL_SYNERR_SEMICOLON, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}

			/* if auto-forged */
#if 0
/* TODO: remove this part if the assertion is confirmed true in the #else part... */
			if (concode != HCL_CONCODE_XLIST && concode != HCL_CONCODE_MLIST && concode != HCL_CONCODE_ALIST && concode != HCL_CONCODE_BLIST)
			{
				hcl_setsynerr (hcl, HCL_SYNERR_UNBALPBB, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}
#else
			HCL_ASSERT (hcl, concode == HCL_CONCODE_XLIST || concode == HCL_CONCODE_MLIST || concode == HCL_CONCODE_ALIST || concode == HCL_CONCODE_BLIST);
#endif

			frd->obj = leave_list(hcl, &frd->list_loc, &frd->flagv, &oldflagv);
			frd->level--;
			frd->flagv |= AT_BEGINNING; /* the current one is over. move on the beginning for the next expression */
			list_loc = &frd->list_loc;
			break;
		}

		case HCL_TOK_RPAREN: /* xlist (), qlist #() */
		case HCL_TOK_RBRACK: /* bytearray #b[], array #[] */
		case HCL_TOK_RBRACE: /* dictionary #{}, block {} */
		{
			int oldflagv;
			int concode;
			hcl_rstl_t* rstl;

			if (frd->level <= 0)
			{
				hcl_setsynerr (hcl, HCL_SYNERR_UNBALPBB, TOKEN_LOC(hcl), HCL_NULL);
				goto oops;
			}

			if (TOKEN_TYPE(hcl) == HCL_TOK_RBRACE)
			{
				rstl = hcl->c->r.st; /* check the parent, not the current */
				if (rstl && (rstl->flagv & AUTO_FORGED))
				{
				#if 0
					/* the auto-forged list has not been terminated. it must be terminated closed first */
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_SEMICOLON, TOKEN_LOC(hcl), TOKEN_NAME(hcl), "semicolon expected");
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
			if (concode == HCL_CONCODE_XLIST && (frd->flagv & AUTO_FORGED))
			{
				/* the auto-created xlist can't be terminated with the regular closing symbol
				 * it must end with the semicolon */
				hcl_setsynerr (hcl, HCL_SYNERR_UNBALPBB, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
				goto oops;
			}

			if (cons_info[concode].closer != TOKEN_TYPE(hcl))
			{
				hcl_setsynerr (hcl, cons_info[concode].synerr, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
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
				hcl_setsynerr (hcl, HCL_SYNERR_LPAREN, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
				goto oops;
			}
#endif
			frd->obj = leave_list(hcl, &frd->list_loc, &frd->flagv, &oldflagv);
			if (HCL_LIKELY(frd->obj))
			{
				frd->level--;
				frd->flagv |= AT_BEGINNING;
				list_loc = &frd->list_loc;
			}
			break;
		}

		case HCL_TOK_NIL:
		case HCL_TOK_TRUE:
		case HCL_TOK_FALSE:
		case HCL_TOK_SELF:
		case HCL_TOK_SUPER:

		case HCL_TOK_CLASS:
		case HCL_TOK_FUN:
		case HCL_TOK_DO:
		case HCL_TOK_IF:
		case HCL_TOK_ELIF:
		case HCL_TOK_ELSE:
		case HCL_TOK_THROW:
		case HCL_TOK_TRY:
		case HCL_TOK_CATCH:
		case HCL_TOK_BREAK:
		case HCL_TOK_CONTINUE:
		case HCL_TOK_UNTIL:
		case HCL_TOK_WHILE:
		case HCL_TOK_RETURN:
		case HCL_TOK_REVERT:
		case HCL_TOK_AND:
		case HCL_TOK_OR:
		case HCL_TOK_PLUS:
		case HCL_TOK_SET:
		case HCL_TOK_SET_R:
			frd->obj = hcl_makecnode(hcl, kw_to_cnode_type(TOKEN_TYPE(hcl)), 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_ELLIPSIS:
			frd->obj = hcl_makecnodeellipsis(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_TRPCOLONS:
			frd->obj = hcl_makecnodetrpcolons(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_DBLCOLONS:
			frd->obj = hcl_makecnodedblcolons(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_COLONGT:
			frd->obj = hcl_makecnodecolongt(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_COLONLT:
			frd->obj = hcl_makecnodecolonlt(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_SMPTRLIT:
		{
			hcl_oow_t i;
			hcl_oow_t v = 0;

			/* 0pNNNN */
			HCL_ASSERT (hcl, TOKEN_NAME_LEN(hcl) >= 3);
			for (i = 2; i < TOKEN_NAME_LEN(hcl); i++)
			{
				HCL_ASSERT (hcl, is_xdigit_char(TOKEN_NAME_CHAR(hcl, i)));
				v = v * 16 + HCL_CHAR_TO_NUM(TOKEN_NAME_CHAR(hcl, i), 16);
			}

			if (!HCL_IN_SMPTR_RANGE(v))
			{
				hcl_setsynerr (hcl, HCL_SYNERR_SMPTRLIT, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
				goto oops;
			}

			frd->obj = hcl_makecnodesmptrlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl), v);
			goto auto_xlist;
		}

		case HCL_TOK_ERRLIT:
		{
			hcl_oow_t i;
			hcl_ooi_t v = 0;

			HCL_ASSERT (hcl, TOKEN_NAME_LEN(hcl) >= 3);
			for (i = 2; i < TOKEN_NAME_LEN(hcl); i++)
			{
				HCL_ASSERT (hcl, is_digit_char(TOKEN_NAME_CHAR(hcl, i)));
				v = v * 10 + HCL_CHAR_TO_NUM(TOKEN_NAME_CHAR(hcl, i), 10);

				if (v > HCL_ERROR_MAX)
				{
					hcl_setsynerr (hcl, HCL_SYNERR_ERRLIT, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
					goto oops;
				}
			}

			frd->obj = hcl_makecnodeerrlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl), v);
			goto auto_xlist;
		}

		case HCL_TOK_CHARLIT:
			frd->obj = hcl_makecnodecharlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl), TOKEN_NAME_CHAR(hcl, 0));
			goto auto_xlist;

		case HCL_TOK_BCHRLIT:
			frd->obj = hcl_makecnodebchrlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl), (hcl_oob_t)TOKEN_NAME_CHAR(hcl, 0));
			goto auto_xlist;

		case HCL_TOK_STRLIT:
			frd->obj = hcl_makecnodestrlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_BSTRLIT:
			frd->obj = hcl_makecnodebstrlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_SYMLIT:
			frd->obj = hcl_makecnodesymlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_NUMLIT:
			frd->obj = hcl_makecnodenumlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_RADNUMLIT:
			frd->obj = hcl_makecnoderadnumlit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_FPDECLIT:
			frd->obj = hcl_makecnodefpdeclit(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		/*
		case HCL_TOK_REAL:
			frd->obj = hcl_makecnoderealnum(hcl, HCL_TOK_RVAL(hcl));
			break;
		*/

		case HCL_TOK_IDENT:
		ident:
			frd->obj = hcl_makecnodesymbol(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl));
			goto auto_xlist;

		case HCL_TOK_IDENT_DOTTED:
			frd->obj = hcl_makecnodedsymbol(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl), 0);
			goto auto_xlist;

		case HCL_TOK_IDENT_DOTTED_CLA:
			frd->obj = hcl_makecnodedsymbol(hcl, 0, TOKEN_LOC(hcl), TOKEN_NAME(hcl), 1);
			goto auto_xlist;

		auto_xlist:
			if (!frd->obj) goto oops; /* TODO: ugly... resturcture this check and the check 5 lines down  */
			if (auto_forge_xlist_if_at_block_beginning(hcl, frd) <= -1) goto oops;
			break;
	}

	if (!frd->obj) goto oops; /* TODO: this doesn't have to be checked if jump has been made to auto_xlist... so restructure the flow */

#if 0
	/* check if the element is read for a quoted list */
	while (flagv & QUOTED)
	{
		int oldflagv;

		HCL_ASSERT (hcl, frd->level > 0);

		/* if so, append the element read into the quote list */
		if (chain_to_list(hcl, obj) <= -1) goto oops;

		/* exit out of the quoted list. the quoted list can have one element only. */
		obj = leave_list(hcl, &flagv, &oldflagv);

		/* one frd->level up toward the top */
		frd->level--;
	}
#endif

	/* check if we are at the top frd->level */
	if (frd->level <= 0)
	{
		int n;

		/* upon exit, we must be at the top level */
		HCL_ASSERT (hcl, frd->flagv & AT_BEGINNING);

		HCL_ASSERT (hcl, hcl->c->r.st == HCL_NULL);
		HCL_ASSERT (hcl, frd->obj != HCL_NULL);

		n = hcl->c->feed.on_cnode(hcl, frd->obj);
		hcl_freecnode (hcl, frd->obj); /* not needed any more */
		frd->obj = HCL_NULL;
		if (n <= -1) goto oops;
	}
	else
	{
		/* if not, append the element read into the current list.
		 * if we are not at the top frd->level, we must be in a list */
		if (chain_to_list(hcl, frd->obj, list_loc) <= -1) goto oops;

		/* because it has been chained to the list, it belongs to the current stack top.
		 * mark that obj is not stand-alone by nullifying it. without this, if a jump
		 * is made to oops, the momory block pointed to by obj may get freed twice. */
		frd->obj = HCL_NULL;

		clear_comma_colon_binop_flag (hcl);
	}

ok:
	if (rbrace_again)
	{
		rbrace_again = 0;
		list_loc = HCL_NULL;
		goto rbrace_ok;
	}

	return 0;

oops:
	feed_reset_reader_state (hcl);

	/* clean up the reader stack for a list */
	feed_clean_up_reader_stack (hcl);
	feed_continue (hcl, HCL_FLX_START);
	return oops_ret;
}

/* ------------------------------------------------------------------------ */

struct delim_token_t
{
	const char*      t_value;
	hcl_oow_t        t_len;
	hcl_tok_type_t t_type;
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

	{ "(",        1, HCL_TOK_LPAREN },
#if defined(HCL_TOK_LPARCOLON)
	{ "(:",       2, HCL_TOK_LPARCOLON },
#endif
	{ ")",        1, HCL_TOK_RPAREN },

	{ "[",        1, HCL_TOK_LBRACK },
	{ "]",        1, HCL_TOK_RBRACK },

	{ "{",        1, HCL_TOK_LBRACE },
	{ "}",        1, HCL_TOK_RBRACE },

	{ "|",        1, HCL_TOK_VBAR },
	{ ",",        1, HCL_TOK_COMMA },

	{ ".",        1, HCL_TOK_DOT },
	{ "..",       2, HCL_TOK_DBLDOTS },
	{ "...",      3, HCL_TOK_ELLIPSIS }, /* for variable arguments */

	{ ":",        1, HCL_TOK_COLON }, /* key-value separator in dictionary or for method call or definition */
	{ ":=",       2, HCL_TOK_COLONEQ }, /* assignment */
	{ ":>",       2, HCL_TOK_COLONGT },
	{ ":<",       2, HCL_TOK_COLONLT },
	{ "::",       2, HCL_TOK_DBLCOLONS }, /* superclass, class variables, class methods */
	{ ":::",      3, HCL_TOK_TRPCOLONS  },

	{ ";",        1, HCL_TOK_SEMICOLON }
};

static int find_delim_token_char (hcl_t* hcl, const hcl_ooci_t c, int row_start, int row_end, int col, hcl_flx_dt_t* dt)
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

static HCL_INLINE int feed_wrap_up (hcl_t* hcl, hcl_tok_type_t type)
{
	int n;
	SET_TOKEN_TYPE (hcl, type);

	n = feed_process_token(hcl);

	hcl->c->feed.lx.state = HCL_FLX_START;
	return n;
}

static int feed_wrap_up_with_char (hcl_t* hcl, hcl_ooci_t c, hcl_tok_type_t type)
{
	ADD_TOKEN_CHAR (hcl, c);
	return feed_wrap_up(hcl, type);
}

static int feed_wrap_up_with_str (hcl_t* hcl, const hcl_ooch_t* str, hcl_oow_t len, hcl_tok_type_t type)
{
	ADD_TOKEN_STR (hcl, str, len);
	return feed_wrap_up(hcl, type);
}

static void feed_continue (hcl_t* hcl, hcl_flx_state_t state)
{
	hcl->c->feed.lx.state = state;
}

static int feed_continue_with_char (hcl_t* hcl, hcl_ooci_t c, hcl_flx_state_t state)
{
	ADD_TOKEN_CHAR (hcl, c);
	hcl->c->feed.lx.state = state;
	return 0;
}

#define FEED_WRAP_UP(hcl, type) do { if (feed_wrap_up(hcl, type) <= -1) return -1; } while(0)
#define FEED_WRAP_UP_WITH_CHAR(hcl, c, type) do { if (feed_wrap_up_with_char(hcl, c, type) <= -1) return -1; } while(0)
#define FEED_WRAP_UP_WITH_CHARS(hcl, str, len, type) do { if (feed_wrap_up_with_str(hcl, str, len, type) <= -1) return -1; } while(0)
#define FEED_CONTINUE(hcl, state) (feed_continue(hcl, state))
#define FEED_CONTINUE_WITH_CHAR(hcl, c, state) do { if (feed_continue_with_char(hcl, c, state) <= -1) return -1; } while(0)

/* ------------------------------------------------------------------------ */

/* short-cuts to basic lexer data */
#define FLX_STATE(hcl) ((hcl)->c->feed.lx.state)
#define FLX_LOC(hcl) (&((hcl)->c->feed.lx.loc))

/* short-cuts to lexer state data */
#define FLX_DT(hcl) (&((hcl)->c->feed.lx.u.dt))
#define FLX_DI(hcl) (&((hcl)->c->feed.lx.u.di))
#define FLX_HC(hcl) (&((hcl)->c->feed.lx.u.hc))
#define FLX_HBC(hcl) (&((hcl)->c->feed.lx.u.hbc))
#define FLX_HN(hcl) (&((hcl)->c->feed.lx.u.hn))
#define FLX_HI(hcl) (&((hcl)->c->feed.lx.u.hi))
#define FLX_PI(hcl) (&((hcl)->c->feed.lx.u.pi))
#define FLX_BINOP(hcl) (&((hcl)->c->feed.lx.u.binop))
#define FLX_PN(hcl) (&((hcl)->c->feed.lx.u.pn))
#define FLX_QT(hcl) (&((hcl)->c->feed.lx.u.qt))
#define FLX_ST(hcl) (&((hcl)->c->feed.lx.u.st))
#define FLX_BCP(hcl) (&((hcl)->c->feed.lx.u.bcp))

static HCL_INLINE void init_flx_di (hcl_flx_di_t* di)
{
	HCL_MEMSET (di, 0, HCL_SIZEOF(*di));
}

static HCL_INLINE void init_flx_hc (hcl_flx_hc_t* hc)
{
	HCL_MEMSET (hc, 0, HCL_SIZEOF(*hc));
}

static HCL_INLINE void init_flx_hi (hcl_flx_hi_t* hi)
{
	HCL_MEMSET (hi, 0, HCL_SIZEOF(*hi));
}

static HCL_INLINE void init_flx_hbc (hcl_flx_hbc_t* hbc, hcl_ooch_t start_c)
{
	HCL_MEMSET (hbc, 0, HCL_SIZEOF(*hbc));
	hbc->start_c = start_c;
}

static HCL_INLINE void init_flx_qt (hcl_flx_qt_t* qt, hcl_tok_type_t tok_type, hcl_synerrnum_t synerr_code, hcl_ooch_t end_char, hcl_ooch_t esc_char, hcl_oow_t min_len, hcl_oow_t max_len, int is_byte)
{
	HCL_MEMSET (qt, 0, HCL_SIZEOF(*qt));
	qt->tok_type = tok_type;
	qt->synerr_code = synerr_code;
	qt->end_char = end_char;
	qt->esc_char = esc_char;
	qt->min_len = min_len;
	qt->max_len = max_len;
	qt->is_byte = is_byte;
}

static HCL_INLINE void init_flx_pi (hcl_flx_pi_t* pi)
{
	HCL_MEMSET (pi, 0, HCL_SIZEOF(*pi));
}

static HCL_INLINE void init_flx_binop (hcl_flx_binop_t* binop)
{
	HCL_MEMSET (binop, 0, HCL_SIZEOF(*binop));
}

static HCL_INLINE void init_flx_pn (hcl_flx_pn_t* pn, hcl_ooch_t start_digit)
{
	HCL_MEMSET (pn, 0, HCL_SIZEOF(*pn));
	pn->start_digit = start_digit;
	pn->radix = 10;
	pn->radix_cand = 0;
	pn->radix_cand_overflown = 0;
	pn->tok_type = HCL_TOK_NUMLIT;
}

static HCL_INLINE void init_flx_st (hcl_flx_st_t* st, hcl_ooch_t sign_c)
{
	HCL_MEMSET (st, 0, HCL_SIZEOF(*st));
	st->sign_c = sign_c;
}

static HCL_INLINE void init_flx_bcp (hcl_flx_bcp_t* bcp, hcl_ooch_t start_c)
{
	HCL_MEMSET (bcp, 0, HCL_SIZEOF(*bcp));
	bcp->start_c = start_c;
}

static void reset_flx_token (hcl_t* hcl)
{
	/* clear the token name, reset its location */
	SET_TOKEN_TYPE (hcl, HCL_TOK_EOF); /* is it correct? */
	CLEAR_TOKEN_NAME (hcl);
	SET_TOKEN_LOC (hcl, &hcl->c->feed.lx.loc);
}

static int flx_start (hcl_t* hcl, hcl_ooci_t c)
{
	HCL_ASSERT (hcl, FLX_STATE(hcl) == HCL_FLX_START);

	if (is_spacechar(c))
	{
		if ((hcl->option.trait & HCL_TRAIT_LANG_ENABLE_EOL) && is_linebreak(c))
		{
			reset_flx_token (hcl);
			FEED_WRAP_UP_WITH_CHAR (hcl, c, HCL_TOK_EOL);
		}

		goto consumed; /* skip spaces */
	}

	reset_flx_token (hcl);

	if (find_delim_token_char(hcl, c, 0, HCL_COUNTOF(delim_token_tab) - 1, 0, FLX_DT(hcl)))
	{
		/* the character is one of the first character of a delimiter token such as (, [, :, etc */
		if (FLX_DT(hcl)->row_start == FLX_DT(hcl)->row_end &&
		    FLX_DT(hcl)->col_next == delim_token_tab[FLX_DT(hcl)->row_start].t_len)
		{
			/* single character delimiter token */
			FEED_WRAP_UP_WITH_CHAR (hcl, c, delim_token_tab[FLX_DT(hcl)->row_start].t_type);
		}
		else
		{
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_DELIM_TOKEN); /* consume c and move to HCL_FLX_DELIM_TOKEN state */
		}
		goto consumed;
	}

	switch (c)
	{
		case HCL_OOCI_EOF:
			/* only EOF of the top-level stream is supposed to be fed in.
			 * the internal logic discard EOFs of included streams */
			FEED_WRAP_UP_WITH_CHARS (hcl, vocas[VOCA_EOF].str, vocas[VOCA_EOF].len, HCL_TOK_EOF);
			goto consumed;

		case '\\':
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_BACKSLASHED);
			goto consumed;

		/* this part is never hit because the semicolon sign is part of delim_tok_tab{}
		   TODO: remove this part once the language spec is finalized to not require this
		case ';':
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_COMMENT);
			goto consumed;
		*/

		case '$':
			init_flx_di (FLX_DI(hcl));
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_DOLLARED_IDENT);
			goto consumed;

		case '#':
			/* no state date to initialize. just change the state */
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_HMARKED_TOKEN);
			goto consumed;

		case '\"':
			init_flx_qt (FLX_QT(hcl), HCL_TOK_STRLIT, HCL_SYNERR_STRLIT, c, '\\', 0, HCL_TYPE_MAX(hcl_oow_t), 0);
			FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;

		case '\'':
			init_flx_qt (FLX_QT(hcl), HCL_TOK_CHARLIT, HCL_SYNERR_CHARLIT, c, '\\', 1, 1, 0);
			FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;

#if defined(HCL_OOCH_IS_UCH) && defined(HCL_LANG_ENABLE_WIDE_DELIM)
		case L'\u201C': /* “ ” */
			init_flx_qt (FLX_QT(hcl), HCL_TOK_STRLIT, HCL_SYNERR_STRLIT, L'\u201D', '\\', 0, HCL_TYPE_MAX(hcl_oow_t), 0);
			FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;
		case L'\u2018': /* ‘ ’ */
			init_flx_qt (FLX_QT(hcl), HCL_TOK_CHARLIT, HCL_SYNERR_CHARLIT, L'\u2019', '\\', 1, 1, 0);
			FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* discard the quote itself. move on the the QUOTED_TOKEN state */
			goto consumed;
#endif

		case '+':
		case '-':
			init_flx_st (FLX_ST(hcl), c);
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_SIGNED_TOKEN);
			goto consumed;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			init_flx_pn (FLX_PN(hcl), c);
			FEED_CONTINUE (hcl, HCL_FLX_PLAIN_NUMBER);
			goto not_consumed;

		case 'B': /* for charcter/string prefixed with B,b,C,c */
		case 'b':
		case 'C':
		case 'c':
			init_flx_bcp(FLX_BCP(hcl), c);
			FEED_CONTINUE_WITH_CHAR(hcl, c, HCL_FLX_BC_PREFIX);
			goto consumed;

		default:
			if (is_binop_char(c))
			{
				init_flx_binop (FLX_BINOP(hcl));
				FEED_CONTINUE (hcl, HCL_FLX_BINOP);
			}
			else if (is_lead_ident_char(c))
			{
				init_flx_pi (FLX_PI(hcl));
				FEED_CONTINUE (hcl, HCL_FLX_PLAIN_IDENT);
			}
			else
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_BACKSLASH, TOKEN_LOC(hcl), HCL_NULL, "invalid token character - %c", c);
				return -1;
			}
			goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_backslashed (hcl_t* hcl, hcl_ooci_t c)
{
	if (is_linebreak(c))
	{
		FEED_CONTINUE (hcl, HCL_FLX_START);
		return 1; /* consumed */
	}

	hcl_setsynerrbfmt (hcl, HCL_SYNERR_BACKSLASH, TOKEN_LOC(hcl), TOKEN_NAME(hcl), "stray backslash");
	return -1;
}

static int flx_comment (hcl_t* hcl, hcl_ooci_t c)
{
	if (is_linebreak(c))
	{
		FEED_CONTINUE (hcl, HCL_FLX_START);
		/* don't consume the line break together with the comment text
		 * if a comment text is located at the back of the line in the
		 * LANG_ENABLE_EOL mode.
		 * TODO: Consider removing this check because not consuming it
		 *       in another mode doesn't cause a problem. */
		if ((hcl->option.trait & HCL_TRAIT_LANG_ENABLE_EOL)) return 0; /* not consumed */
	}
	return 1; /* consumed */
}

static int flx_delim_token (hcl_t* hcl, hcl_ooci_t c)
{
	if (find_delim_token_char(hcl, c, FLX_DT(hcl)->row_start, FLX_DT(hcl)->row_end, FLX_DT(hcl)->col_next, FLX_DT(hcl)))
	{
		if (FLX_DT(hcl)->row_start == FLX_DT(hcl)->row_end &&
		    FLX_DT(hcl)->col_next == delim_token_tab[FLX_DT(hcl)->row_start].t_len)
		{
			/* complete token and switch to the HCL_FLX_START state */
			FEED_WRAP_UP_WITH_CHAR (hcl, c, delim_token_tab[FLX_DT(hcl)->row_start].t_type);
		}
		else
		{
			ADD_TOKEN_CHAR(hcl, c);
		}
		goto consumed;
	}
	else
	{
		/* the longest match so far */
		FEED_WRAP_UP(hcl, delim_token_tab[FLX_DT(hcl)->row_start].t_type);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_dollared_ident (hcl_t* hcl, hcl_ooci_t c)
{
	hcl_flx_di_t* di = FLX_DI(hcl);

	/* di->char_count doesn't include the first '$' */

	if (is_delim_char(c))
	{
		hcl_tok_type_t tok_type;

		if (di->char_count == 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, FLX_LOC(hcl), HCL_NULL,
				"no valid character after dollar sign");
			return -1;
		}

		if (get_directive_token_type(hcl, &tok_type) <= -1)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), TOKEN_NAME(hcl),
				"invalid dollar-prefixed identifier '%.*js'", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
			return -1;
		}
		else
		{
			FEED_WRAP_UP (hcl, tok_type);
			goto not_consumed;
		}
	}
	else if (is_ident_char(c))
	{
		if (di->char_count == 0)
		{
			if (!is_lead_ident_char(c))
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
					"'%c' prohibited as first character after '%.*js'",
					c, TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
				return -1;
			}
		}

		ADD_TOKEN_CHAR (hcl, c);
		di->char_count++;
		goto consumed;
	}
	else
	{
		hcl_setsynerrbfmt (
			hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
			"invalid dollar-prefixed identifier character '%jc' after '%.*js'", c,
			TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_token (hcl_t* hcl, hcl_ooci_t c)
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
	 * #( )     qlist
	 * #{ }     dictionary
	 * #"..."   symbol literal
	 */

	if (is_binop_char(c))
	{
		reset_flx_token (hcl);
		FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_HMARKED_BINOP);
		goto consumed;
	}

	switch (c)
	{
		case '#':
		case '!':
			/* ## comment start
			 * #! also comment start.
			 * ; comment start */
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_COMMENT);
			goto consumed;

		/* --------------------------- */

	#if 0
		case 'x': /* hexadecimal number */
			init_flx_hn (FLX_HN(hcl), HCL_TOK_RADNUMLIT, HCL_SYNERR_NUMLIT, 16);
			goto radixed_number;

		case 'o': /* octal number */
			init_flx_hn (FLX_HN(hcl), HCL_TOK_RADNUMLIT, HCL_SYNERR_NUMLIT, 8);
			goto radixed_number;
	#endif

		case 'b': /* binary number or byte array */
		case 'B':
		case 'c': /* character array */
		case 'C':
			/* if #b is followed by [, it is a starter for a byte array */
			init_flx_hbc (FLX_HBC(hcl), c);
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_HMARKED_BC);
			break;

		/* --------------------------- */
		case '\\':
			init_flx_hc (FLX_HC(hcl));
			FEED_CONTINUE_WITH_CHAR (hcl, c, HCL_FLX_HMARKED_CHAR);
			goto consumed;

		/* --------------------------- */
		case '[': /* #[ */
			FEED_WRAP_UP_WITH_CHAR (hcl, c, HCL_TOK_APAREN);
			goto consumed;

		case '(': /* #( */
			FEED_WRAP_UP_WITH_CHAR (hcl, c, HCL_TOK_QLPAREN);
			goto consumed;

		case '{': /* #{ */
			FEED_WRAP_UP_WITH_CHAR (hcl, c, HCL_TOK_DLPAREN);
			goto consumed;

		case '"': /* #" - double-quoted symbol */
			reset_flx_token (hcl);
			init_flx_qt (FLX_QT(hcl), HCL_TOK_SYMLIT, HCL_SYNERR_SYMLIT, c, '\\', 0, HCL_TYPE_MAX(hcl_oow_t), 0);
			FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* discard prefix, quote and move on */
			goto consumed;

		/* --------------------------- */
		default:
			init_flx_hi (FLX_HI(hcl));
			FEED_CONTINUE (hcl, HCL_FLX_HMARKED_IDENT);
			goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_char (hcl_t* hcl, hcl_ooci_t c)
{
	hcl_flx_hc_t* hc = FLX_HC(hcl);

	if (is_delim_char(c))
	{
		if (hc->char_count == 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_CHARLIT, TOKEN_LOC(hcl), TOKEN_NAME(hcl),
				"no valid character in character literal %.*js", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
			return -1;
		}

		if (TOKEN_NAME_LEN(hcl) >= 4)
		{
			int max_digit_count = 0;

			if (TOKEN_NAME_CHAR(hcl, 2) == 'x')
			{
				hcl_oow_t i;
				max_digit_count = 2;

			hexcharlit:
				if (TOKEN_NAME_LEN(hcl) - 3 > max_digit_count)
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_CHARLIT, TOKEN_LOC(hcl), TOKEN_NAME(hcl),
						"invalid hexadecimal character character literal %.*js", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
					return -1;
				}
				c = 0;
				for (i = 3; i < TOKEN_NAME_LEN(hcl); i++)
				{
					if (!is_xdigit_char(TOKEN_NAME_CHAR(hcl, i)))
					{
						hcl_setsynerrbfmt (hcl, HCL_SYNERR_CHARLIT, TOKEN_LOC(hcl), TOKEN_NAME(hcl),
							"invalid hexadecimal character character literal %.*js", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
						return -1;
					}
					c = c * 16 + HCL_CHAR_TO_NUM(TOKEN_NAME_CHAR(hcl, i), 16); /* don't care if it is for 'p' */
				}
			}
		#if (HCL_SIZEOF_OOCH_T >= 2)
			else if (TOKEN_NAME_CHAR(hcl, 2) == 'u')
			{
				max_digit_count = 4;
				goto hexcharlit;
			}
		#endif
		#if (HCL_SIZEOF_OOCH_T >= 4)
			else if (TOKEN_NAME_CHAR(hcl, 2) == 'U')
			{
				max_digit_count = 8;
				goto hexcharlit;
			}
		#endif
			else if (does_token_name_match(hcl, VOCA_CHAR_BACKSPACE)) c = '\b';
			else if (does_token_name_match(hcl, VOCA_CHAR_LINEFEED))  c = '\n';
			else if (does_token_name_match(hcl, VOCA_CHAR_NEWLINE))   c = '\n'; 	/* TODO: convert it to host newline convention. how to handle if it's composed of 2 letters like \r\n? */
			else if (does_token_name_match(hcl, VOCA_CHAR_NUL))       c = '\0';  /* null character. not the object null */
			else if (does_token_name_match(hcl, VOCA_CHAR_PAGE))      c = '\f';
			else if (does_token_name_match(hcl, VOCA_CHAR_RETURN))    c = '\r';
			else if (does_token_name_match(hcl, VOCA_CHAR_RUBOUT))    c = '\x7F'; /* DEL */
			else if (does_token_name_match(hcl, VOCA_CHAR_SPACE))     c = ' ';
			else if (does_token_name_match(hcl, VOCA_CHAR_TAB))       c = '\t';
			else if (does_token_name_match(hcl, VOCA_CHAR_VTAB))      c = '\v';
			else
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_CHARLIT, TOKEN_LOC(hcl), TOKEN_NAME(hcl),
					"invalid character literal %.*js", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
				return -1;
			}
		}
		else
		{
			HCL_ASSERT (hcl, TOKEN_NAME_LEN(hcl) == 3);
			c = TOKEN_NAME_CHAR(hcl, 2);
		}

		/* reset the token name to the converted character */
		CLEAR_TOKEN_NAME (hcl);
		ADD_TOKEN_CHAR (hcl, c);
		FEED_WRAP_UP (hcl, HCL_TOK_CHARLIT);
		goto not_consumed;
	}
	else
	{
		ADD_TOKEN_CHAR (hcl, c);
		hc->char_count++;
		goto consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_bc (hcl_t* hcl, hcl_ooci_t c)
{
	hcl_flx_hbc_t* hb = FLX_HBC(hcl);

	if (c == '[')
	{
		/* #b[ - byte array starter */
		/* TODO: more types.. #c[  #w[ .. #u32[ ... etc */
		/*                  char-array word-array 32bit-int-array etc */
		hcl_tok_type_t tt;
		tt = (hb->start_c == 'b' || hb->start_c == 'B')? HCL_TOK_BAPAREN: HCL_TOK_CAPAREN;
		FEED_WRAP_UP_WITH_CHAR (hcl, c, tt);
		goto consumed;
	}
	else
	{
		hcl_ooch_t start_c = hb->start_c;
		reset_flx_token (hcl);
		FEED_CONTINUE_WITH_CHAR (hcl, start_c, HCL_FLX_HMARKED_IDENT);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_binop (hcl_t* hcl, hcl_ooci_t c)
{
	if (is_binop_char(c))
	{
		ADD_TOKEN_CHAR(hcl, c);
		goto consumed;
	}
	else if (is_delim_char(c))
	{
		FEED_WRAP_UP(hcl, HCL_TOK_SYMLIT);
		goto not_consumed;
	}
	else
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_SYMLIT,
			TOKEN_LOC(hcl), HCL_NULL /* no token name as incomplete */,
			"invalid binary selector character '%jc' after #%.*js",
			c, TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_hmarked_ident (hcl_t* hcl, hcl_ooci_t c)
{
	hcl_flx_hi_t* hi = FLX_HI(hcl);

	/* hi->char_count doesn't include the first '#' */

	if (is_delim_char(c))
	{
		if (hi->char_count == 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_SYMLIT, FLX_LOC(hcl), HCL_NULL,
				"no valid character after hash sign");
			return -1;
		}

		FEED_WRAP_UP (hcl, HCL_TOK_SYMLIT);
		goto not_consumed;
	}
	else if (is_ident_char(c))
	{
		if (hi->char_count == 0)
		{
			if (!is_lead_ident_char(c))
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
					"'%c' prohibited as first character of symbol", c);
				return -1;
			}
		}

		ADD_TOKEN_CHAR (hcl, c);
		hi->char_count++;
		goto consumed;
	}
	else
	{
		hcl_setsynerrbfmt (
			hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
			"invalid symbol character '%jc' after '%.*js'", c,
			TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_plain_ident (hcl_t* hcl, hcl_ooci_t c) /* identifier */
{
	hcl_flx_pi_t* pi = FLX_PI(hcl);

	if (is_delim_char(c)) /* [NOTE] . is one of the delimiter character */
	{
		hcl_oow_t start;
		hcl_oocs_t seg;
		hcl_tok_type_t tok_type;

		if (pi->seg_len == 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
				"blank segment after '%.*js'", TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
			return -1;
		}

		start = TOKEN_NAME_LEN(hcl) - pi->seg_len;
		seg.ptr = &TOKEN_NAME_CHAR(hcl, start);
		seg.len = pi->seg_len;
		if (seg.ptr[seg.len - 1] == '-')
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), TOKEN_NAME(hcl),
				"'%c' prohibited as last character of identifier or identifier segment",
				seg.ptr[seg.len - 1]);
			return -1;
		}
		tok_type = classify_ident_token(hcl, &seg);
		if (tok_type != HCL_TOK_IDENT)
		{
			if (pi->seg_count == 0 && (tok_type == HCL_TOK_SELF || tok_type == HCL_TOK_SUPER))
			{
				/* allowed if it begins with self. or super. */
				pi->is_cla = 1; /* mark that it's prefixed with self or super */
			}
			else
			{
				/* for example, if.if.abc - flag the error after having consumed all the segments */
				pi->non_ident_seg_count++;
				pi->last_non_ident_type = tok_type;
			}
		}

		pi->seg_len = 0; /* the length of the segment to be worked on */
		pi->seg_count++; /* total number of segments completed */

		if (c == '.')
		{
			/* move on to the next segment */
			ADD_TOKEN_CHAR(hcl, c);
			pi->char_count++;
			goto consumed;
		}

		/* finish */
		if (pi->non_ident_seg_count > 0)
		{
			if (pi->seg_count == 1)
			{
				FEED_WRAP_UP (hcl, pi->last_non_ident_type);
				goto not_consumed;
			}
			else
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), TOKEN_NAME(hcl), "wrong multi-segment identifier");
				return -1;
			}
		}

		/* if single-segmented, perform classification(call classify_ident_token()) again
		 * bcause self and super as the first segment have not been marked as a non-identifier above */
		tok_type = (pi->seg_count == 1? classify_ident_token(hcl, TOKEN_NAME(hcl)):
		                                (pi->is_cla? HCL_TOK_IDENT_DOTTED_CLA: HCL_TOK_IDENT_DOTTED));
		FEED_WRAP_UP (hcl, tok_type);
		goto not_consumed;
	}
	else if (is_ident_char(c))
	{
		if (pi->seg_len == 0)
		{
			if (!is_lead_ident_char(c))
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
					"'%c' prohibited as first character of identifier or identifier segment after '%.*js'",
					c, TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
				return -1;
			}
		}

		ADD_TOKEN_CHAR(hcl, c);
		pi->char_count++;
		pi->seg_len++;
		goto consumed;
	}
	else
	{
		hcl_setsynerrbfmt (
			hcl, HCL_SYNERR_ILTOK, TOKEN_LOC(hcl), HCL_NULL,
			"invalid identifier character '%jc' after '%.*js'", c,
			TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_binop (hcl_t* hcl, hcl_ooci_t c) /* binary operator/selector */
{
#if 0
	hcl_flx_binop_t* binop = FLX_BINOP(hcl);
#endif

	if (is_binop_char(c))
	{
		ADD_TOKEN_CHAR (hcl, c);
		goto consumed;
	}
	else
	{
		FEED_WRAP_UP (hcl, HCL_TOK_BINOP);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_plain_number (hcl_t* hcl, hcl_ooci_t c) /* number */
{
	hcl_flx_pn_t* pn = FLX_PN(hcl);

	if (is_radixed_digit_char(c, pn->radix))
	{
		ADD_TOKEN_CHAR (hcl, c);
		pn->digit_count[pn->fpdec]++;
		if (pn->tok_type == HCL_TOK_NUMLIT)
		{
			hcl_oow_t cand = pn->radix_cand * 10 + (c - '0');
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
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_NUMLIT, FLX_LOC(hcl), HCL_NULL,
					"invalid use of decimal point after radixed number '%.*js'",
					TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
				return -1;
			}
			pn->fpdec = 1;
			pn->tok_type = HCL_TOK_FPDECLIT;
			ADD_TOKEN_CHAR (hcl, c);
			goto consumed;
		}

		if (pn->digit_count[0] == 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_NUMLIT, TOKEN_LOC(hcl), HCL_NULL,
				"invalid numeric literal with no digit after '%.*js'",
				TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
			return -1;
		}
		else if (pn->fpdec && pn->digit_count[1] == 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_NUMLIT, TOKEN_LOC(hcl), HCL_NULL,
				"invalid numeric literal with no digit after decimal point '%.*js'",
				TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
			return -1;
		}

		FEED_WRAP_UP (hcl, pn->tok_type);
		goto not_consumed;
	}
	else
	{
		if (!pn->fpdec && pn->digit_count[0] == 1 && pn->start_digit == '0' && pn->tok_type == HCL_TOK_NUMLIT)
		{
			/* prefixed with 0 */
			switch (c)
			{
				case 'x':
					pn->tok_type = HCL_TOK_RADNUMLIT;
					pn->radix = 16;
					break;

				case 'o':
					pn->tok_type = HCL_TOK_RADNUMLIT;
					pn->radix = 8;
					break;

				case 'b':
					pn->tok_type = HCL_TOK_RADNUMLIT;
					pn->radix = 2;
					break;

				case 'p':
					pn->tok_type = HCL_TOK_SMPTRLIT;
					pn->radix = 16;
					break;

				case 'e':
					pn->tok_type = HCL_TOK_ERRLIT;
					pn->radix = 10;
					break;

				default:
					goto other_char;
			}

			ADD_TOKEN_CHAR (hcl, c);
			pn->digit_count[0] = 0;
			goto consumed;
		}

	other_char:
		if (!pn->fpdec && pn->tok_type == HCL_TOK_NUMLIT && pn->digit_count[0] > 0 && c == 'r')
		{
			/* 16rABCD */
			if (pn->radix_cand_overflown || pn->radix_cand < 2 || pn->radix_cand > 36)
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_NUMLIT, TOKEN_LOC(hcl), HCL_NULL,
					"unsupported radix '%.*js' before '%jc'",
					TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl), c);
				return -1;
			}

			pn->tok_type = HCL_TOK_RADNUMLIT;
			pn->radix = pn->radix_cand;
			ADD_TOKEN_CHAR (hcl, c);
			pn->digit_count[0] = 0;
			goto consumed;
		}

		hcl_setsynerrbfmt (hcl, HCL_SYNERR_NUMLIT, TOKEN_LOC(hcl), HCL_NULL,
			"invalid numeric literal character '%jc' after '%.*js'",
			c, TOKEN_NAME_LEN(hcl), TOKEN_NAME_PTR(hcl));
		return -1;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_quoted_token (hcl_t* hcl, hcl_ooci_t c) /* string, character */
{
	hcl_flx_qt_t* qt = FLX_QT(hcl);
	hcl_loc_t synerr_loc = *TOKEN_LOC(hcl);

	if (c == HCL_OOCI_EOF) goto invalid_token;

	if (qt->is_byte && c > 0xFF)
	{
		synerr_loc = *FLX_LOC(hcl);
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
				ADD_TOKEN_CHAR (hcl, qt->c_acc);
				qt->escaped = 0;
			}
			goto consumed;
		}
		else
		{
			ADD_TOKEN_CHAR (hcl, qt->c_acc);
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
				ADD_TOKEN_CHAR (hcl, qt->c_acc);
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
				ADD_TOKEN_CHAR (hcl, qt->c_acc);
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
				ADD_TOKEN_CHAR (hcl, qt->c_acc);
				qt->escaped = 0;
			}
			goto consumed;
		}
		else
		{
			hcl_ooch_t rc;
			rc = (qt->escaped == 2)? 'x':
			     (qt->escaped == 4)? 'u': 'U';
			if (qt->digit_count == 0)
				ADD_TOKEN_CHAR (hcl, rc);
			else ADD_TOKEN_CHAR (hcl, qt->c_acc);

			qt->escaped = 0;
		}
	}

	if (qt->escaped == 0 && c == qt->end_char)
	{
		/* terminating quote */

		/* qt->tok_type + qt->is_byte assumes that the token types for
		 * byte-string and byte-character literals are 1 greater than
		 * string and character literals. see the definition of
		 * hcl_tok_type_t in hcl-prv.h.
		 * qt->is_byte is always 0 for HCL_TOK_SYMLIT. */
		FEED_WRAP_UP (hcl, (hcl_tok_type_t)(qt->tok_type + qt->is_byte)); /* HCL_TOK_STRLIT or HCL_TOK_CHARLIT */
		if (TOKEN_NAME_LEN(hcl) < qt->min_len) goto invalid_token;
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
	#if (HCL_SIZEOF_OOCH_T >= 2)
		else if (c == 'u' && !qt->is_byte)
		{
			#if 0
			if (qt->is_byte)
			{
				synerr_loc = *FLX_LOC(hcl);
				goto invalid_token;
			}
			#endif
			qt->escaped = 4;
			qt->digit_count = 0;
			qt->c_acc = 0;
			goto consumed;
		}
	#endif
	#if (HCL_SIZEOF_OOCH_T >= 4)
		else if (c == 'U' && !qt->is_byte)
		{
			#if 0
			if (qt->is_byte)
			{
				synerr_loc = *FLX_LOC(hcl);
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
			ADD_TOKEN_CHAR (hcl, qt->esc_char);
		}

		qt->escaped = 0;
	}

	ADD_TOKEN_CHAR (hcl, c);

consumed:
	if (TOKEN_NAME_LEN(hcl) > qt->max_len) goto invalid_token;
	return 1;

invalid_token:
	hcl_setsynerr (hcl, qt->synerr_code, &synerr_loc, HCL_NULL);
	return -1;
}

static int flx_signed_token (hcl_t* hcl, hcl_ooci_t c)
{
	hcl_flx_st_t* st = FLX_ST(hcl);

	HCL_ASSERT (hcl, st->char_count == 0);
	if (is_digit_char(c))
	{
		/* the sign is not part of the pn->digit_count[0] but is
		 * in the current token buffer. pn->digit_count[0] doesn't
		 * include the sign and calling init_flx_pn() to make it 0
		 * is good enough. */
		init_flx_pn (FLX_PN(hcl), c);
		FEED_CONTINUE (hcl, HCL_FLX_PLAIN_NUMBER);
		goto not_consumed;
	}
	else
	{
	#if 0
		init_flx_pi (FLX_PI(hcl));

		/* the sign is already in the token name buffer.
		 * adjust the state data for the sign. */
		HCL_ASSERT (hcl, TOKEN_NAME_LEN(hcl) == 1);
		FLX_PI(hcl)->char_count++;
		FLX_PI(hcl)->seg_len++;

		/* let refeeding of 'c' happen at the next iteration */
		FEED_CONTINUE (hcl, HCL_FLX_PLAIN_IDENT);
		goto not_consumed;
	#else
		/* the leading sign must be + or - and must be one of the binop chars. */
		HCL_ASSERT (hcl, is_binop_char(st->sign_c));/* must be + or - and they must be one of the binop chars. */

		/* switch to binop mode */
		init_flx_binop (FLX_BINOP(hcl));
		HCL_ASSERT (hcl, TOKEN_NAME_LEN(hcl) == 1);
		FEED_CONTINUE (hcl, HCL_FLX_BINOP);
		goto not_consumed;
	#endif
	}

consumed:
	return 1;

not_consumed:
	return 0;
}

static int flx_bc_prefix (hcl_t* hcl, hcl_ooci_t c)
{
	hcl_flx_bcp_t* bcp = FLX_BCP(hcl);


	if (c == '\"') /* b" B" c" C" */
	{
		int is_byte = (bcp->start_c == 'b' || bcp->start_c == 'B');
		reset_flx_token (hcl);
		init_flx_qt (FLX_QT(hcl), HCL_TOK_STRLIT, HCL_SYNERR_STRLIT, c, '\\', 0, HCL_TYPE_MAX(hcl_oow_t), is_byte);
		FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* discard prefix, quote and move on */
		goto consumed;
	}
	else if (c == '\'') /* b' B' c' C' */
	{
		int is_byte = (bcp->start_c == 'b' || bcp->start_c == 'B');
		reset_flx_token (hcl);
		init_flx_qt (FLX_QT(hcl), HCL_TOK_CHARLIT, HCL_SYNERR_CHARLIT, c, '\\', 1, 1, is_byte);
		FEED_CONTINUE (hcl, HCL_FLX_QUOTED_TOKEN); /* dicard prefix, quote, and move on */
		goto consumed;
	}
	else
	{
		/* not followed by a quote. switch to the plain identifier */
		init_flx_pi (FLX_PI(hcl));

		/* the prefix is already in the token buffer. just adjust state data */
		FLX_PI(hcl)->char_count++;
		FLX_PI(hcl)->seg_len++;

		/* refeed c */
		FEED_CONTINUE (hcl, HCL_FLX_PLAIN_IDENT);
		goto not_consumed;
	}

consumed:
	return 1;

not_consumed:
	return 0;
}


/* ------------------------------------------------------------------------ */


static int feed_char (hcl_t* hcl, hcl_ooci_t c)
{
/*hcl_logbfmt (hcl, HCL_LOG_STDERR, "FEED->[%jc] %d STATE->%d\n", c, c, FLX_STATE(hcl));*/

	switch (FLX_STATE(hcl))
	{
		case HCL_FLX_START:            return flx_start(hcl, c);
		case HCL_FLX_BACKSLASHED:      return flx_backslashed(hcl, c);
		case HCL_FLX_COMMENT:          return flx_comment(hcl, c);
		case HCL_FLX_DELIM_TOKEN:      return flx_delim_token(hcl, c);
		case HCL_FLX_DOLLARED_IDENT:   return flx_dollared_ident(hcl, c);
		case HCL_FLX_HMARKED_TOKEN:    return flx_hmarked_token(hcl, c);
		case HCL_FLX_HMARKED_BC:       return flx_hmarked_bc(hcl, c);
		case HCL_FLX_HMARKED_BINOP:    return flx_hmarked_binop(hcl, c);
		case HCL_FLX_HMARKED_CHAR:     return flx_hmarked_char(hcl, c);
		case HCL_FLX_HMARKED_IDENT:    return flx_hmarked_ident(hcl, c);

		case HCL_FLX_PLAIN_IDENT:      return flx_plain_ident(hcl, c);
		case HCL_FLX_BINOP:            return flx_binop(hcl, c);
		case HCL_FLX_PLAIN_NUMBER:     return flx_plain_number(hcl, c);
		case HCL_FLX_QUOTED_TOKEN:     return flx_quoted_token(hcl, c);
		case HCL_FLX_SIGNED_TOKEN:     return flx_signed_token(hcl, c);
		case HCL_FLX_BC_PREFIX:        return flx_bc_prefix(hcl, c);

		default:
			/* unknown state */
			break;
	}

	HCL_ASSERT (hcl, !"internal error - this must never happen");
	hcl_seterrbfmt (hcl, HCL_EINTERN, "internal error - unknown flx state - %d", FLX_STATE(hcl));
	return -1;
}

static void feed_update_lx_loc (hcl_t* hcl, hcl_ooci_t ch)
{
	if (is_linebreak(ch))
	{
		hcl->c->feed.lx.loc.line++;
		hcl->c->feed.lx.loc.colm = 1;
	}
	else
	{
		hcl->c->feed.lx.loc.colm++;
	}
}

static hcl_oow_t move_cci_residue_bytes (hcl_io_cciarg_t* curinp)
{
	hcl_oow_t cpl;

	cpl = HCL_COUNTOF(curinp->rsd.buf) - curinp->rsd.len;
	if (cpl > 0)
	{
		hcl_oow_t avail;
		avail = curinp->b.len - curinp->b.pos; /* available in the read buffer */
		if (cpl > avail) cpl = avail;
		HCL_MEMCPY(&curinp->rsd.buf[curinp->rsd.len], &curinp->buf.b[curinp->b.pos], cpl);
		curinp->rsd.len += cpl;
		curinp->b.pos += cpl; /* advance the position because the bytes moved to the residue buffer */
	}
	return curinp->rsd.len;
}

static int feed_from_includee (hcl_t* hcl)
{
	int x;
	hcl_ooch_t c;
	hcl_io_cciarg_t* curinp;

	HCL_ASSERT (hcl, hcl->c->curinp != HCL_NULL && hcl->c->curinp != &hcl->c->cci_arg);

	curinp = hcl->c->curinp;
	do
	{
		hcl_oow_t taken;

	#if defined(HCL_OOCH_IS_UCH)
		if (curinp->byte_oriented)
		{
			hcl_cmgr_t* cmgr;
			const hcl_uint8_t* inpptr;
			hcl_oow_t inplen, n;

			cmgr = HCL_CMGR(hcl);

		start_over:
			if (curinp->b.pos >= curinp->b.len)
			{
				x = hcl->c->cci_rdr(hcl, HCL_IO_READ_BYTES, curinp);
				if (x <= -1)
				{
					const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
					hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to read bytes from %js - %js", curinp->name, orgmsg);
					goto oops;
				}

				if (curinp->xlen <= 0)
				{
					/* got EOF from an included stream */
					if (curinp->rsd.len > 0)
					{
						hcl_seterrbfmt (hcl, HCL_EECERR, "incomplete byte sequence in %js", curinp->name);
						goto oops;
					}
					feed_end_include (hcl);
					curinp = hcl->c->curinp;
					continue;
				}

				curinp->b.pos = 0;
				curinp->b.len = curinp->xlen;
			}

			if (curinp->rsd.len > 0)
			{
				/* there is data in the residue buffer. use the residue buffer to
				 * locate a proper multi-byte sequence */
				HCL_ASSERT (hcl, curinp->b.pos == 0);
				inplen = move_cci_residue_bytes(curinp);
				inpptr = &curinp->rsd.buf[0];
			}
			else
			{
				inplen = curinp->b.len - curinp->b.pos;
				inpptr = &curinp->buf.b[curinp->b.pos];
			}

			n = cmgr->bctouc((const hcl_bch_t*)inpptr, inplen, &c);
			if (n == 0) /* invalid sequence */
			{
				/* TODO: more accurate location of the invalid byte sequence */
				hcl_seterrbfmt (hcl, HCL_EECERR, "invalid byte sequence in %js", curinp->name);
				goto oops;
			}
			if (n > inplen) /* incomplete sequence */
			{
				HCL_ASSERT (hcl, curinp->rsd.len < HCL_COUNTOF(curinp->rsd.buf));
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
				x = hcl->c->cci_rdr(hcl, HCL_IO_READ, curinp);
				if (x <= -1)
				{
					/* TODO: more accurate location of failure */
					const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
					hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to read %js - %js", curinp->name, orgmsg);
					goto oops;
				}
				if (curinp->xlen <= 0)
				{
					/* got EOF from an included stream */
					feed_end_include (hcl);
					curinp = hcl->c->curinp;
					continue;
				}

				curinp->b.pos = 0;
				curinp->b.len = curinp->xlen;
			}

			c = curinp->buf.c[curinp->b.pos];
			taken = 1;
	#if defined(HCL_OOCH_IS_UCH)
		}
	#endif

		x = feed_char(hcl, c);
		if (x <= -1) goto oops;
		if (x >= 1)
		{
			/* consumed */
			feed_update_lx_loc (hcl, c);
			curinp->b.pos += taken;
	#if defined(HCL_OOCH_IS_UCH)
			curinp->rsd.len = 0; /* clear up the residue byte buffer. needed for byte reading only */
	#endif
		}

		if (hcl->c->feed.rd.do_include_file)
		{
			/* feed_process_token(), called for the "filename" token for the #include
			 * directive, sets hcl->c->feed.rd.do_include_file to 1 instead of attepmting
			 * to include the file. the file inclusion is attempted here after the return
			 * value of feed_char() is used to advance the hcl->c->curinp->b.pos pointer. */
			hcl->c->feed.rd.do_include_file = 0; /* clear this regardless of inclusion result */
			if (feed_begin_include(hcl) <= -1) goto oops;
			curinp = hcl->c->curinp;
		}
	}
	while (curinp != &hcl->c->cci_arg);

	return 0;

oops:
	while (feed_end_include(hcl) >= 1) /* loop */; /* roll back the entire inclusion chain */
	return -1;
}

int hcl_beginfeed (hcl_t* hcl, hcl_on_cnode_t on_cnode)
{
	/* if the fed data contains #include, you must call hcl_attachccio() first.
	 * file includsion requires the ccio handler to be implemented. */

	if (!hcl->c && init_compiler(hcl) <= -1) return -1;

	init_feed (hcl);
	if (on_cnode) hcl->c->feed.on_cnode = on_cnode;
	/* if you pass HCL_NULL for on_cnode, hcl->c->feed.on_cnode resets
	 * back to the default handler in init_feed() */

	return 0;
}

int hcl_endfeed (hcl_t* hcl)
{
	return hcl_feed(hcl, HCL_NULL, 0);
}

int hcl_feedpending (hcl_t* hcl)
{
	return !(hcl->c->r.st == HCL_NULL && FLX_STATE(hcl) == HCL_FLX_START);
}

void hcl_resetfeedloc (hcl_t* hcl)
{
	hcl->c->feed.lx.loc.line = 1;
	hcl->c->feed.lx.loc.colm = 1;
	hcl->c->feed.lx.loc.file = HCL_NULL;
}

int hcl_feed (hcl_t* hcl, const hcl_ooch_t* data, hcl_oow_t len)
{
/* TODO: need to return the number of processed characters?
 *       need to stop after the first complete expression? */
	hcl_oow_t i;
	int x;

	HCL_ASSERT (hcl, hcl->c != HCL_NULL);

#if defined(HCL_OOCH_IS_UCH)
	if (hcl->c->feed.rsd.len > 0 && !hcl->c->feed.rsd.no_check)
	{
		hcl_seterrbfmt (hcl, HCL_EPERM, "feed disallowed for incomplete sequence pending more feeding");
		return -1;
	}
#endif

	if (data)
	{
		for (i = 0; i < len; )
		{
			x = feed_char(hcl, data[i]);
			if (x <= -1)
			{
				feed_update_lx_loc (hcl, data[i]); /* update the location upon an error too */
				goto oops; /* TODO: return the number of processed characters via an argument? */
			}

			if (x > 0)
			{
				/* consumed */
				feed_update_lx_loc (hcl, data[i]);
				i += x; /* x is supposed to be 1. otherwise, some characters may get skipped. */
			}

			if (hcl->c->feed.rd.do_include_file)
			{
				hcl->c->feed.rd.do_include_file = 0; /* done regardless of inclusion result */
				if (feed_begin_include(hcl) <= -1) goto oops;
			}

			if (hcl->c->curinp && hcl->c->curinp != &hcl->c->cci_arg && feed_from_includee(hcl) <= -1)
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
			x = feed_char(hcl, HCL_OOCI_EOF);
			if (x <= -1) goto oops;
			i += x;
		}
	}

	return 0;

oops:
	feed_reset_reader_state (hcl);

	/* if enter_list() is in feed_process_token(), the stack grows.
	 * leave_list() pops an element off the stack. the stack can be
	 * not empty if an error occurs outside feed_process_token() after
	 * leave_list() in it. for example,
	 *
	 *  (              #aaa
	 *  ^              ^
	 *  leave_list()   error in flx_hmarked_ident() before a full cnode is processed
	 */
	feed_clean_up_reader_stack (hcl);
	feed_continue (hcl, HCL_FLX_START);
	return -1;
}

int hcl_feedbchars (hcl_t* hcl, const hcl_bch_t* data, hcl_oow_t len)
{
#if defined(HCL_OOCH_IS_UCH)
	hcl_uch_t outbuf[128];
	hcl_oow_t inlen, outlen, inpos, brwlen;
	int n;

	HCL_ASSERT (hcl, hcl->c != HCL_NULL);

	inpos = 0;

	if (hcl->c->feed.rsd.len > 0) /* residue length greater than 0 */
	{
		hcl_oow_t rsdlen;

		/* handle the residue bytes from the previous feeding */
		rsdlen = hcl->c->feed.rsd.len; /* original residue length*/
		brwlen = HCL_COUNTOF(hcl->c->feed.rsd.buf) - rsdlen;
		if (len < brwlen) brwlen = len;
		HCL_MEMCPY(&hcl->c->feed.rsd.buf[rsdlen], data, brwlen);
		hcl->c->feed.rsd.len += brwlen;

		inlen = hcl->c->feed.rsd.len;
		outlen = 1; /* ensure that it can only convert 1 character */
		n = hcl_conv_bchars_to_uchars_with_cmgr(hcl->c->feed.rsd.buf, &inlen, outbuf, &outlen, HCL_CMGR(hcl), 0);

		if (outlen > 0)
		{
			int x;
			hcl->c->feed.rsd.no_check = 1;
			x = hcl_feed(hcl, outbuf, outlen);
			hcl->c->feed.rsd.no_check = 0;
			if (x <= -1) return -1;
		}

		if (n <= -1)
		{
			if (n == -3 || (n == -2 && outlen > 0))
			{
				/* n == -3. invalid sequence. more feeding is required */
				/* n == -2. there were extra bytes for the second character in the input */
				HCL_ASSERT (hcl, (n == -3 && inlen == 0 && outlen == 0) || (n == -2 && inlen > 0));
				/* nothing to do. carry on */
			}
			else
			{
				hcl_seterrnum (hcl, (n == -2)? HCL_EBUFFULL: HCL_EECERR);
				return -1;
			}
		}

		/*
		 * | rsdlen   |  brwlen  |
		 * | inlen        |
		 */
		if (inlen < rsdlen)
		{
			HCL_ASSERT (hcl, inlen == 0);
			HCL_ASSERT (hcl, brwlen ==  len);
			/* brwlen needs no change */
			/* hcl->c->feed.rsd.len nees no change */
		}
		else
		{
			HCL_ASSERT (hcl, inlen > rsdlen);
			brwlen = inlen - rsdlen; /* actual bytes borrowed and converted */
			hcl->c->feed.rsd.len = 0;
		}
		inpos += brwlen;
		len -= brwlen;
	}

	while (len > 0)
	{
		inlen = len;
		outlen = HCL_COUNTOF(outbuf);

		/* hcl_convbtouchars() does not differentiate between illegal character and incomplete sequence.
		 * use a lower-level function that hcl_convbtouchars() uses */
		n = hcl_conv_bchars_to_uchars_with_cmgr(&data[inpos], &inlen, outbuf, &outlen, HCL_CMGR(hcl), 0);
		if (outlen > 0 && hcl_feed(hcl, outbuf, outlen) <= -1) return -1;

		if (n <= -1)
		{
			if (n == -2 && outlen > 0) goto ok;

			if (n == -2 || n == -3)
			{
				hcl_oow_t rsdlen;
				HCL_ASSERT (hcl, len > inlen);
				rsdlen = len - inlen;
				HCL_ASSERT (hcl, rsdlen <= HCL_COUNTOF(hcl->c->feed.rsd.buf));
				HCL_MEMCPY (hcl->c->feed.rsd.buf, &data[inpos + inlen], rsdlen);
				hcl->c->feed.rsd.len = len - inlen;
				break;
			}

			hcl_seterrnum (hcl, HCL_EECERR);
			return -1;
		}

	ok:
		inpos += inlen;
		len -= inlen;
	}

	return 0;
#else
	return hcl_feed(hcl, data, len);
#endif
}

int hcl_feeduchars (hcl_t* hcl, const hcl_uch_t* data, hcl_oow_t len)
{
#if defined(HCL_OOCH_IS_UCH)
	return hcl_feed(hcl, data, len);
#else
	hcl_bch_t outbuf[HCL_BCSIZE_MAX * 128];
	hcl_oow_t inlen, outlen, inpos;
	int n;

	inpos = 0;
	while (len > 0)
	{
		inlen = len;
		outlen = HCL_COUNTOF(outbuf);
		n = hcl_convutobchars(hcl, &data[inpos], &inlen, outbuf, &outlen);
		if (outlen > 0 && hcl_feed(hcl, outbuf, outlen) <= -1) return -1;
		inpos += inlen;
		len -= inlen;
		if (n <= -1) return -1;
	}
	return 0;
#endif
}

/*
hcl_setopt (ON_EXPRESSION CALLBACK??? );



hcl_feed (hcl, "(hello) (10)", 12);
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

static void gc_compiler_cb (hcl_t* hcl)
{
	if (hcl->c)
	{
		hcl->c->r.s = hcl_moveoop(hcl, hcl->c->r.s);
		hcl->c->r.e = hcl_moveoop(hcl, hcl->c->r.e);
	}
}

static void fini_compiler_cb (hcl_t* hcl)
{
	/* called before the hcl object is closed */
	if (hcl->c)
	{
		if (hcl->c->cfs.ptr)
		{
			hcl_freemem (hcl, hcl->c->cfs.ptr);
			hcl->c->cfs.ptr = HCL_NULL;
			hcl->c->cfs.top = -1;
			hcl->c->cfs.capa = 0;
		}

		if (hcl->c->tv.s.ptr)
		{
			hcl_freemem (hcl, hcl->c->tv.s.ptr);
			hcl->c->tv.s.ptr = HCL_NULL;
			hcl->c->tv.s.len = 0;
			hcl->c->tv.capa = 0;
			hcl->c->tv.wcount = 0;
		}
		HCL_ASSERT (hcl, hcl->c->tv.capa == 0);
		HCL_ASSERT (hcl, hcl->c->tv.wcount == 0);

		if (hcl->c->cblk.info)
		{
			hcl_freemem (hcl, hcl->c->cblk.info);
			hcl->c->cblk.info = HCL_NULL;
			hcl->c->cblk.info_capa = 0;
			hcl->c->cblk.depth = -1;
		}

		if (hcl->c->clsblk.info)
		{
			hcl_freemem (hcl, hcl->c->clsblk.info);
			hcl->c->clsblk.info = HCL_NULL;
			hcl->c->clsblk.info_capa = 0;
			hcl->c->clsblk.depth = -1;
		}

		if (hcl->c->fnblk.info)
		{
			hcl_freemem (hcl, hcl->c->fnblk.info);
			hcl->c->fnblk.info = HCL_NULL;
			hcl->c->fnblk.info_capa = 0;
			hcl->c->fnblk.depth = -1;
		}

		clear_sr_names (hcl);
		if (hcl->c->tok.name.ptr) hcl_freemem (hcl, hcl->c->tok.name.ptr);

		hcl_detachccio (hcl);

		hcl_freemem (hcl, hcl->c);
		hcl->c = HCL_NULL;
	}
}

static void fini_compiler (hcl_t* hcl)
{
	/* unlike fini_compiler_cb(), this is to be used in some error handling
	 * between init_compiler success and subquent operation failure */
	if (hcl->c)
	{
		hcl_deregcb (hcl, hcl->c->cbp);
		fini_compiler_cb (hcl);
	}
}

static int init_compiler (hcl_t* hcl)
{
	hcl_cb_t cb, * cbp = HCL_NULL;

	HCL_ASSERT (hcl, hcl->c == HCL_NULL);

	HCL_MEMSET (&cb, 0, HCL_SIZEOF(cb));
	cb.on_gc = gc_compiler_cb;
	cb.on_fini = fini_compiler_cb;
	cbp = hcl_regcb(hcl, &cb);
	if (HCL_UNLIKELY(!cbp)) return -1;

	hcl->c = (hcl_compiler_t*)hcl_callocmem(hcl, HCL_SIZEOF(*hcl->c));
	if (HCL_UNLIKELY(!hcl->c))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to allocate compiler - %js", orgmsg);
		hcl_deregcb (hcl, cbp);
		return -1;
	}

	hcl->c->ilchr_ucs.ptr = &hcl->c->ilchr;
	hcl->c->ilchr_ucs.len = 1;

	hcl->c->r.s = hcl->_nil;
	hcl->c->r.e = hcl->_nil;

	hcl->c->cfs.top = -1;
	hcl->c->cblk.depth = -1;
	hcl->c->clsblk.depth = -1;
	hcl->c->fnblk.depth = -1;

	init_feed (hcl);
	hcl->c->cbp = cbp;


	/* initialize the internal cons to represent a cell pointing to `null` in the `car` part */
	hcl->c->fake_cnode.nil.cn_type = HCL_CNODE_NIL;
	hcl->c->fake_cnode.nil.cn_tok.ptr = vocas[VOCA_KW_NIL].str;
	hcl->c->fake_cnode.nil.cn_tok.len = vocas[VOCA_KW_NIL].len;

	hcl->c->fake_cnode.cons_to_nil.cn_type = HCL_CNODE_CONS;
	hcl->c->fake_cnode.cons_to_nil.u.cons.car = &hcl->c->fake_cnode.nil;
	hcl->c->fake_cnode.cons_to_nil.u.cons.cdr = HCL_NULL;
	return 0;
}

int hcl_attachccio (hcl_t* hcl, hcl_io_impl_t cci_rdr)
{
	int n;
	int inited_compiler = 0;
	hcl_io_cciarg_t new_cciarg;

	if (!hcl->c)
	{
		if (init_compiler(hcl) <= -1) return -1;
		inited_compiler = 1;
	}

	if (cci_rdr)
	{
		/* The name field and the includer field are HCL_NULL
		 * for the main stream */
		HCL_MEMSET (&new_cciarg, 0, HCL_SIZEOF(new_cciarg));
		new_cciarg.line = 1;
		new_cciarg.colm = 1;

		/* open the top-level source input stream */
		n = cci_rdr(hcl, HCL_IO_OPEN, &new_cciarg);
		if (n <= -1) goto oops;

		if (hcl->c->cci_rdr)
		{
			/* close the old source input stream */
			hcl->c->cci_rdr (hcl, HCL_IO_CLOSE, &hcl->c->cci_arg);
		}
		hcl->c->cci_rdr = cci_rdr;
		hcl->c->cci_arg = new_cciarg;

		/* clear unneeded source stream names */
		/*clear_sr_names (hcl); <---- TODO: tricky to clean up here */

		/* initialize some other key fields */
		hcl->c->nungots = 0;
		/* the source stream is open. set it as the current input stream */
		hcl->c->curinp = &hcl->c->cci_arg;
	}

	return 0;

oops:
	if (inited_compiler) fini_compiler (hcl);
	return -1;
}

void hcl_detachccio (hcl_t* hcl)
{
	/* an error occurred and control has reached here
	 * probably, some included files might not have been
	 * closed. close them */

	if (hcl->c)
	{
		if (hcl->c->cci_rdr)
		{
			while (hcl->c->curinp != &hcl->c->cci_arg)
			{
				hcl_io_cciarg_t* prev;

				/* nothing much to do about a close error */
				hcl->c->cci_rdr (hcl, HCL_IO_CLOSE, hcl->c->curinp);

				prev = hcl->c->curinp->includer;
				HCL_ASSERT (hcl, hcl->c->curinp->name != HCL_NULL);
				hcl_freemem (hcl, hcl->c->curinp);
				hcl->c->curinp = prev;
			}

			hcl->c->cci_rdr (hcl, HCL_IO_CLOSE, hcl->c->curinp);
			hcl->c->cci_rdr = HCL_NULL; /* ready for another attachment */
		}
	}
}

int hcl_attachudio (hcl_t* hcl, hcl_io_impl_t udi_rdr, hcl_io_impl_t udo_wrtr)
{
	int n;
	hcl_io_udiarg_t new_udiarg;
	hcl_io_udoarg_t new_udoarg;

	if (udi_rdr)
	{
		HCL_MEMSET (&new_udiarg, 0, HCL_SIZEOF(new_udiarg));
		n = udi_rdr(hcl, HCL_IO_OPEN, &new_udiarg);
		if (n <= -1)
		{
			goto oops;
		}
	}

	if (udo_wrtr)
	{
		/* open the new output stream */
		HCL_MEMSET (&new_udoarg, 0, HCL_SIZEOF(new_udoarg));
		n = udo_wrtr(hcl, HCL_IO_OPEN, &new_udoarg);
		if (n <= -1)
		{
			if (udi_rdr) udi_rdr (hcl, HCL_IO_CLOSE, &new_udiarg);
			goto oops;
		}
	}

	if (udi_rdr)
	{
		if (hcl->io.udi_rdr)
		{
			/* close the old input stream */
			hcl->io.udi_rdr (hcl, HCL_IO_CLOSE, &hcl->io.udi_arg);
		}
		hcl->io.udi_rdr = udi_rdr;
		hcl->io.udi_arg = new_udiarg;
	}

	if (udo_wrtr)
	{
		if (hcl->io.udo_wrtr)
		{
			/* close the old output stream */
			hcl->io.udo_wrtr (hcl, HCL_IO_CLOSE, &hcl->io.udo_arg);
		}
		hcl->io.udo_wrtr = udo_wrtr;
		hcl->io.udo_arg = new_udoarg;
	}

	return 0;

oops:
	return -1;
}


void hcl_detachudio (hcl_t* hcl)
{
	if (hcl->io.udi_rdr)
	{
		hcl->io.udi_rdr (hcl, HCL_IO_CLOSE, &hcl->io.udi_arg);
		hcl->io.udi_rdr = HCL_NULL; /* ready for another attachment */
	}

	if (hcl->io.udo_wrtr)
	{
		hcl->io.udo_wrtr (hcl, HCL_IO_CLOSE, &hcl->io.udo_arg);
		hcl->io.udo_wrtr = HCL_NULL; /* ready for another attachment */
	}
}

void hcl_flushudio (hcl_t* hcl)
{
	if (hcl->io.udo_wrtr) hcl->io.udo_wrtr (hcl, HCL_IO_FLUSH, &hcl->io.udo_arg);
}

/* TODO: discard the fwollowing three functions - hcl_setbasesrloc, hcl_readbasesrchar */
void hcl_setbasesrloc (hcl_t* hcl, hcl_oow_t line, hcl_oow_t colm)
{
	hcl->c->cci_arg.line = line;
	hcl->c->cci_arg.colm = colm;
}

hcl_lxc_t* hcl_readbasesrchar (hcl_t* hcl)
{
	/* read a character using the base input stream. the caller must care extra
	 * care when using this function. this function reads the main stream regardless
	 * of the inclusion status and ignores the ungot characters. */
	int n = _get_char(hcl, &hcl->c->cci_arg);
	if (n <= -1) return HCL_NULL;
	return &hcl->c->cci_arg.lxc;
}
