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

hak_cnode_t* hak_makecnode (hak_t* hak, hak_cnode_type_t type, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	hak_cnode_t* cnode;
	hak_oocs_t empty;
	hak_ooch_t dummy;

	if (!tok)
	{
		empty.ptr = &dummy;
		empty.len = 0;
		tok = &empty;
	}
	cnode = (hak_cnode_t*)hak_callocmem(hak, HAK_SIZEOF(*cnode) + HAK_SIZEOF(*tok->ptr) * (tok->len + 1));
	if (HAK_UNLIKELY(!cnode)) return HAK_NULL;

	cnode->cn_type = type;
	cnode->cn_flags = flags;
	cnode->cn_loc = *loc;

	cnode->cn_tok.ptr = (hak_ooch_t*)(cnode + 1);
	cnode->cn_tok.len = tok->len;
	hak_copy_oochars (cnode->cn_tok.ptr, tok->ptr, tok->len);
	cnode->cn_tok.ptr[tok->len] = '\0';

	return cnode;
}

hak_cnode_t* hak_makecnodenil (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_NIL, flags, loc, tok);
}

hak_cnode_t* hak_makecnodetrue (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_TRUE, flags, loc, tok);
}

hak_cnode_t* hak_makecnodefalse (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_FALSE, flags, loc, tok);
}

hak_cnode_t* hak_makecnodeself (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_SELF, flags, loc, tok);
}

hak_cnode_t* hak_makecnodesuper (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_SUPER, flags, loc, tok);
}

hak_cnode_t* hak_makecnodeellipsis (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_ELLIPSIS, flags, loc, tok);
}

hak_cnode_t* hak_makecnodetrpcolons (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_TRPCOLONS, flags, loc, tok);
}

hak_cnode_t* hak_makecnodedblcolons (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_DBLCOLONS, flags, loc, tok);
}

hak_cnode_t* hak_makecnodecolon (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_COLON, flags, loc, tok);
}

hak_cnode_t* hak_makecnodecolongt (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_COLONGT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodecolonlt (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_COLONLT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodecharlit (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok, hak_ooch_t v)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_CHARLIT, flags, loc, tok);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.charlit.v = v;
	return c;
}

hak_cnode_t* hak_makecnodebchrlit (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok, hak_oob_t v)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_BCHRLIT, flags, loc, tok);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.bchrlit.v = v;
	return c;
}

hak_cnode_t* hak_makecnodesymbol (hak_t* hak, int flags, const hak_loc_t* loc, const  hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_SYMBOL, flags, loc, tok);
}

hak_cnode_t* hak_makecnodedsymbol (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, int is_cla)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_DSYMBOL, flags, loc, tok);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.dsymbol.is_cla = is_cla;
	return c;
}

hak_cnode_t* hak_makecnodestrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_STRLIT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodebstrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_BSTRLIT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodesymlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_SYMLIT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodenumlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_NUMLIT, flags, loc, tok);
}

hak_cnode_t* hak_makecnoderadnumlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_RADNUMLIT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodefpdeclit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok)
{
	return hak_makecnode(hak, HAK_CNODE_FPDECLIT, flags, loc, tok);
}

hak_cnode_t* hak_makecnodesmptrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_oow_t v)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_SMPTRLIT, flags,  loc, tok);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.smptrlit.v = v;
	return c;
}

hak_cnode_t* hak_makecnodeerrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_ooi_t v)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_ERRLIT, flags, loc, tok);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.errlit.v = v;
	return c;
}

hak_cnode_t* hak_makecnodecons (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_cnode_t* car, hak_cnode_t* cdr)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_CONS, flags, loc, tok);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.cons.car = car;
	c->u.cons.cdr = cdr;
	return c;
}

hak_cnode_t* hak_makecnodeelist (hak_t* hak, int flags, const hak_loc_t* loc, hak_concode_t type)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_ELIST, flags, loc, HAK_NULL);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.elist.concode = type;
	return c;
}

hak_cnode_t* hak_makecnodeshell (hak_t* hak, int flags, const hak_loc_t* loc, hak_cnode_t* obj)
{
	hak_cnode_t* c = hak_makecnode(hak, HAK_CNODE_SHELL, flags, loc, HAK_NULL);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;
	c->u.shell.obj = obj;
	return c;
}

void hak_freesinglecnode (hak_t* hak, hak_cnode_t* c)
{
	hak_freemem(hak, c);
}

void hak_freecnode (hak_t* hak, hak_cnode_t* c)
{
redo:
	switch (c->cn_type)
	{
		case HAK_CNODE_CONS:
		{
			hak_cnode_t* tmp1, * tmp2;

			tmp1 = c->u.cons.car;
			tmp2 = c->u.cons.cdr;

			HAK_ASSERT(hak, tmp1 != HAK_NULL);
			hak_freemem(hak, c);
			hak_freecnode(hak, tmp1); /* TODO: remove recursion? */
			if (tmp2)
			{
				c = tmp2;
				goto redo;
			}

			break;
		}

		case HAK_CNODE_SHELL:
		{
			hak_cnode_t* tmp;

			tmp = c->u.shell.obj;
			hak_freemem(hak, c);
			if (tmp)
			{
				c = tmp;
				goto redo;
			}

			break;
		}

		default:
			hak_freemem(hak, c);
			break;
	}
}

hak_oow_t hak_countcnodecons (hak_t* hak, hak_cnode_t* cons)
{
	/* this function ignores the last cdr */
	hak_oow_t count = 1;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(cons));
	do
	{
		cons = HAK_CNODE_CONS_CDR(cons);
		if (!cons) break;
		count++;
	}
	while (1);

	return count;
}
