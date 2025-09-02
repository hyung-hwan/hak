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

void hak_dumpsymtab (hak_t* hak)
{
	hak_oow_t i;
	hak_oop_char_t symbol;

	HAK_DEBUG0 (hak, "--------------------------------------------\n");
	HAK_DEBUG1 (hak, "HAK Symbol Table %zu\n", HAK_OBJ_GET_SIZE(hak->symtab->bucket));
	HAK_DEBUG0 (hak, "--------------------------------------------\n");

	for (i = 0; i < HAK_OBJ_GET_SIZE(hak->symtab->bucket); i++)
	{
		symbol = (hak_oop_char_t)hak->symtab->bucket->slot[i];
 		if ((hak_oop_t)symbol != hak->_nil)
		{
			HAK_DEBUG2 (hak, " %07zu %O\n", i, symbol);
		}
	}

	HAK_DEBUG0 (hak, "--------------------------------------------\n");
}

void hak_dumpdic (hak_t* hak, hak_oop_dic_t dic, const hak_bch_t* title)
{
	hak_oow_t i;
	hak_oop_cons_t ass;

	HAK_DEBUG0 (hak, "--------------------------------------------\n");
	HAK_DEBUG2 (hak, "%s %zu\n", title, HAK_OBJ_GET_SIZE(dic->bucket));
	HAK_DEBUG0 (hak, "--------------------------------------------\n");

	for (i = 0; i < HAK_OBJ_GET_SIZE(dic->bucket); i++)
	{
		ass = (hak_oop_cons_t)dic->bucket->slot[i];
		if ((hak_oop_t)ass != hak->_nil)
		{
			HAK_DEBUG2 (hak, " %07zu %O\n", i, ass->car);
		}
	}
	HAK_DEBUG0 (hak, "--------------------------------------------\n");
}




