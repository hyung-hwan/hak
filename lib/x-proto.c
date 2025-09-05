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

#include <hak-x.h>
#include "hak-prv.h"

enum hak_xproto_rcv_state_t
{
	HAK_XPROTO_RCV_HDR,
	HAK_XPROTO_RCV_PLD
};
typedef enum hak_xproto_rcv_state_t hak_xproto_rcv_state_t;

struct hak_xproto_t
{
	hak_oow_t   _instsize;
	hak_mmgr_t* _mmgr;
	hak_xproto_cb_t _cb;

	struct
	{
		hak_xproto_rcv_state_t state;
		hak_oow_t len_needed;
		unsigned int eof: 1;

		hak_oow_t len;
		hak_uint8_t buf[HAK_XPKT_MAX_PLD_LEN];

		/* normalize header of hak_xpkt_hdr_t with combined bits into separate placeholders */
		struct
		{
			hak_uint8_t id;
			hak_uint8_t type;
			hak_uint16_t len; /* this is wider than the len field of hak_xpkt_hdr_t */
		} hdr;
	} rcv;

	struct
	{

	} snd;
};

hak_xproto_t* hak_xproto_open (hak_mmgr_t* mmgr, hak_xproto_cb_t* cb, hak_oow_t xtnsize)
{
	hak_xproto_t* proto;

	proto = (hak_xproto_t*)HAK_MMGR_ALLOC(mmgr, HAK_SIZEOF(*proto) + xtnsize);
	if (HAK_UNLIKELY(!proto)) return HAK_NULL;

	HAK_MEMSET(proto, 0, HAK_SIZEOF(*proto));
	proto->_instsize = HAK_SIZEOF(*proto);
	proto->_mmgr = mmgr;
	proto->_cb = *cb;
	proto->rcv.state = HAK_XPROTO_RCV_HDR;
	proto->rcv.len_needed = HAK_XPKT_HDR_LEN;
	proto->rcv.eof = 0;

	return proto;
}

void hak_xproto_close (hak_xproto_t* proto)
{
	HAK_MMGR_FREE (proto->_mmgr, proto);
}

void* hak_xproto_getxtn (hak_xproto_t* proto)
{
	return (proto + 1);
}

hak_uint8_t* hak_xproto_getbuf (hak_xproto_t* proto, hak_oow_t* capa)
{
	*capa = HAK_COUNTOF(proto->rcv.buf) - proto->rcv.len;
	return &proto->rcv.buf[proto->rcv.len];
}

int hak_xproto_geteof (hak_xproto_t* proto)
{
	return proto->rcv.eof;
}

void hak_xproto_seteof (hak_xproto_t* proto, int v)
{
	proto->rcv.eof = v;
}

void hak_xproto_advbuf (hak_xproto_t* proto, hak_oow_t inc)
{
	proto->rcv.len += inc;
}

int hak_xproto_ready (hak_xproto_t* proto)
{
	/* has it received suffient data for processing? */
	return proto->rcv.len >= proto->rcv.len_needed;
}

int hak_xproto_process (hak_xproto_t* proto)
{
	int n;
	hak_xpkt_hdr_t* hdr;

	switch (proto->rcv.state)
	{
		case HAK_XPROTO_RCV_HDR:
			if (proto->rcv.len < HAK_XPKT_HDR_LEN) goto carry_on; /* need more data */

			hdr = (hak_xpkt_hdr_t*)proto->rcv.buf;
			proto->rcv.hdr.id = hdr->id;
			proto->rcv.hdr.type = hdr->type & 0x0F;
			proto->rcv.hdr.len = (hak_uint16_t)hdr->len  | ((hak_uint16_t)(hdr->type >> 4) << 8);

			/* consume the header */
			HAK_MEMMOVE(proto->rcv.buf, &proto->rcv.buf[HAK_XPKT_HDR_LEN], proto->rcv.len - HAK_XPKT_HDR_LEN);
			proto->rcv.len -= HAK_XPKT_HDR_LEN;

			/* switch to the payload mode */
			if (proto->rcv.hdr.len > 0)
			{
				proto->rcv.state = HAK_XPROTO_RCV_PLD;
				proto->rcv.len_needed = proto->rcv.hdr.len;
			}
			else
			{
				/* take shortcut */
/* TODO: convert handle_packet as call back */
				n = proto->_cb.on_packet(proto, proto->rcv.hdr.type, proto->rcv.buf, proto->rcv.hdr.len);
				if (n <= -1) goto fail_with_errmsg;
				if (n == 0) return 0;
			}

			break;

		case HAK_XPROTO_RCV_PLD:
			if (proto->rcv.len < proto->rcv.hdr.len) goto carry_on; /* need more payload data */

			n = proto->_cb.on_packet(proto, proto->rcv.hdr.type, proto->rcv.buf, proto->rcv.hdr.len);

			/* switch to the header mode */
			if (proto->rcv.hdr.len > 0)
			{
/* TODO: minimize the use of HAK_MEMOVE... use the buffer */
				HAK_MEMMOVE(proto->rcv.buf, &proto->rcv.buf[proto->rcv.hdr.len], proto->rcv.len - proto->rcv.hdr.len);
				proto->rcv.len -= proto->rcv.hdr.len;
			}
			proto->rcv.state = HAK_XPROTO_RCV_HDR;
			proto->rcv.len_needed = HAK_XPKT_HDR_LEN;

			if (n <= -1) goto fail_with_errmsg;
			if (n == 0) return 0;

			break;

		default:
/*
			hak_seterrbfmt(hak, HAK_EINTERN, "invalid request state %d", (int)proto->rcv.state);
*/
/* TODO: call back */
			goto fail_with_errmsg;
	}

carry_on:
	return 1;

fail_with_errmsg:
// TODO: proper error handling
	//send_proto_hak_error (proto);
	//HAK_LOG1 (hak, SERVER_LOGMASK_ERROR, "Unable to compile .SCRIPT contents - %js\n", hak_geterrmsg(worker->hak));
	return -1;

}

