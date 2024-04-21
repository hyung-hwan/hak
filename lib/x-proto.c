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

#include <hcl-x.h>
#include "hcl-prv.h"

enum hcl_xproto_rcv_state_t
{
	HCL_XPROTO_RCV_HDR,
	HCL_XPROTO_RCV_PLD
};
typedef enum hcl_xproto_rcv_state_t hcl_xproto_rcv_state_t;

struct hcl_xproto_t
{
	hcl_oow_t   _instsize;
	hcl_mmgr_t* _mmgr;
	hcl_xproto_cb_t _cb;

	struct
	{
		hcl_xproto_rcv_state_t state;
		hcl_oow_t len_needed;
		unsigned int eof: 1;

		hcl_oow_t len;
		hcl_uint8_t buf[HCL_XPKT_MAX_PLD_LEN];

		/* normalize header of hcl_xpkt_hdr_t with combined bits into separate placeholders */
		struct
		{
			hcl_uint8_t id;
			hcl_uint8_t type;
			hcl_uint16_t len; /* this is wider than the len field of hcl_xpkt_hdr_t */
		} hdr;
	} rcv;

	struct
	{

	} snd;
};

hcl_xproto_t* hcl_xproto_open (hcl_mmgr_t* mmgr, hcl_xproto_cb_t* cb, hcl_oow_t xtnsize)
{
	hcl_xproto_t* proto;

	proto = (hcl_xproto_t*)HCL_MMGR_ALLOC(mmgr, HCL_SIZEOF(*proto) + xtnsize);
	if (HCL_UNLIKELY(!proto)) return HCL_NULL;

	HCL_MEMSET (proto, 0, HCL_SIZEOF(*proto));
	proto->_instsize = HCL_SIZEOF(*proto);
	proto->_mmgr = mmgr;
	proto->_cb = *cb;
	proto->rcv.state = HCL_XPROTO_RCV_HDR;
	proto->rcv.len_needed = HCL_XPKT_HDR_LEN;
	proto->rcv.eof = 0;

	return proto;
}

void hcl_xproto_close (hcl_xproto_t* proto)
{
	HCL_MMGR_FREE (proto->_mmgr, proto);
}

void* hcl_xproto_getxtn (hcl_xproto_t* proto)
{
	return (proto + 1);
}

hcl_uint8_t* hcl_xproto_getbuf (hcl_xproto_t* proto, hcl_oow_t* capa)
{
	*capa = HCL_COUNTOF(proto->rcv.buf) - proto->rcv.len;
	return &proto->rcv.buf[proto->rcv.len];
}

int hcl_xproto_geteof (hcl_xproto_t* proto)
{
	return proto->rcv.eof;
}

void hcl_xproto_seteof (hcl_xproto_t* proto, int v)
{
	proto->rcv.eof = v;
}

void hcl_xproto_advbuf (hcl_xproto_t* proto, hcl_oow_t inc)
{
	proto->rcv.len += inc;
}

int hcl_xproto_ready (hcl_xproto_t* proto)
{
	/* has it received suffient data for processing? */
	return proto->rcv.len >= proto->rcv.len_needed;
}

int hcl_xproto_process (hcl_xproto_t* proto)
{
	int n;
	hcl_xpkt_hdr_t* hdr;

	switch (proto->rcv.state)
	{
		case HCL_XPROTO_RCV_HDR:
			if (proto->rcv.len < HCL_XPKT_HDR_LEN) goto carry_on; /* need more data */

			hdr = (hcl_xpkt_hdr_t*)proto->rcv.buf;
			proto->rcv.hdr.id = hdr->id;
			proto->rcv.hdr.type = hdr->type & 0x0F;
			proto->rcv.hdr.len = (hcl_uint16_t)hdr->len  | ((hcl_uint16_t)(hdr->type >> 4) << 8);

			/* consume the header */
			HCL_MEMMOVE (proto->rcv.buf, &proto->rcv.buf[HCL_XPKT_HDR_LEN], proto->rcv.len - HCL_XPKT_HDR_LEN);
			proto->rcv.len -= HCL_XPKT_HDR_LEN;

			/* switch to the payload mode */
			if (proto->rcv.hdr.len > 0)
			{
				proto->rcv.state = HCL_XPROTO_RCV_PLD;
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

		case HCL_XPROTO_RCV_PLD:
			if (proto->rcv.len < proto->rcv.hdr.len) goto carry_on; /* need more payload data */

/* TODO: convert handle_packet as call back */
			n = proto->_cb.on_packet(proto, proto->rcv.hdr.type, proto->rcv.buf, proto->rcv.hdr.len);

/* TODO: minimize the use of HCL_MEMOVE... use the buffer */
			/* switch to the header mode */
			if (proto->rcv.hdr.len > 0)
			{
				HCL_MEMMOVE (proto->rcv.buf, &proto->rcv.buf[proto->rcv.hdr.len], proto->rcv.len - proto->rcv.hdr.len);
				proto->rcv.len -= proto->rcv.hdr.len;
			}
			proto->rcv.state = HCL_XPROTO_RCV_HDR;
			proto->rcv.len_needed = HCL_XPKT_HDR_LEN;

			if (n <= -1) goto fail_with_errmsg;
			if (n == 0) return 0;

			break;

		default:
/*
			hcl_seterrbfmt (hcl, HCL_EINTERN, "invalid request state %d", (int)proto->rcv.state);
*/
/* TODO: call back */
			goto fail_with_errmsg;
	}

carry_on:
	return 1;

fail_with_errmsg:
// TODO: proper error handling
	//send_proto_hcl_error (proto);
	//HCL_LOG1 (hcl, SERVER_LOGMASK_ERROR, "Unable to compile .SCRIPT contents - %js\n", hcl_geterrmsg(worker->hcl));
	return -1;

}

