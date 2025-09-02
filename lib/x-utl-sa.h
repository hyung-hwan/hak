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

static int str_to_ipv4 (const ooch_t* str, hak_oow_t len, struct in_addr* inaddr)
{
	const ooch_t* end;
	int dots = 0, digits = 0;
	hak_uint32_t acc = 0, addr = 0;
	ooch_t c;

	end = str + len;

	do
	{
		if (str >= end)
		{
			if (dots < 3 || digits == 0) return -1;
			addr = (addr << 8) | acc;
			break;
		}

		c = *str++;

		if (c >= '0' && c <= '9')
		{
			if (digits > 0 && acc == 0) return -1;
			acc = acc * 10 + (c - '0');
			if (acc > 255) return -1;
			digits++;
		}
		else if (c == '.')
		{
			if (dots >= 3 || digits == 0) return -1;
			addr = (addr << 8) | acc;
			dots++; acc = 0; digits = 0;
		}
		else return -1;
	}
	while (1);

	inaddr->s_addr = hak_hton32(addr);
	return 0;

}

#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN6 > 0)
static int str_to_ipv6 (const ooch_t* src, hak_oow_t len, struct in6_addr* inaddr)
{
	hak_uint8_t* tp, * endp, * colonp;
	const ooch_t* curtok;
	ooch_t ch;
	int saw_xdigit;
	unsigned int val;
	const ooch_t* src_end;

	src_end = src + len;

	HAK_MEMSET (inaddr, 0, HAK_SIZEOF(*inaddr));
	tp = &inaddr->s6_addr[0];
	endp = &inaddr->s6_addr[HAK_COUNTOF(inaddr->s6_addr)];
	colonp = HAK_NULL;

	/* Leading :: requires some special handling. */
	if (src < src_end && *src == ':')
	{
		src++;
		if (src >= src_end || *src != ':') return -1;
	}

	curtok = src;
	saw_xdigit = 0;
	val = 0;

	while (src < src_end)
	{
		int v1;

		ch = *src++;

		if (ch >= '0' && ch <= '9')
			v1 = ch - '0';
		else if (ch >= 'A' && ch <= 'F')
			v1 = ch - 'A' + 10;
		else if (ch >= 'a' && ch <= 'f')
			v1 = ch - 'a' + 10;
		else v1 = -1;
		if (v1 >= 0)
		{
			val <<= 4;
			val |= v1;
			if (val > 0xffff) return -1;
			saw_xdigit = 1;
			continue;
		}

		if (ch == ':')
		{
			curtok = src;
			if (!saw_xdigit)
			{
				if (colonp) return -1;
				colonp = tp;
				continue;
			}
			else if (src >= src_end)
			{
				/* a colon can't be the last character */
				return -1;
			}

			*tp++ = (hak_uint8_t)(val >> 8) & 0xff;
			*tp++ = (hak_uint8_t)val & 0xff;
			saw_xdigit = 0;
			val = 0;
			continue;
		}

		if (ch == '.' && ((tp + HAK_SIZEOF(struct in_addr)) <= endp) &&
		    str_to_ipv4(curtok, src_end - curtok, (struct in_addr*)tp) == 0)
		{
			tp += HAK_SIZEOF(struct in_addr*);
			saw_xdigit = 0;
			break;
		}

		return -1;
	}

	if (saw_xdigit)
	{
		if (tp + HAK_SIZEOF(hak_uint16_t) > endp) return -1;
		*tp++ = (hak_uint8_t)(val >> 8) & 0xff;
		*tp++ = (hak_uint8_t)val & 0xff;
	}
	if (colonp != HAK_NULL)
	{
		/*
		 * Since some memmove()'s erroneously fail to handle
		 * overlapping regions, we'll do the shift by hand.
		 */
		hak_oow_t n = tp - colonp;
		hak_oow_t i;

		for (i = 1; i <= n; i++)
		{
			endp[-i] = colonp[n - i];
			colonp[n - i] = 0;
		}
		tp = endp;
	}

	if (tp != endp) return -1;

	return 0;
}
#endif

static int str_to_ifindex (hak_t* hak, const ooch_t* ptr, hak_oow_t len, unsigned int* ifindex)
{
#if defined(SIOCGIFINDEX)
	int h, x;
	struct ifreq ifr;

	/* use AF_INET6 because str_to_ifindex is called for ipv6 only in this file */
	h = socket(AF_INET6, SOCK_DGRAM, 0);
	if (h <= -1)
	{
		hak_seterrbfmtwithsyserr (hak, 0, errno, "unable to open socket for if_nametoindex conversion");
		return -1;
	}

	HAK_MEMSET (&ifr, 0, HAK_SIZEOF(ifr));

#if (ooch_mode == 2)
	hak_oow_t ucslen, bcslen;
	ucslen = len;
	bcslen = HAK_COUNTOF(ifr.ifr_name) - 1;
	if (hak_convutobchars(hak, ptr, &ucslen, ifr.ifr_name, &bcslen) <= -1)
	{
		close (h);
		return -1;
	}
	ifr.ifr_name[bcslen] = '\0';
#else
	if (hak_copy_bchars_to_bcstr(ifr.ifr_name, HAK_COUNTOF(ifr.ifr_name), ptr, len) < len)
	{
		close (h);
		return -1;
	}
#endif

	x = ioctl(h, SIOCGIFINDEX, &ifr);
	close (h);

	if (x >= 0)
	{
	#if defined(HAVE_STRUCT_IFREQ_IFR_IFINDEX)
		*ifindex = ifr.ifr_ifindex;
	#else
		*ifindex = ifr.ifr_index;
	#endif
	}

	return x;
#else
/* TODO: use if_nametoindex()? */
	hak_seterrbfmt (hak, HAK_ENOIMPL, "ifname to ifindex conversion not implemented");
	return -1;
#endif
}

int str_to_sockaddr (hak_t* hak, const ooch_t* str, hak_oow_t len, hak_sckaddr_t* sckaddr, hak_scklen_t* scklen)
{
	const ooch_t* p;
	const ooch_t* end;
	oocs_t tmp;
	sockaddr_t* nwad = (sockaddr_t*)sckaddr;

	p = str;
	end = str + len;

	if (p >= end)
	{
		if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "blank address");
		return -1;
	}

	HAK_MEMSET (nwad, 0, HAK_SIZEOF(*nwad));

#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN6 > 0)
	if (*p == '[')
	{
		/* IPv6 address */
		tmp.ptr = (ooch_t*)++p; /* skip [ and remember the position */
		while (p < end && *p != '%' && *p != ']') p++;

		if (p >= end) goto no_rbrack;

		tmp.len = p - tmp.ptr;
		if (*p == '%')
		{
			/* handle scope id */
			hak_uint32_t x, y;

			p++; /* skip % */

			if (p >= end)
			{
				/* premature end */
				if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "scope id blank");
				return -1;
			}

			if (*p >= '0' && *p <= '9')
			{
				/* numeric scope id */
				y = 0;
				do
				{
					x = y * 10 + (*p - '0');
					if (x < y)
					{
						if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "scope id too large");
						return -1; /* overflow */
					}
					y = x;
					p++;
				}
				while (p < end && *p >= '0' && *p <= '9');
				nwad->in6.sin6_scope_id = y;
			}
			else
			{
				/* interface name as a scope id? */
				const ooch_t* stmp = p;
				unsigned int index;
				do p++; while (p < end && *p != ']');
				if (str_to_ifindex(hak, stmp, p - stmp, &index) <= -1) return -1;
				nwad->in6.sin6_scope_id = index;
			}

			if (p >= end || *p != ']') goto no_rbrack;
		}
		p++; /* skip ] */

		if (str_to_ipv6(tmp.ptr, tmp.len, &nwad->in6.sin6_addr) <= -1) goto unrecog;
		nwad->in6.sin6_family = AF_INET6;
	}
	else
	{
#endif
		/* IPv4 address */
		tmp.ptr = (ooch_t*)p;
		while (p < end && *p != ':') p++;
		tmp.len = p - tmp.ptr;

		if (str_to_ipv4(tmp.ptr, tmp.len, &nwad->in4.sin_addr) <= -1)
		{
		#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN6 > 0)
			/* check if it is an IPv6 address not enclosed in [].
			 * the port number can't be specified in this format. */
			if (p >= end || *p != ':')
			{
				/* without :, it can't be an ipv6 address */
				goto unrecog;
			}

			while (p < end && *p != '%') p++;
			tmp.len = p - tmp.ptr;

			if (str_to_ipv6(tmp.ptr, tmp.len, &nwad->in6.sin6_addr) <= -1) goto unrecog;

			if (p < end && *p == '%')
			{
				/* handle scope id */
				hak_uint32_t x, y;

				p++; /* skip % */

				if (p >= end)
				{
					/* premature end */
					if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "scope id blank");
					return -1;
				}

				if (*p >= '0' && *p <= '9')
				{
					/* numeric scope id */
					y = 0;
					do
					{
						x = y * 10 + (*p - '0');
						if (x < y)
						{
							if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "scope id too large");
							return -1; /* overflow */
						}
						y = x;
						p++;
					}
					while (p < end && *p >= '0' && *p <= '9');
					nwad->in6.sin6_scope_id = y;
				}
				else
				{
					/* interface name as a scope id? */
					const ooch_t* stmp = p;
					unsigned int index;
					do p++; while (p < end);
					if (str_to_ifindex(hak, stmp, p - stmp, &index) <= -1) return -1;
					nwad->in6.sin6_scope_id = index;
				}
			}

			if (p < end) goto unrecog; /* some gargage after the end? */

			nwad->in6.sin6_family = AF_INET6;
			*scklen = HAK_SIZEOF(nwad->in6);
			return nwad->in6.sin6_family;
		#else
			goto unrecog;
		#endif
		}

		nwad->in4.sin_family = AF_INET;
#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN6 > 0)
	}
#endif

	if (p < end && *p == ':')
	{
		/* port number */
		hak_uint32_t port = 0;

		p++; /* skip : */

		tmp.ptr = (ooch_t*)p;
		while (p < end && *p >= '0' && *p <= '9')
		{
			port = port * 10 + (*p - '0');
			p++;
		}

		tmp.len = p - tmp.ptr;
		if (tmp.len <= 0 || tmp.len >= 6 ||
		    port > HAK_TYPE_MAX(hak_uint16_t))
		{
			if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "port number blank or too large");
			return -1;
		}

	#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN6 > 0)
		if (nwad->in4.sin_family == AF_INET)
			nwad->in4.sin_port = hak_hton16(port);
		else
			nwad->in6.sin6_port = hak_hton16(port);
	#else
		nwad->in4.sin_port = hak_hton16(port);
	#endif
	}

	*scklen = (nwad->in4.sin_family == AF_INET)? HAK_SIZEOF(nwad->in4): HAK_SIZEOF(nwad->in6);
	return nwad->in4.sin_family;

unrecog:
	if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "unrecognized address");
	return -1;

no_rbrack:
	if (hak) hak_seterrbfmt (hak, HAK_EINVAL, "missing right bracket");
	return -1;
}
