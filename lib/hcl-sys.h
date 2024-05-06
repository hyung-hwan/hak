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

#ifndef _HCL_SYS_T_
#define _HCL_SYS_T_

#include <hcl.h>

/* forward declaration to skip including <sys/uio.h> just for struct iovec */
typedef struct iovec hcl_iovec_t;

#if defined(__cplusplus)
extern "C" {
#endif

HCL_EXPORT int hcl_sys_send_iov (
	int          sck,
	hcl_iovec_t* iov, /* note this is not read-only and can change */
	int          count
);

HCL_EXPORT int hcl_sys_open_pipes (
	int          pfd[2],
	int          nonblock
);

HCL_EXPORT void hcl_sys_close_pipes (
	int          pfd[2]
);

HCL_EXPORT int hcl_sys_set_nonblock (
	int          fd,
	int          v
);

HCL_EXPORT int hcl_sys_set_cloexec (
	int          fd,
	int          v
);

HCL_EXPORT int hcl_sys_is_errno_wb (
	int          no
);

#if defined(__cplusplus)
}
#endif

#endif
