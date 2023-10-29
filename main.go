package main

/*
#cgo CFLAGS: -I/home/hyung-hwan/xxx/include -g -Wall
#cgo LDFLAGS: -L/home/hyung-hwan/xxx/lib -lhcl -ldl -lquadmath

#include <hcl.h>
#include <stdlib.h>

extern int go_read_handler (hcl_t* hcl, hcl_iocmd_t cmd, void* arg);
extern int go_scan_handler (hcl_t* hcl, hcl_iocmd_t cmd, void* arg);
extern int go_print_handler (hcl_t* hcl, hcl_iocmd_t cmd, void* arg);

int read_handler (hcl_t* hcl, hcl_iocmd_t cmd, void* arg)
{
	return go_read_handler(hcl, cmd, arg);
}

int scan_handler (hcl_t* hcl, hcl_iocmd_t cmd, void* arg)
{
	return go_scan_handler(hcl, cmd, arg);
}

int print_handler (hcl_t* hcl, hcl_iocmd_t cmd, void* arg)
{
	return go_print_handler(hcl, cmd, arg);
}
*/
//import "C"

import (
	"fmt"
	hcl "code.miflux.com/hyung-hwan/hcl/go"
	"os"
	_ "cfg"
)

/*
	func run_hcl() {
		var hcl *C.hcl_t = C.hcl_openstd(0, nil)
		var src []uint16 = utf16.Encode(([]rune)(
			`(printf ">>>>>>>>> [%d]\n" (+ 30 455))

(printf ">>>>>>>>> [%d]\n" (+ 11 455))
#include "a.hcl"
(printf ">>>>>>>>> [%d]\n" (+ 20 455))
`))

		if hcl == nil {
			log.Printf("Unable to open HCL\n")
		} else {
			var (
				x       C.int
				logmask C.hcl_bitmask_t
				tgt     *C.char
				empty   *C.char
				r       C.hcl_oop_t
			)

			logmask = ^C.hcl_bitmask_t(0)
			tgt = C.CString("/dev/stderr")
			defer C.free(unsafe.Pointer(tgt))

			empty = C.CString("")
			defer C.free(unsafe.Pointer(empty))

			C.hcl_setoption(hcl, C.HCL_LOG_MASK, unsafe.Pointer(&logmask))
			C.hcl_setoption(hcl, C.HCL_LOG_TARGET_BCSTR, unsafe.Pointer(tgt))

			_ = C.hcl_ignite(hcl, 1000000)
			//fmt.Printf ("ignire %d\n", x)
			_ = C.hcl_addbuiltinprims(hcl)
			//fmt.Printf ("addbuiltinprims %d\n", x)
			//_ = C.hcl_attachiostdwithbcstr(hcl, empty, empty, empty)
			//x = C.hcl_attachio(hcl, (*[0]byte)(C.read_handler), (*[0]byte)(C.scan_handler), (*[0]byte)(C.print_handler))
			x = C.hcl_attachio(hcl, C.hcl_ioimpl_t(C.read_handler), C.hcl_ioimpl_t(C.scan_handler), C.hcl_ioimpl_t(C.print_handler))
			if x <= -1 {
				log.Printf("unable to attach IO handlers -  %d - %s\n", x, C.GoString(C.hcl_geterrbmsg(hcl)))
				goto done
			}
			_ = C.hcl_beginfeed(hcl, nil)
			//fmt.Printf ("beginfeed %d\n", x)
			_ = C.hcl_feed(hcl, (*C.ushort)(&src[0]), (C.ulong)(len(src)))
			//fmt.Printf ("feed %d\n", x)
			_ = C.hcl_endfeed(hcl)
			//fmt.Printf ("endfeed %d => bclen %d\n", x, C.hcl_getbclen(hcl))
			_ = x

			C.hcl_decode(hcl, 0, C.hcl_getbclen(hcl))
			logmask = 0
			C.hcl_setoption(hcl, C.HCL_LOG_MASK, unsafe.Pointer(&logmask))
			r = C.hcl_execute(hcl)
			if r == nil {
				fmt.Printf("Error - %s\n", C.GoString(C.hcl_geterrbmsg(hcl)))
			}

		done:
			C.hcl_close(hcl)
		}
	}
*/
func main() {
	//run_hcl()

	var x *hcl.HCL = nil
	var err error = nil

	/*
		var rfh hcl.ReadFileHandler
		var sfh hcl.ScanFileHandler
		var pfh hcl.PrintFileHandler
	*/

	x, err = hcl.New()
	if err != nil {
		fmt.Printf("ERROR failed to instantiate hcl - %s\n", err.Error())
		os.Exit(1)
	}
	err = x.Ignite(1000000)
	if err != nil {
		fmt.Printf("ERROR failed to ignite - %s\n", err.Error())
		goto oops
	}
	err = x.AddBuiltinPrims()
	if err != nil {
		fmt.Printf("ERROR failed to add builtin primitives - %s\n", err.Error())
		goto oops
	}

	/*
	   	err = x.AttachIO(&rfh, &sfh, &pfh)
	   	if err != nil {
	   		fmt.Printf("Error - %s", err.Error())
	   	}
	   	x.FeedString(`(printf ">>>>>>>>> [%d]\n" (+ 30 455))

	   (printf ">>>>>>>>> [%d]\n" (+ 11 455))
	   #include "a.hcl"
	   (printf ">>>>>>>>> [%d]\n" (+ 20 455))`)

	   	x.EndFeed()
	   	x.Execute()

	*/
	x.Close()

	os.Exit(0)

oops:
	if x != nil {
		x.Close()
		x = nil
	}
	os.Exit(1)
}
