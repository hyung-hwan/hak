package hcl

/*
#include <hcl.h>
#include <hcl-utl.h>
#include <stdlib.h> // for C.free

extern int hcl_go_cci_handler (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg);
extern int hcl_go_udi_handler (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg);
extern int hcl_go_udo_handler (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg);

int hcl_cci_handler_for_go (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg) {
    return hcl_go_cci_handler(hcl, cmd, arg);
}
int hcl_udi_handler_for_go (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg) {
    return hcl_go_udi_handler(hcl, cmd, arg);
}
int hcl_udo_handler_for_go (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg) {
    return hcl_go_udo_handler(hcl, cmd, arg);
}
*/
import "C"

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"runtime"
	"unsafe"
)

type CciImpl interface {
	Open(g *HCL, name string) (int, error)
	Close(fd int)
	Read(fd int, buf []rune) (int, error)
}

type UdiImpl interface {
	Open(g *HCL) error
	Close()
	Read(buf []rune) (int, error)
}

type UdoImpl interface {
	Open(g *HCL) error
	Close()
	Write(data []rune) error
	WriteBytes(data []byte) error
	Flush() error
}

type HCL struct {
	c       *C.hcl_t
	inst_no int
	io      struct {
		cci      CciImpl
		cci_main string
		udi      UdiImpl
		udo      UdoImpl
	}
}

type Ext struct {
	inst_no int
}

type BitMask C.hcl_bitmask_t

const TRAIT_LANG_ENABLE_BLOCK BitMask = C.HCL_TRAIT_LANG_ENABLE_BLOCK
const TRAIT_LANG_ENABLE_EOL BitMask = C.HCL_TRAIT_LANG_ENABLE_EOL

var inst_table InstanceTable

func deregister_instance(g *HCL) {
	if g.inst_no >= 0 {
		inst_table.delete_instance(g.inst_no)
		g.inst_no = -1
	}
}

func New() (*HCL, error) {
	var c *C.hcl_t
	var g *HCL
	var ext *Ext
	var errnum C.hcl_errnum_t

	c = C.hcl_openstd(C.hcl_oow_t(unsafe.Sizeof(*ext)), &errnum)
	if c == nil {
		var buf [64]C.hcl_uch_t
		var ptr *C.hcl_uch_t
		var err error

		ptr = C.hcl_errnum_to_errucstr(errnum, &buf[0], C.hcl_oow_t(cap(buf)))
		err = fmt.Errorf("%s", string(ucstr_to_rune_slice(ptr)))
		return nil, err
	}

	ext = (*Ext)(unsafe.Pointer(C.hcl_getxtn(c)))

	g = &HCL{c: c, inst_no: -1}

	runtime.SetFinalizer(g, deregister_instance)
	g.inst_no = inst_table.add_instance(c, g)
	ext.inst_no = g.inst_no

	return g, nil
}

func (hcl *HCL) Close() {
	C.hcl_close(hcl.c)
	deregister_instance(hcl)
}

func (hcl *HCL) GetTrait() BitMask {
	var x C.int
	var log_mask BitMask = 0

	x = C.hcl_getoption(hcl.c, C.HCL_TRAIT, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to get log mask - %s", hcl.get_errmsg()))
	}

	return log_mask
}

func (hcl *HCL) SetTrait(log_mask BitMask) {
	var x C.int

	x = C.hcl_setoption(hcl.c, C.HCL_TRAIT, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log mask - %s", hcl.get_errmsg()))
	}
}

func (hcl *HCL) GetLogMask() BitMask {
	var x C.int
	var log_mask BitMask = 0

	x = C.hcl_getoption(hcl.c, C.HCL_LOG_MASK, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to get log mask - %s", hcl.get_errmsg()))
	}

	return log_mask
}

func (hcl *HCL) SetLogMask(log_mask BitMask) {
	var x C.int

	x = C.hcl_setoption(hcl.c, C.HCL_LOG_MASK, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log mask - %s", hcl.get_errmsg()))
	}
}

func (hcl *HCL) GetLogTarget() string {
	var x C.int
	var tgt *C.char

	x = C.hcl_getoption(hcl.c, C.HCL_LOG_TARGET_BCSTR, unsafe.Pointer(&tgt))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log target - %s", hcl.get_errmsg()))
	}

	return C.GoString(tgt)
}

func (hcl *HCL) SetLogTarget(target string) {
	var x C.int
	var tgt *C.char

	tgt = C.CString(target) // TODO: need error check?
	defer C.free(unsafe.Pointer(tgt))

	x = C.hcl_setoption(hcl.c, C.HCL_LOG_TARGET_BCSTR, unsafe.Pointer(tgt))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log target - %s", hcl.get_errmsg()))
	}
}

func (hcl *HCL) Ignite(memsize uintptr) error {
	var x C.int

	x = C.hcl_ignite(hcl.c, C.hcl_oow_t(memsize))
	if x <= -1 {
		return fmt.Errorf("unable to ignite - %s", hcl.get_errmsg())
	}

	return nil
}

func (hcl *HCL) AddBuiltinPrims() error {
	var x C.int

	x = C.hcl_addbuiltinprims(hcl.c)
	if x <= -1 {
		return fmt.Errorf("unable to add built-in primitives - %s", hcl.get_errmsg())
	}
	return nil
}

// the name of the main cci stream is required because:
// - the main stream is not handled by this IO handler
// - the feeder must read the main stream and pass data.
// - the inclusion of another file from the main stream requires the path information of the main strea.
func (hcl *HCL) AttachCCIO(cci CciImpl, main_cci_name string) error {
	var x C.int
	var old_cci CciImpl
	var old_cci_name string

	old_cci = hcl.io.cci
	old_cci_name = hcl.io.cci_main

	hcl.io.cci = cci
	hcl.io.cci_main = main_cci_name

	x = C.hcl_attachccio(hcl.c, C.hcl_io_impl_t(C.hcl_cci_handler_for_go))
	if x <= -1 {
		// restore the io handler set due to attachment failure
		hcl.io.cci_main = old_cci_name
		hcl.io.cci = old_cci
		return fmt.Errorf("unable to attach source input stream handler - %s", hcl.get_errmsg())
	}
	return nil
}

func (hcl *HCL) AttachUDIO(udi UdiImpl, udo UdoImpl) error {
	var x C.int
	var os UdiImpl
	var op UdoImpl

	os = hcl.io.udi
	op = hcl.io.udo

	hcl.io.udi = udi
	hcl.io.udo = udo

	x = C.hcl_attachudio(hcl.c,
		C.hcl_io_impl_t(C.hcl_udi_handler_for_go),
		C.hcl_io_impl_t(C.hcl_udo_handler_for_go))
	if x <= -1 {
		//restore the io handlers set due to attachment failure
		hcl.io.udi = os
		hcl.io.udo = op
		return fmt.Errorf("unable to attach user data stream handlers - %s", hcl.get_errmsg())
	}
	return nil
}

func (hcl *HCL) BeginFeed() error {
	var x C.int

	x = C.hcl_beginfeed(hcl.c, nil)
	if x <= -1 {
		return fmt.Errorf("unable to begin feeding - %s", hcl.get_errmsg())
	}

	return nil
}

func (hcl *HCL) EndFeed() error {
	var x C.int

	x = C.hcl_endfeed(hcl.c)
	if x <= -1 {
		return fmt.Errorf("unable to end feeding - %s", hcl.get_errmsg())
	}

	return nil
}

func (hcl *HCL) FeedString(str string) error {
	var x C.int
	var q []C.hcl_uch_t

	q = string_to_uchars(str)
	x = C.hcl_feed(hcl.c, &q[0], C.hcl_oow_t(len(q)))
	if x <= -1 {
		return fmt.Errorf("unable to feed string - %s", hcl.get_errmsg())
	}
	return nil
}

func (hcl *HCL) FeedRunes(str []rune) error {
	var x C.int
	var q []C.hcl_uch_t

	q = rune_slice_to_uchars(str)
	x = C.hcl_feed(hcl.c, &q[0], C.hcl_oow_t(len(q)))
	if x <= -1 {
		return fmt.Errorf("unable to feed runes - %s", hcl.get_errmsg())
	}
	return nil
}

func (hcl *HCL) FeedFromReader(rdr io.Reader) error {
	var err error
	var n int
	var x C.int
	var buf [1024]byte

	for {
		n, err = rdr.Read(buf[:])
		if err == io.EOF {
			break
		} else if err != nil {
			return fmt.Errorf("unable to read bytes - %s", err.Error())
		}

		x = C.hcl_feedbchars(hcl.c, (*C.hcl_bch_t)(unsafe.Pointer(&buf[0])), C.hcl_oow_t(n))
		if x <= -1 {
			return fmt.Errorf("unable to feed bytes - %s", hcl.get_errmsg())
		}
	}

	return nil
}

func (hcl *HCL) FeedFromFile(file string) error {
	var f *os.File
	var err error

	f, err = os.Open(file)
	if err != nil {
		return fmt.Errorf("unable to open %s - %s", file, err.Error())
	}

	defer f.Close()
	return hcl.FeedFromReader(bufio.NewReader(f))
}

func (hcl *HCL) Execute() error {
	var x C.hcl_oop_t

	x = C.hcl_execute(hcl.c)
	if x == nil {
		return fmt.Errorf("unable to execute - %s", hcl.get_errmsg())
	}

	// TODO: wrap C.hcl_oop_t in a go type
	//       and make this function to return 'x' in the wrapper
	return nil
}

func (hcl *HCL) Decode() error {
	var x C.int

	x = C.hcl_decode(hcl.c, C.hcl_getcode(hcl.c), 0, C.hcl_getbclen(hcl.c))
	if x <= -1 {
		return fmt.Errorf("unable to decode byte codes - %s", hcl.get_errmsg())
	}

	return nil
}

func (hcl *HCL) get_errmsg() string {
	return C.GoString(C.hcl_geterrbmsg(hcl.c))
}

func (hcl *HCL) set_errmsg(num C.hcl_errnum_t, msg string) {
	var ptr *C.char
	ptr = C.CString(msg)
	defer C.free(unsafe.Pointer(ptr))
	C.hcl_seterrbmsg(hcl.c, num, ptr)
}

func ucstr_to_rune_slice(str *C.hcl_uch_t) []rune {
	return uchars_to_rune_slice(str, uintptr(C.hcl_count_ucstr(str)))
}

func uchars_to_rune_slice(str *C.hcl_uch_t, len uintptr) []rune {
	var res []rune
	var i uintptr
	var ptr uintptr

	// TODO: proper encoding...
	ptr = uintptr(unsafe.Pointer(str))
	res = make([]rune, len)
	for i = 0; i < len; i++ {
		res[i] = rune(*(*C.hcl_uch_t)(unsafe.Pointer(ptr)))
		ptr += unsafe.Sizeof(*str)
	}
	return res
}

func string_to_uchars(str string) []C.hcl_uch_t {
	var r []rune
	var c []C.hcl_uch_t
	var i int

	// TODO: proper encoding
	r = []rune(str)
	c = make([]C.hcl_uch_t, len(r), len(r))
	for i = 0; i < len(r); i++ {
		c[i] = C.hcl_uch_t(r[i])
	}

	return c
}

func rune_slice_to_uchars(r []rune) []C.hcl_uch_t {
	var c []C.hcl_uch_t
	var i int

	// TODO: proper encoding
	c = make([]C.hcl_uch_t, len(r), len(r))
	for i = 0; i < len(r); i++ {
		c[i] = C.hcl_uch_t(r[i])
	}
	return c
}

func c_to_go(c *C.hcl_t) *HCL {
	var ext *Ext
	var inst Instance

	ext = (*Ext)(unsafe.Pointer(C.hcl_getxtn(c)))
	inst = inst_table.slot_to_instance(ext.inst_no)
	return inst.g
}
