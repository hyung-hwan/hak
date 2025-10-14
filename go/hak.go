package hak

/*
#include <hak.h>
#include <hak-str.h>
#include <stdlib.h> // for C.free

extern int hak_go_cci_handler (hak_t* hak, hak_io_cmd_t cmd, void* arg);
extern int hak_go_udi_handler (hak_t* hak, hak_io_cmd_t cmd, void* arg);
extern int hak_go_udo_handler (hak_t* hak, hak_io_cmd_t cmd, void* arg);

int hak_cci_handler_for_go (hak_t* hak, hak_io_cmd_t cmd, void* arg) {
    return hak_go_cci_handler(hak, cmd, arg);
}
int hak_udi_handler_for_go (hak_t* hak, hak_io_cmd_t cmd, void* arg) {
    return hak_go_udi_handler(hak, cmd, arg);
}
int hak_udo_handler_for_go (hak_t* hak, hak_io_cmd_t cmd, void* arg) {
    return hak_go_udo_handler(hak, cmd, arg);
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
	Open(g *Hak, name string) (int, error)
	Close(fd int)
	Read(fd int, buf []rune) (int, error)
}

type UdiImpl interface {
	Open(g *Hak) error
	Close()
	Read(buf []rune) (int, error)
}

type UdoImpl interface {
	Open(g *Hak) error
	Close()
	Write(data []rune) error
	WriteBytes(data []byte) error
	Flush() error
}

type Hak struct {
	c       *C.hak_t
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

type Err struct {
	Line uint
	Colm uint
	File string
	Msg string
}

type BitMask C.hak_bitmask_t

const TRAIT_LANG_ENABLE_EOL BitMask = C.HAK_TRAIT_LANG_ENABLE_EOL

var inst_table InstanceTable

func deregister_instance(g *Hak) {
	if g.inst_no >= 0 {
		inst_table.delete_instance(g.inst_no)
		g.inst_no = -1
	}
}

func New() (*Hak, error) {
	var c *C.hak_t
	var g *Hak
	var ext *Ext
	var errinf C.hak_errinf_t

	c = C.hak_openstd(C.hak_oow_t(unsafe.Sizeof(*ext)), &errinf)
	if c == nil {
		var err error
		err = fmt.Errorf("%s", string(ucstr_to_rune_slice(&errinf.msg[0])))
		return nil, err
	}

	ext = (*Ext)(unsafe.Pointer(C.hak_getxtn(c)))

	g = &Hak{c: c, inst_no: -1}

	runtime.SetFinalizer(g, deregister_instance)
	g.inst_no = inst_table.add_instance(c, g)
	ext.inst_no = g.inst_no

	return g, nil
}

func (hak *Hak) Close() {
	C.hak_close(hak.c)
	deregister_instance(hak)
}

func (hak *Hak) make_errinfo() *Err {
	var loc C.hak_loc_t
	var err Err
	var errnum C.hak_errnum_t

	err.Msg = hak.get_errmsg()

	errnum = C.hak_geterrnum(hak.c)
	if errnum == C.HAK_ESYNERR {
		var synerr C.hak_synerr_t
		C.hak_getsynerr(hak.c, &synerr)
		loc = synerr.loc
	} else {
		C.hak_geterrloc(hak.c, &loc)
	}

	err.Line = uint(loc.line)
	err.Colm = uint(loc.colm)
	if loc.file != nil {
		err.File = string(ucstr_to_rune_slice(loc.file))
	} else {
		err.File = hak.io.cci_main
	}
	return &err
}

func (hak *Hak) GetTrait() BitMask {
	var x C.int
	var log_mask BitMask = 0

	x = C.hak_getoption(hak.c, C.HAK_TRAIT, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to get log mask - %s", hak.get_errmsg()))
	}

	return log_mask
}

func (hak *Hak) SetTrait(log_mask BitMask) {
	var x C.int

	x = C.hak_setoption(hak.c, C.HAK_TRAIT, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log mask - %s", hak.get_errmsg()))
	}
}

func (hak *Hak) GetLogMask() BitMask {
	var x C.int
	var log_mask BitMask = 0

	x = C.hak_getoption(hak.c, C.HAK_LOG_MASK, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to get log mask - %s", hak.get_errmsg()))
	}

	return log_mask
}

func (hak *Hak) SetLogMask(log_mask BitMask) {
	var x C.int

	x = C.hak_setoption(hak.c, C.HAK_LOG_MASK, unsafe.Pointer(&log_mask))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log mask - %s", hak.get_errmsg()))
	}
}

func (hak *Hak) GetLogTarget() string {
	var x C.int
	var tgt *C.char

	x = C.hak_getoption(hak.c, C.HAK_LOG_TARGET_BCSTR, unsafe.Pointer(&tgt))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log target - %s", hak.get_errmsg()))
	}

	return C.GoString(tgt)
}

func (hak *Hak) SetLogTarget(target string) {
	var x C.int
	var tgt *C.char

	tgt = C.CString(target) // TODO: need error check?
	defer C.free(unsafe.Pointer(tgt))

	x = C.hak_setoption(hak.c, C.HAK_LOG_TARGET_BCSTR, unsafe.Pointer(tgt))
	if x <= -1 {
		// this must not happen
		panic(fmt.Errorf("unable to set log target - %s", hak.get_errmsg()))
	}
}

func (hak *Hak) Ignite(memsize uintptr) error {
	var x C.int

	x = C.hak_ignite(hak.c, C.hak_oow_t(memsize))
	if x <= -1 {
		//return fmt.Errorf("unable to ignite - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}

	return nil
}

func (hak *Hak) AddBuiltinPrims() error {
	var x C.int

	x = C.hak_addbuiltinprims(hak.c)
	if x <= -1 {
		//return fmt.Errorf("unable to add built-in primitives - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}
	return nil
}

// the name of the main cci stream is required because:
// - the main stream is not handled by this IO handler
// - the feeder must read the main stream and pass data.
// - the inclusion of another file from the main stream requires the path information of the main strea.
func (hak *Hak) AttachCCIO(cci CciImpl, main_cci_name string) error {
	var x C.int
	var old_cci CciImpl
	var old_cci_name string

	old_cci = hak.io.cci
	old_cci_name = hak.io.cci_main

	hak.io.cci = cci
	hak.io.cci_main = main_cci_name

	x = C.hak_attachccio(hak.c, C.hak_io_impl_t(C.hak_cci_handler_for_go))
	if x <= -1 {
		// restore the io handler set due to attachment failure
		hak.io.cci_main = old_cci_name
		hak.io.cci = old_cci
		//return fmt.Errorf("unable to attach source input stream handler - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}
	return nil
}

func (hak *Hak) AttachUDIO(udi UdiImpl, udo UdoImpl) error {
	var x C.int
	var os UdiImpl
	var op UdoImpl

	os = hak.io.udi
	op = hak.io.udo

	hak.io.udi = udi
	hak.io.udo = udo

	x = C.hak_attachudio(hak.c,
		C.hak_io_impl_t(C.hak_udi_handler_for_go),
		C.hak_io_impl_t(C.hak_udo_handler_for_go))
	if x <= -1 {
		//restore the io handlers set due to attachment failure
		hak.io.udi = os
		hak.io.udo = op
		//return fmt.Errorf("unable to attach user data stream handlers - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}
	return nil
}

func (hak *Hak) BeginFeed() error {
	var x C.int

	x = C.hak_beginfeed(hak.c, nil)
	if x <= -1 {
		//return fmt.Errorf("unable to begin feeding - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}

	return nil
}

func (hak *Hak) EndFeed() error {
	var x C.int

	x = C.hak_endfeed(hak.c)
	if x <= -1 {
		//return fmt.Errorf("unable to end feeding - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}

	return nil
}

func (hak *Hak) FeedString(str string) error {
	var x C.int
	var q []C.hak_uch_t

	q = string_to_uchars(str)
	x = C.hak_feed(hak.c, &q[0], C.hak_oow_t(len(q)))
	if x <= -1 {
		//return fmt.Errorf("unable to feed string - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}
	return nil
}

func (hak *Hak) FeedRunes(str []rune) error {
	var x C.int
	var q []C.hak_uch_t

	q = rune_slice_to_uchars(str)
	x = C.hak_feed(hak.c, &q[0], C.hak_oow_t(len(q)))
	if x <= -1 {
		//return fmt.Errorf("unable to feed runes - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}
	return nil
}

func (hak *Hak) FeedFromReader(rdr io.Reader) error {
	var err error
	var n int
	var x C.int
	var buf [1024]byte

	for {
		n, err = rdr.Read(buf[:])
		if err == io.EOF {
			break
		} else if err != nil {
			//return fmt.Errorf("unable to read bytes - %s", err.Error())
			return &Err{File: hak.io.cci_main, Msg: err.Error()}
		}

		x = C.hak_feedbchars(hak.c, (*C.hak_bch_t)(unsafe.Pointer(&buf[0])), C.hak_oow_t(n))
		if x <= -1 {
			//return fmt.Errorf("unable to feed bytes - %s", hak.get_errmsg())
			return hak.make_errinfo()
		}
	}

	return nil
}

func (hak *Hak) FeedFromFile(file string) error {
	var f *os.File
	var err error

	f, err = os.Open(file)
	if err != nil {
		//return fmt.Errorf("unable to open %s - %s", file, err.Error())
		return &Err{File: file, Msg: err.Error()}
	}

	defer f.Close()
	return hak.FeedFromReader(bufio.NewReader(f))
}

func (hak *Hak) Execute() error {
	var x C.hak_oop_t

	x = C.hak_execute(hak.c)
	if x == nil {
		//return fmt.Errorf("unable to execute - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}

	// TODO: wrap C.hak_oop_t in a go type
	//       and make this function to return 'x' in the wrapper
	return nil
}

func (hak *Hak) Decode() error {
	var x C.int

	x = C.hak_decode(hak.c, C.hak_getcode(hak.c), 0, C.hak_getbclen(hak.c))
	if x <= -1 {
		//return fmt.Errorf("unable to decode byte codes - %s", hak.get_errmsg())
		return hak.make_errinfo()
	}

	return nil
}

func (hak *Hak) get_errmsg() string {
	return C.GoString(C.hak_geterrbmsg(hak.c))
}

func (hak *Hak) set_errmsg(num C.hak_errnum_t, msg string) {
	var ptr *C.char
	ptr = C.CString(msg)
	defer C.free(unsafe.Pointer(ptr))
	C.hak_seterrbmsg(hak.c, num, ptr)
}

// -----------------------------------------------------------

func ucstr_to_rune_slice(str *C.hak_uch_t) []rune {
	return uchars_to_rune_slice(str, uintptr(C.hak_count_ucstr(str)))
}

func uchars_to_rune_slice(str *C.hak_uch_t, len uintptr) []rune {
	var res []rune
	var i uintptr
	var ptr uintptr

	// TODO: proper encoding...
	ptr = uintptr(unsafe.Pointer(str))
	res = make([]rune, len)
	for i = 0; i < len; i++ {
		res[i] = rune(*(*C.hak_uch_t)(unsafe.Pointer(ptr)))
		ptr += unsafe.Sizeof(*str)
	}
	return res
}

func string_to_uchars(str string) []C.hak_uch_t {
	var r []rune
	var c []C.hak_uch_t
	var i int

	// TODO: proper encoding
	r = []rune(str)
	c = make([]C.hak_uch_t, len(r), len(r))
	for i = 0; i < len(r); i++ {
		c[i] = C.hak_uch_t(r[i])
	}

	return c
}

func rune_slice_to_uchars(r []rune) []C.hak_uch_t {
	var c []C.hak_uch_t
	var i int

	// TODO: proper encoding
	c = make([]C.hak_uch_t, len(r), len(r))
	for i = 0; i < len(r); i++ {
		c[i] = C.hak_uch_t(r[i])
	}
	return c
}

func c_to_go(c *C.hak_t) *Hak {
	var ext *Ext
	var inst Instance

	ext = (*Ext)(unsafe.Pointer(C.hak_getxtn(c)))
	inst = inst_table.slot_to_instance(ext.inst_no)
	return inst.g
}

// -----------------------------------------------------------

func (err* Err) Error() string {
	return fmt.Sprintf("%s[%d,%d] %s", err.File, err.Line, err.Colm, err.Msg)
}
