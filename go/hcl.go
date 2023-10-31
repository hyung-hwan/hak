package hcl

/*
#include <hcl.h>
#include <hcl-utl.h>

extern int hcl_go_read_handler (hcl_t hcl, hcl_iocmd_t cmd, void* arg);
extern int hcl_go_scan_handler (hcl_t hcl, hcl_iocmd_t cmd, void* arg);
extern int hcl_go_print_handler (hcl_t hcl, hcl_iocmd_t cmd, void* arg);

int hcl_read_handler_for_go (hcl_t hcl, hcl_iocmd_t cmd, void* arg) {
    return hcl_go_read_handler(hcl, cmd, arg);
}
int hcl_scan_handler_for_go (hcl_t hcl, hcl_iocmd_t cmd, void* arg) {
    return hcl_go_scan_handler(hcl, cmd, arg);
}
int hcl_print_handler_for_go (hcl_t hcl, hcl_iocmd_t cmd, void* arg) {
    return hcl_go_print_handler(hcl, cmd, arg);
}
*/
import "C"

import (
	"fmt"
	"runtime"
	"unsafe"
)

type IOReadImpl interface {
	Open(g *HCL, name string, includer_name string) (int, error)
	Close(fd int)
	Read(fd int, buf []rune) (int, error)
}

type IOScanImpl interface {
	Open(g *HCL) error
	Close()
	Read(buf []rune) (int, error)
}

type IOPrintImpl interface {
	Open(g *HCL) error
	Close()
	Write(data []rune) error
	WriteBytes(data []byte) error
	Flush() error
}

type IOImplSet struct {
	r IOReadImpl
	s IOScanImpl
	p IOPrintImpl
}

type HCL struct {
	c       *C.hcl_t
	inst_no int
	io IOImplSet
}

type Ext struct {
	inst_no int
}

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

func (hcl *HCL) Ignite(memsize uintptr) error {
	if C.hcl_ignite(hcl.c, C.hcl_oow_t(memsize)) <= -1 {
		return fmt.Errorf("unable to ignite: %s", string(ucstr_to_rune_slice(C.hcl_geterrstr(hcl.c))))
	}

	return nil
}

func (hcl *HCL) AddBuiltinPrims() error {
	if C.hcl_addbuiltinprims(hcl.c) <= -1 {
	}
	return nil
}

func (hcl *HCL) AttachIO(r IOReadImpl, s IOScanImpl, p IOPrintImpl) error {
	var x C.int
	var io IOImplSet

	io = hcl.io

	hcl.io.r = r
	hcl.io.s = s
	hcl.io.p = p

	x = C.hcl_attachio(hcl.c,
		C.hcl_ioimpl_t(C.hcl_read_handler_for_go),
		C.hcl_ioimpl_t(C.hcl_scan_handler_for_go),
		C.hcl_ioimpl_t(C.hcl_print_handler_for_go))
	if x <= -1 {
		hcl.io = io // restore the set
		return fmt.Errorf("unable to attach I/O handlers: %s", string(ucstr_to_rune_slice(C.hcl_geterrstr(hcl.c))))
	}
	return nil
}

func (hcl *HCL) FeedString(str []rune) error {
	return nil
}

func (hcl *HCL) BeginFeed() {
}

func (hcl *HCL) EndFeed() {
}

func (hcl *HCL) Execute() {
}

func ucstr_to_rune_slice(str *C.hcl_uch_t) []rune {
	return uchars_to_rune_slice(str, uintptr(C.hcl_count_ucstr(str)))
}

func uchars_to_rune_slice(str *C.hcl_uch_t, len uintptr) []rune {
	var res []rune
	var i uintptr

	// TODO: proper encoding...
	res = make([]rune, len)
	for i = 0; i < len; i++ {
		res[i] = *(*rune)(unsafe.Pointer(uintptr(unsafe.Pointer(str)) + unsafe.Sizeof(*str)*i))
	}
	return res
}

func c_to_go(c *C.hcl_t) *HCL {
	var ext *Ext
	var inst Instance

	ext = (*Ext)(unsafe.Pointer(C.hcl_getxtn(c)))
	inst = inst_table.slot_to_instance(ext.inst_no)
	return inst.g
}
