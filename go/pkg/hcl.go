package hcl

/*
#cgo CFLAGS: -I/home/hyung-hwan/xxx/include -g -Wall
#cgo LDFLAGS: -L/home/hyung-hwan/xxx/lib -lhcl -ldl -lquadmath

#include <hcl.h>
#include <hcl-utl.h>
*/
import "C"

import (
	"fmt"
	"runtime"
//	"unsafe"
)

type HCL struct {
	inst *C.hcl_t
	no int

	io struct {
		r IOHandle
		w IOHandle
	}
}

var inst_table InstanceTable

func deregister_instance(hcl *HCL) {
	if (hcl.no >= 0) {
		inst_table.delete_instance(hcl.no)
		hcl.no = -1
	}
}

func New() (*HCL, error) {
	var inst *C.hcl_t
	var hcl* HCL
	inst = C.hcl_open(nil, 0, nil, nil) // TODO: prim in, errnum out
	hcl = &HCL{inst: inst, no : -1}

	runtime.SetFinalizer(hcl, deregister_instance)
	hcl.no = inst_table.add_instance(inst, hcl)

	return hcl, nil
}

func (hcl *HCL) Close() {
	C.hcl_close (hcl.inst)
	deregister_instance (hcl)
}

func (hcl *HCL) Ignite(memsize uintptr) error {
	if C.hcl_ignite(hcl.inst, C.hcl_oow_t(memsize)) <= -1 {
		// TODO: need to convert string...
		return fmt.Errorf ("unable to ignite: %s", ucstr_to_rune_slice(C.hcl_geterrstr(hcl.inst)))
	}

	return nil
}

func (hcl *HCL) AddBuiltinPrims() error {
	if C.hcl_addbuiltinprims(hcl.inst) <= -1 {
	}
	return nil
}

func (hcl *HCL) AttachIO() error {
	return nil
}

func (hcl *HCL) FeedString(str []rune) error {
	return nil
}

func (hcl *HCL) BeginFeed()  {
}

func (hcl *HCL) EndFeed()  {
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
//TODO		res[i] := str[i]
	}
	return res
}


func c_to_go (inst *C.hcl_t) *HCL {
	return nil
}
