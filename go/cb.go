package hcl

/*
#include <hcl.h>
#include <hcl-utl.h>
*/
import "C"

import (
	"bufio"
	"io"
	"os"
	"sync"
	"unsafe"
)

type IOHandle struct {
	file *os.File
	ioif interface{}
}

type IOHandleTable struct {
	mtx        sync.Mutex
	handles    []IOHandle
	free_slots []int
}

func (io *IOHandleTable) add_io_handle(f *os.File, ioif interface{}) int {
	io.mtx.Lock()
	defer io.mtx.Unlock()

	var n int = len(io.free_slots)

	if n <= 0 { // no free slots
		io.handles = append(io.handles, IOHandle{file: f, ioif: ioif})
		return len(io.handles) - 1
	} else {
		var slot int
		n--
		slot = io.free_slots[n]
		io.free_slots = io.free_slots[:n]
		io.handles[slot].file = f
		io.handles[slot].ioif = ioif
		return slot
	}
}

func (io *IOHandleTable) del_io_handle(slot int) IOHandle {
	var (
		h IOHandle
		n int
	)

	io.mtx.Lock()
	defer io.mtx.Unlock()

	h = io.handles[slot]
	io.handles[slot].file = nil
	io.handles[slot].ioif = nil

	n = len(io.handles)
	if slot == n-1 {
		io.handles = io.handles[:n-1]
	} else {
		io.free_slots = append(io.free_slots, slot)
	}

	return h
}

func (io *IOHandleTable) slot_to_io_handle(slot int) IOHandle {
	io.mtx.Lock()
	defer io.mtx.Unlock()
	return io.handles[slot]
}

var io_tab IOHandleTable = IOHandleTable{}

//export hcl_go_cci_handler
func hcl_go_cci_handler(c *C.hcl_t, cmd C.hcl_io_cmd_t, arg unsafe.Pointer) C.int {
	var (
		g   *HCL
		err error
	)

	g = c_to_go(c)

	switch cmd {
	case C.HCL_IO_OPEN:
		var (
			ioarg         *C.hcl_io_cciarg_t
			name          string
			includer_name string
			fd            int
		)

		ioarg = (*C.hcl_io_cciarg_t)(arg)
		if ioarg.name == nil { // main stream when it's not feed based.
			name = ""
		} else {
			var k []rune = ucstr_to_rune_slice(ioarg.name)
			name = string(k)
		}

		if ioarg.includer == nil || ioarg.includer.name == nil {
			includer_name = ""
		} else {
			var k []rune = ucstr_to_rune_slice(ioarg.includer.name)
			includer_name = string(k)
		}

		fd, err = g.io.r.Open(g, name, includer_name)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}

		ioarg.handle = unsafe.Pointer(uintptr(fd))
		return 0

	case C.HCL_IO_CLOSE:
		var ioarg *C.hcl_io_cciarg_t = (*C.hcl_io_cciarg_t)(arg)
		g.io.r.Close(int(uintptr(ioarg.handle)))
		return 0

	case C.HCL_IO_READ:
		var (
			ioarg *C.hcl_io_cciarg_t
			n     int
			i     int
			buf   []rune
		)
		ioarg = (*C.hcl_io_cciarg_t)(arg)

		buf = make([]rune, 1024) // TODO:  different size...
		n, err = g.io.r.Read(int(uintptr(ioarg.handle)), buf)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}

		for i = 0; i < n; i++ { // TODO: use a proper conversion when the rune size is different from hio_uch_t
			ioarg.buf[i] = C.hcl_uch_t(buf[i])
		}
		ioarg.xlen = C.hcl_oow_t(n)
		return 0
	}

	C.hcl_seterrnum(c, C.HCL_EIOERR)
	return -1
}

//export hcl_go_udi_handler
func hcl_go_udi_handler(c *C.hcl_t, cmd C.hcl_io_cmd_t, arg unsafe.Pointer) C.int {
	var (
		g   *HCL
		err error
	)

	g = c_to_go(c)

	switch cmd {
	case C.HCL_IO_OPEN:
		err = g.io.s.Open(g)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}

		// we don't need to set ioarg.handle because the c object to go object
		// mapping has been established and it doesn't handle multiple streams
		return 0

	case C.HCL_IO_CLOSE:
		g.io.s.Close()
		return 0

	case C.HCL_IO_READ:
		var (
			ioarg *C.hcl_io_udiarg_t
			n     int
			err   error
			buf   []rune
		)
		ioarg = (*C.hcl_io_udiarg_t)(arg)

		buf = make([]rune, 1024) // TODO:  different size...
		n, err = g.io.s.Read(buf)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}
		ioarg.xlen = C.ulong(n)
		return 0
	}

	C.hcl_seterrnum(c, C.HCL_EIOERR)
	return -1
}

//export hcl_go_udo_handler
func hcl_go_udo_handler(c *C.hcl_t, cmd C.hcl_io_cmd_t, arg unsafe.Pointer) C.int {
	var (
		g   *HCL
		err error
	)

	g = c_to_go(c)

	switch cmd {
	case C.HCL_IO_OPEN:
		err = g.io.p.Open(g)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}

		// we don't need to set ioarg.handle because the c object to go object
		// mapping has been established and it doesn't handle multiple streams
		return 0

	case C.HCL_IO_CLOSE:
		g.io.p.Close()
		return 0

	case C.HCL_IO_WRITE:
		var (
			ioarg *C.hcl_io_udoarg_t
			data  []rune
			err   error
		)
		ioarg = (*C.hcl_io_udoarg_t)(arg)
		data = uchars_to_rune_slice((*C.hcl_uch_t)(ioarg.ptr), uintptr(ioarg.len))
		err = g.io.p.Write(data)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}
		ioarg.xlen = C.hcl_oow_t(len(data))
		return 0

	case C.HCL_IO_WRITE_BYTES:
		var (
			ioarg *C.hcl_io_udoarg_t
			data  []byte
			err   error
		)
		ioarg = (*C.hcl_io_udoarg_t)(arg)
		data = unsafe.Slice((*byte)(ioarg.ptr), ioarg.len)
		err = g.io.p.WriteBytes(data)
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}
		ioarg.xlen = C.hcl_oow_t(len(data))
		return 0

	case C.HCL_IO_FLUSH:
		var err error = g.io.p.Flush()
		if err != nil {
			g.set_errmsg (C.HCL_EIOERR, err.Error())
			return -1
		}
		return 0
	}

	C.hcl_seterrnum(c, C.HCL_EIOERR)
	return -1
}

// ------------------------------------------------------
type CciFileHandler struct {
	g *HCL
}

func (p *CciFileHandler) Open(g *HCL, name string, includer_name string) (int, error) {
	var (
		f   *os.File
		r   *bufio.Reader
		fd  int
		err error
	)

	if name == "" {
		f = os.Stdin
	} else {
		f, err = os.Open(name)
		if err != nil {
			return -1, err
		}
	}

	r = bufio.NewReader(f)
	fd = io_tab.add_io_handle(f, r)

	p.g = g
	return fd, nil
}

func (p *CciFileHandler) Close(fd int) {
	var hnd IOHandle = io_tab.slot_to_io_handle(fd)
	if hnd.file != nil {
		if hnd.file != os.Stdout && hnd.file != os.Stderr {
			hnd.file.Close()
		}
		hnd.file = nil
		hnd.ioif = nil
	}
}

func (p *CciFileHandler) Read(fd int, buf []rune) (int, error) {
	var (
		hnd IOHandle
		i   int
		c   rune
		r   *bufio.Reader
		err error
	)

	hnd = io_tab.slot_to_io_handle(fd)
	r, _ = hnd.ioif.(*bufio.Reader)
	for i = 0; i < len(buf); i++ {
		c, _, err = r.ReadRune()
		if err == io.EOF {
			break
		} else if err != nil {
			return -1, err
		}

		buf[i] = c
	}

	return i, nil
}

// ------------------------------------------------------
type UdiFileHandler struct {
	g *HCL
	f *os.File
	r *bufio.Reader
}

func (p *UdiFileHandler) Open(g *HCL) error {
	var (
		f *os.File
	//	err error
	)

	f = os.Stdin
	//f, err = os.Open("/dev/stdin")
	//if err != nil {
	//	return err
	//}

	p.r = bufio.NewReader(f)
	p.f = f
	p.g = g

	return nil
}

func (p *UdiFileHandler) Close() {
	if p.f != nil {
		if p.f != os.Stdout && p.f != os.Stderr {
			p.f.Close()
		}
		p.f = nil
		p.r = nil
	}
}

func (p *UdiFileHandler) Read(buf []rune) (int, error) {
	var (
		i   int
		c   rune
		err error
	)

	// flush all pending print out before reading
	p.g.io.p.Flush()

	for i = 0; i < len(buf); i++ {
		c, _, err = p.r.ReadRune()
		if err == io.EOF {
			break
		} else if err != nil {
			return -1, err
		}

		buf[i] = c
	}

	return i, nil
}

// ------------------------------------------------------

type UdoFileHandler struct {
	f *os.File
	w *bufio.Writer
}

func (p *UdoFileHandler) Open(g *HCL) error {
	var (
		f *os.File
	//	err error
	)

	//		f, err = os.OpenFile("/dev/stdout", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	//		if err != nil {
	//			return err
	//		}

	f = os.Stdout

	p.w = bufio.NewWriter(f)
	p.f = f

	//fmt.Fprintf(os.Stderr, "XXXXXXXXXX open porint\n")
	return nil
}

func (p *UdoFileHandler) Close() {

	//fmt.Fprintf(os.Stderr, "XXXXXXXXXX close porint\n")
	if p.f != nil {
		if p.f != os.Stdout && p.f != os.Stderr {
			p.f.Close()
		}
		p.f = nil
		p.w = nil
	}
}

func (p *UdoFileHandler) Write(data []rune) error {
	var err error

	//fmt.Fprintf(os.Stderr, "XXXXXXXXXX write porint\n")
	_, err = p.w.WriteString(string(data))
	p.w.Flush() // TODO: is this needed?
	return err
}

func (p *UdoFileHandler) WriteBytes(data []byte) error {
	var err error

	//fmt.Fprintf(os.Stderr, "XXXXXXXXXX write porint\n")
	_, err = p.w.Write(data)
	p.w.Flush() // TODO: is this needed?
	return err
}

func (p *UdoFileHandler) Flush() error {
	//fmt.Fprintf(os.Stderr, "XXXXXXXXXX flush porint\n")
	return p.w.Flush()
}
