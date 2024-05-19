package hcl

/*
#include <hcl.h>
#include <hcl-utl.h>
#include <string.h> // for memcpy
*/
import "C"

import (
	"bufio"
	"io"
	"os"
	"path"
	"path/filepath"
	"reflect"
	"sync"
	"unsafe"
)

//import "fmt"

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
			fd            int
			tptr          unsafe.Pointer
			tlen          C.size_t
		)

		ioarg = (*C.hcl_io_cciarg_t)(arg)

		if ioarg.includer == nil /* || ioarg.includer.name == nil */ {
			// main stream
			name = g.io.cci_main
		} else {
			// actual included stream
			var includer_name string

			name = string(ucstr_to_rune_slice(ioarg.name))

			tptr = ioarg.includer.handle
			tlen = *(*C.size_t)(unsafe.Pointer(uintptr(tptr) + unsafe.Sizeof(fd)))

			includer_name = C.GoStringN((*C.char)(unsafe.Pointer(uintptr(tptr)+unsafe.Sizeof(fd)+unsafe.Sizeof(tlen))), C.int(tlen))
			name = filepath.Join(path.Dir(includer_name), name)
		}

		tlen = C.size_t(len(name)) // number of bytes in the string
		tptr = C.hcl_allocmem(c, C.size_t(unsafe.Sizeof(fd)) + C.size_t(unsafe.Sizeof(tlen)) + tlen)
		if tptr == nil {
			g.set_errmsg(C.HCL_ESYSMEM, "cci name allocation failure")
			return -1
		}

		if ioarg.includer == nil {
			fd = -1
		} else {
			fd, err = g.io.cci.Open(g, name)
			if err != nil {
				g.set_errmsg(C.HCL_EIOERR, err.Error())
				C.hcl_freemem(c, tptr)
				return -1
			}
		}

		// | fd | length | name bytes of the length |
		*(*int)(tptr) = fd;
		*(*C.size_t)(unsafe.Pointer(uintptr(tptr)+unsafe.Sizeof(fd))) = tlen;

		// C.CString() allocates a memory block. Use a SliceHeader to avoid extra memory allocation
		// for the string conversion. Create a fake slice header that can be used with copy() instead.
		var dsthdr reflect.SliceHeader
		dsthdr.Data = uintptr(tptr)+unsafe.Sizeof(fd)+unsafe.Sizeof(tlen)
		dsthdr.Len = int(tlen)
		dsthdr.Cap = int(tlen)
		copy(*(*[]byte)(unsafe.Pointer(&dsthdr)), name);

		ioarg.handle = tptr
		return 0

	case C.HCL_IO_CLOSE:
		var (
			fd    int
			ioarg *C.hcl_io_cciarg_t
		)

		ioarg = (*C.hcl_io_cciarg_t)(arg)
		fd = *(*int)(ioarg.handle) // the descriptor at the beginning
		if fd >= 0 {
			g.io.cci.Close(fd)
		}
		C.hcl_freemem(c, ioarg.handle)
		ioarg.handle = nil
		return 0

	case C.HCL_IO_READ:
		var (
			ioarg *C.hcl_io_cciarg_t
			n     int
			i     int
			buf   []rune
			dummy C.hcl_uch_t
			fd    int
		)
		ioarg = (*C.hcl_io_cciarg_t)(arg)

		fd = *(*int)(ioarg.handle) // the descriptor at the beginning

		buf = make([]rune, 1024) // TODO:  different size...
		n, err = g.io.cci.Read(fd, buf)
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
			return -1
		}

		ioarg.byte_oriented = 0
		if unsafe.Sizeof(buf[0]) == unsafe.Sizeof(dummy) {
			C.memcpy(
				unsafe.Pointer(&ioarg.buf[0]),
				unsafe.Pointer(&buf[0]),
				C.size_t(unsafe.Sizeof(buf[0])*uintptr(n)))
		} else {
			var dst uintptr
			// work around cgo's union issue. not able to access individual union
			// member fields. cgo treats union as byte aggregates.
			dst = uintptr(unsafe.Pointer(&ioarg.buf[0]))
			for i = 0; i < n; i++ {
				//ioarg.buf.c[i] = C.hcl_uch_t(buf[i])
				*(*C.hcl_uch_t)(unsafe.Pointer(dst)) = C.hcl_uch_t(buf[i])
				dst += unsafe.Sizeof(dummy)
			}
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
		err = g.io.udi.Open(g)
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
			return -1
		}

		// we don't need to set ioarg.handle because the c object to go object
		// mapping has been established and it doesn't handle multiple streams
		return 0

	case C.HCL_IO_CLOSE:
		g.io.udi.Close()
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
		n, err = g.io.udi.Read(buf)
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
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
		err = g.io.udo.Open(g)
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
			return -1
		}

		// we don't need to set ioarg.handle because the c object to go object
		// mapping has been established and it doesn't handle multiple streams
		return 0

	case C.HCL_IO_CLOSE:
		g.io.udo.Close()
		return 0

	case C.HCL_IO_WRITE:
		var (
			ioarg *C.hcl_io_udoarg_t
			data  []rune
			err   error
		)
		ioarg = (*C.hcl_io_udoarg_t)(arg)
		data = uchars_to_rune_slice((*C.hcl_uch_t)(ioarg.ptr), uintptr(ioarg.len))
		err = g.io.udo.Write(data)
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
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
		err = g.io.udo.WriteBytes(data)
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
			return -1
		}
		ioarg.xlen = C.hcl_oow_t(len(data))
		return 0

	case C.HCL_IO_FLUSH:
		var err error = g.io.udo.Flush()
		if err != nil {
			g.set_errmsg(C.HCL_EIOERR, err.Error())
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

func (p *CciFileHandler) Open(g *HCL, name string) (int, error) {
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
	p.g.io.udo.Flush()

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
