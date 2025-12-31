package hak

/*
#include <hak.h>
*/
import "C"
import "sync"
import "weak"

type Instance struct {
	c *C.hak_t // c object
	g weak.Pointer[Hak] // go object as a weak pointer
}

type InstanceTable struct {
	mtx        sync.Mutex
	insts      []Instance
	free_slots []int
}

var inst_table InstanceTable

func (itab *InstanceTable) add_instance(c *C.hak_t, g *Hak) int {
	itab.mtx.Lock()
	defer itab.mtx.Unlock()

	var n int = len(itab.free_slots)

	if n <= 0 { // no free slots
		itab.insts = append(itab.insts, Instance{c: c, g: weak.Make(g)})
		return len(itab.insts) - 1
	} else {
		var slot int
		n--
		slot = itab.free_slots[n]
		itab.free_slots = itab.free_slots[:n]
		itab.insts[slot].c = c
		itab.insts[slot].g = weak.Make(g)
		return slot
	}
}

func (itab *InstanceTable) delete_instance(slot int) Instance {
	var h Instance
	var n int

	itab.mtx.Lock()
	defer itab.mtx.Unlock()

	h = itab.insts[slot]
	itab.insts[slot].c = nil
	itab.insts[slot].g = weak.Make((*Hak)(nil))

	n = len(itab.insts)
	if slot == n-1 {
		itab.insts = itab.insts[:n-1]
	} else {
		itab.free_slots = append(itab.free_slots, slot)
	}

	return h
}

func (itab *InstanceTable) slot_to_instance(slot int) Instance {
	itab.mtx.Lock()
	defer itab.mtx.Unlock()
	return itab.insts[slot]
}
