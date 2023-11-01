package main

import (
	_ "cfg"
	"fmt"
	"os"

	hcl "code.miflux.com/hyung-hwan/hcl/go"
)

/*
		var src []uint16 = utf16.Encode(([]rune)(
			`(printf ">>>>>>>>> [%d]\n" (+ 30 455))

(printf ">>>>>>>>> [%d]\n" (+ 11 455))
#include "a.hcl"
(printf ">>>>>>>>> [%d]\n" (+ 20 455))
`))
*/
func main() {

	var x *hcl.HCL = nil
	var err error = nil

	var rfh hcl.ReadFileHandler
	var sfh hcl.ScanFileHandler
	var pfh hcl.PrintFileHandler

	x, err = hcl.New()
	if err != nil {
		fmt.Printf("Error: failed to instantiate hcl - %s\n", err.Error())
		os.Exit(1)
	}

	x.SetLogMask (^hcl.BitMask(0))
	x.SetLogTarget ("/dev/stderr")

	err = x.Ignite(1000000)
	if err != nil {
		fmt.Printf("Error: failed to ignite - %s\n", err.Error())
		goto oops
	}
	err = x.AddBuiltinPrims()
	if err != nil {
		fmt.Printf("Error: failed to add builtin primitives - %s\n", err.Error())
		goto oops
	}

	err = x.AttachIO(&rfh, &sfh, &pfh)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	err = x.FeedString(`(printf ">>>>>>>>> [%d]\n" (+ 30 455))
	   (printf ">>>>>>>>> [%d]\n" (+ 11 455))
	   #include "a.hcl"
	   (printf ">>>>>>>>> [%d]\n" (+ 20 455))`)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	err = x.EndFeed()
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	x.Decode()
	x.SetLogMask (0)

	err = x.Execute()
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	x.Close()

	os.Exit(0)

oops:
	if x != nil {
		x.Close()
		x = nil
	}
	os.Exit(1)
}
