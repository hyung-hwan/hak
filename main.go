package main

import (
	"fmt"
	"os"
	"strings"

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

/* to be set in build time */
var BINDIR = "."
var SBINDIR = "."
var LIBDIR = "."
var SYSCONFDIR = "."

type Param struct {
	log_file   string
	input_file string
}

func handle_arguments(param *Param) error {
	var nargs int = len(os.Args)
	var i int

	for i = 1; i < nargs; i++ {
		if strings.HasPrefix(os.Args[i], "--log=") {
			param.log_file = os.Args[i][6:]
		} else if os.Args[i] == "--log" {
			i++
			param.log_file = os.Args[i]
		} else if strings.HasPrefix(os.Args[i], "--") || strings.HasPrefix(os.Args[i], "-") {
			return fmt.Errorf("unknown option - %s", os.Args[i])
		} else {
			break
		}
	}

	if i >= nargs {
		return fmt.Errorf("no input file specified")
	}

	param.input_file = os.Args[i]
	return nil
}

func main() {

	var x *hcl.HCL = nil
	var err error = nil
	var param Param

	var rfh hcl.CciFileHandler
	var sfh hcl.UdiFileHandler
	var pfh hcl.UdoFileHandler

	err = handle_arguments(&param)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	x, err = hcl.New()
	if err != nil {
		fmt.Printf("Error: failed to instantiate hcl - %s\n", err.Error())
		os.Exit(1)
	}

	if param.log_file != "" {
		x.SetLogMask(^hcl.BitMask(0))
		x.SetLogTarget("/dev/stderr")
	}

	x.SetTrait(x.GetTrait() | hcl.TRAIT_LANG_ENABLE_EOL | hcl.TRAIT_LANG_ENABLE_BLOCK)

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

	err = x.AttachCCIO(&rfh, param.input_file)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	err = x.AttachUDIO(&sfh, &pfh)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	err = x.BeginFeed()
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		goto oops
	}

	err = x.FeedFromFile(param.input_file)
	//err = x.FeedString(`(printf ">>>>>>>>> [%d]\n" (+ 30 455))
	//   (printf ">>>>>>>>> [%d]\n" (+ 11 455))
	//   #include "a.hcl"
	//   (printf ">>>>>>>>> [%d]\n" (+ 20 455))`)
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
	x.SetLogMask(0)

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
