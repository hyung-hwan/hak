package main

import (
	"flag"
	"fmt"
	"hak"
	"os"
	//"strings"
)

/*
		var src []uint16 = utf16.Encode(([]rune)(
			`(printf ">>>>>>>>> [%d]\n" (+ 30 455))

(printf ">>>>>>>>> [%d]\n" (+ 11 455))
#include "a.hak"
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
	heapsize   uint
	modlibdirs string
	fs_usage   func()
}

func empty_usage() {

}

func handle_arguments(param *Param) error {
	/*
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
	*/
	var fs *flag.FlagSet
	var heapsize *uint
	var modlibdirs *string
	var log *string
	var err error

	fs = flag.NewFlagSet(os.Args[0], flag.ContinueOnError)

	heapsize = fs.Uint("heapsize", 0, "specify heap size")
	modlibdirs = fs.String("modlibdirs", "", "specify module library directories")
	log = fs.String("log", "", "specify log file")

	param.fs_usage = fs.Usage
	fs.Usage = empty_usage // i don't want fs.Parse() print the usage
	err = fs.Parse(os.Args[1:])
	fs.Usage = param.fs_usage // restore it
	if err != nil {
		return fmt.Errorf("command line error - %s", err.Error())
	}

	if fs.NArg() < 1 {
		return fmt.Errorf("no input file specified")
	} else if fs.NArg() > 1 {
		return fmt.Errorf("too many input files specified")
	}

	param.input_file = fs.Arg(0);
	param.log_file = *log // TODO: parse the option part  (e.g. --log /dev/stderr,debug+)
	param.heapsize = *heapsize // TODO: set this to hak
	param.modlibdirs = *modlibdirs // TODO: set this to hak
	return nil;
}

func main() {

	var x *hak.Hak = nil
	var err error = nil
	var param Param

	var rfh hak.CciFileHandler
	var sfh hak.UdiFileHandler
	var pfh hak.UdoFileHandler

	err = handle_arguments(&param)
	if err != nil {
		fmt.Printf("ERROR: %s\n", err.Error())
		param.fs_usage()
		os.Exit(1)
	}

	x, err = hak.New()
	if err != nil {
		fmt.Printf("ERROR: failed to instantiate hak - %s\n", err.Error())
		os.Exit(1)
	}

	if param.log_file != "" {
		x.SetLogMask(^hak.BitMask(0))
		x.SetLogTarget("/dev/stderr")
	}

	x.SetTrait(x.GetTrait() | hak.TRAIT_LANG_ENABLE_EOL)

	err = x.Ignite(1000000)
	if err != nil {
		fmt.Printf("ERROR: failed to ignite - %s\n", err.Error())
		goto oops
	}
	err = x.AddBuiltinPrims()
	if err != nil {
		fmt.Printf("ERROR: failed to add builtin primitives - %s\n", err.Error())
		goto oops
	}

	err = x.AttachCCIO(&rfh, param.input_file)
	if err != nil {
		fmt.Printf("ERROR: failed to attach input file handler - %s\n", err.Error())
		goto oops
	}

	err = x.AttachUDIO(&sfh, &pfh)
	if err != nil {
		fmt.Printf("ERROR: failed to attach the user I/O handler - %s\n", err.Error())
		goto oops
	}

	err = x.BeginFeed()
	if err != nil {
		fmt.Printf("ERROR: %s\n", err.Error())
		goto oops
	}

	err = x.FeedFromFile(param.input_file)
	//err = x.FeedString(`(printf ">>>>>>>>> [%d]\n" (+ 30 455))
	//   (printf ">>>>>>>>> [%d]\n" (+ 11 455))
	//   #include "a.hak"
	//   (printf ">>>>>>>>> [%d]\n" (+ 20 455))`)
	if err != nil {
		fmt.Printf("ERROR: %s\n", err.Error())
		goto oops
	}

	err = x.EndFeed()
	if err != nil {
		fmt.Printf("ERROR: %s\n", err.Error())
		goto oops
	}

	x.Decode()
	x.SetLogMask(0)

	err = x.Execute()
	if err != nil {
		//fmt.Printf("ERROR: %s[%d:%d] - %s\n", herr.File, herr.Line, herr.Colm, herr.Msg)
		fmt.Printf("ERROR: %s\n", err.Error())
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
