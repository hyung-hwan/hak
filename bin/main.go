package main

import (
	"flag"
	"fmt"
	"hak"
	"io"
	"os"
	"strings"
)

/*
		var src []uint16 = utf16.Encode(([]rune)(
			`(printf ">>>>>>>>> [%d]\n" (+ 30 455))

(printf ">>>>>>>>> [%d]\n" (+ 11 455))
#include "a.hak"
(printf ">>>>>>>>> [%d]\n" (+ 20 455))
`))
*/

/* 0 means no pre-allocated heap, as in bin/hak.c */
const DEFAULT_HEAPSIZE uint = 0

/* to be set in build time */
var BINDIR = "."
var SBINDIR = "."
var LIBDIR = "."
var SYSCONFDIR = "."

type Param struct {
	log_target string
	log_mask   hak.BitMask
	input_file string
	heapsize   uint
	modlibdirs string
	incdirs    string
	verbose    bool
	show_info  bool
	fs_usage   func()
}

func empty_usage() {

}

/* the filter names accepted after the comma in --log, mirroring the xtab
 * table in handle_logopt() of bin/hak.c. 'and' marks the entries that clear
 * bits instead of setting them. */
type log_filter struct {
	name string
	and  bool
	mask hak.BitMask
}

var log_filters = []log_filter{
	{"", false, 0},

	{"app", false, hak.LOG_APP},
	{"compiler", false, hak.LOG_COMPILER},
	{"vm", false, hak.LOG_VM},
	{"mnemonic", false, hak.LOG_MNEMONIC},
	{"gc", false, hak.LOG_GC},
	{"ic", false, hak.LOG_IC},
	{"primitive", false, hak.LOG_PRIMITIVE},

	/* a specific level */
	{"fatal", false, hak.LOG_FATAL},
	{"error", false, hak.LOG_ERROR},
	{"warn", false, hak.LOG_WARN},
	{"info", false, hak.LOG_INFO},
	{"debug", false, hak.LOG_DEBUG},

	/* a specific level or higher */
	{"fatal+", false, hak.LOG_FATAL},
	{"error+", false, hak.LOG_FATAL | hak.LOG_ERROR},
	{"warn+", false, hak.LOG_FATAL | hak.LOG_ERROR | hak.LOG_WARN},
	{"info+", false, hak.LOG_FATAL | hak.LOG_ERROR | hak.LOG_WARN | hak.LOG_INFO},
	{"debug+", false, hak.LOG_FATAL | hak.LOG_ERROR | hak.LOG_WARN | hak.LOG_INFO | hak.LOG_DEBUG},

	/* a specific level or lower */
	{"fatal-", false, hak.LOG_FATAL | hak.LOG_ERROR | hak.LOG_WARN | hak.LOG_INFO | hak.LOG_DEBUG},
	{"error-", false, hak.LOG_ERROR | hak.LOG_WARN | hak.LOG_INFO | hak.LOG_DEBUG},
	{"warn-", false, hak.LOG_WARN | hak.LOG_INFO | hak.LOG_DEBUG},
	{"info-", false, hak.LOG_INFO | hak.LOG_DEBUG},
	{"debug-", false, hak.LOG_DEBUG},

	/* exclude a specific level */
	{"-fatal", true, ^hak.LOG_FATAL},
	{"-error", true, ^hak.LOG_ERROR},
	{"-warn", true, ^hak.LOG_WARN},
	{"-info", true, ^hak.LOG_INFO},
	{"-debug", true, ^hak.LOG_DEBUG},
}

/* split "path,filter,filter" into a target and a log mask. with no comma the
 * whole string is the target and every level and type is enabled. */
func parse_logopt(logstr string) (string, hak.BitMask, error) {
	var comma int = strings.Index(logstr, ",")
	var mask hak.BitMask

	if comma < 0 {
		return logstr, hak.LOG_ALL_LEVELS | hak.LOG_ALL_TYPES, nil
	}

	for _, f := range strings.Split(logstr[comma+1:], ",") {
		var i int
		for i = 0; i < len(log_filters); i++ {
			if log_filters[i].name == f {
				if log_filters[i].and {
					mask &= log_filters[i].mask
				} else {
					mask |= log_filters[i].mask
				}
				break
			}
		}
		if i >= len(log_filters) {
			return "", 0, fmt.Errorf("unrecognized log filter - %s - in %s", f, logstr)
		}
	}

	/* nothing selected in a category means everything in that category */
	if (mask & hak.LOG_ALL_TYPES) == 0 {
		mask |= hak.LOG_ALL_TYPES
	}
	if (mask & hak.LOG_ALL_LEVELS) == 0 {
		mask |= hak.LOG_ALL_LEVELS
	}

	return logstr[0:comma], mask, nil
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
	var incdirs *string
	var log *string
	var verbose *bool
	var show_info *bool
	var err error

	fs = flag.NewFlagSet(os.Args[0], flag.ContinueOnError)

	/* the same set bin/hak.c accepts. the flag package treats -x and --x
	 * alike, so registering both spellings of a short option is enough to
	 * give -I and --incdirs the same destination. */
	heapsize = fs.Uint("heapsize", DEFAULT_HEAPSIZE, "specify the heap size in bytes")
	incdirs = fs.String("incdirs", "", "specify the list of include directories")
	fs.StringVar(incdirs, "I", "", "specify the list of include directories")
	log = fs.String("log", "", "specify the log file path and options")
	fs.StringVar(log, "l", "", "specify the log file path and options")
	modlibdirs = fs.String("modlibdirs", "", "specify directories to load modules from")
	verbose = fs.Bool("v", false, "show verbose messages")
	show_info = fs.Bool("info", false, "show build information")

	param.fs_usage = fs.Usage
	fs.Usage = empty_usage  // i don't want fs.Parse() print the usage
	fs.SetOutput(io.Discard) // nor its own copy of the error, which we report ourselves
	err = fs.Parse(os.Args[1:])
	fs.Usage = param.fs_usage // restore it
	fs.SetOutput(os.Stderr)   // so the restored usage still prints
	if err != nil {
		return fmt.Errorf("command line error - %s", err.Error())
	}

	param.heapsize = *heapsize
	param.incdirs = *incdirs
	param.modlibdirs = *modlibdirs
	param.verbose = *verbose
	param.show_info = *show_info

	if *log != "" {
		param.log_target, param.log_mask, err = parse_logopt(*log)
		if err != nil {
			return err
		}
	}

	/* --info answers on its own and needs no script */
	if param.show_info {
		return nil
	}

	if fs.NArg() < 1 {
		return fmt.Errorf("no input file specified")
	} else if fs.NArg() > 1 {
		/* bin/hak.c also takes an optional output file as the second
		 * argument, but the go binding attaches the user data streams
		 * through handler objects rather than a path, so there is nothing
		 * to pass it to yet. */
		return fmt.Errorf("too many input files specified")
	}

	param.input_file = fs.Arg(0)
	return nil
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

	if param.show_info {
		fmt.Println(hak.BuildInfo())
		os.Exit(0)
	}

	x, err = hak.New()
	if err != nil {
		fmt.Printf("ERROR: failed to instantiate hak - %s\n", err.Error())
		os.Exit(1)
	}

	if param.log_target != "" {
		/* honour both halves of --log. the previous code discarded the path
		 * and always logged everything to /dev/stderr. */
		x.SetLogMask(param.log_mask)
		err = x.SetLogTarget(param.log_target)
		if err != nil {
			fmt.Printf("ERROR: failed to set log target - %s\n", err.Error())
			os.Exit(1)
		}
	}

	if param.incdirs != "" {
		x.SetIncDirs(param.incdirs)
	}

	if param.modlibdirs != "" {
		x.SetModLibDirs(param.modlibdirs)
	}

	x.SetTrait(x.GetTrait() | hak.TRAIT_LANG_ENABLE_EOL)

	err = x.Ignite(uintptr(param.heapsize))
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

	/* Decode() writes the bytecode mnemonics through the log, so it only
	 * produces anything when --log selects the mnemonic type. the log mask
	 * is no longer cleared afterwards - doing that silenced --log for the
	 * whole of Execute(). */
	x.Decode()

	err = x.Execute()
	if err != nil {
		//fmt.Printf("ERROR: %s[%d:%d] - %s\n", herr.File, herr.Line, herr.Colm, herr.Msg)
		fmt.Printf("ERROR: %s\n", err.Error())
		goto oops
	}

	if param.verbose {
		fmt.Printf("EXECUTION OK - %s\n", param.input_file)
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
