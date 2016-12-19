/*
 * options.i --
 *
 * Parsing of command line options in Yorick.
 *
 *-----------------------------------------------------------------------------
 *
 * This file is part of YLib available at <https://github.com/emmt/ylib> and
 * licensed under the MIT "Expat" License.
 *
 * Copyright (C) 2009-2016, Éric Thiébaut.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 *-----------------------------------------------------------------------------
 */

local opt_init;
local OPT_FLAG, OPT_INTEGER, OPT_REAL, OPT_STRING, OPT_HELP, OPT_VERSION;
func opt_init(usage, brief, ops)
/* DOCUMENT tab = opt_init(usage, brief, ops)

   Build table for fast parsing of command line options.  USAGE is the syntax
   of the command, BRIEF is a short description of the command.  OPS is a
   ops of accepted options.  The items in OPS are lists of 5 items:

     ops = _lst(_lst(name1, defval1, units1, type1, descr1),
                _lst(name2, defval2, units2, type2, descr2), ...);

   with NAMEi the option name, DEFVALi the default value of the option, UNITSi
   the name of units or type of the option, TYPEi the identifier of the option
   type (one of: OPT_FLAG, OPT_INTEGER, OPT_REAL, OPT_STRING, OPT_HELP, or
   OPT_VERSION) and DESCRi the description of the option.

   The options will be used as (with one or two leading dashes):

     --NAMEi              for an OPT_FLAG, OPT_HELP, or OPT_VERSION option
     --NAMEi=VALUE        for an OPT_INTEGER, OPT_REAL, or OPT_STRING option

   There should be at most one OPT_HELP and one OPT_VERSION options.

   OPT_VERSION: If the option is set on the command line, the version will be
   printed.  The default value is the version string, the units are ignored.

   OPT_HELP: If the option is set on the command line, a help message will be
   printed with a list of all options.  The default value and the units are
   ignored.

   OPT_FLAG: If the option is set on the command line, the value of the option
   will be true (1n); otherwise, the value is false (0n).  Given default value
   and units are ignored.


   For instance:

     NULL = [];
     ops = _lst(_lst("input", NULL, "FILE", OPT_STRING, "name of input file"),
                _lst("safe", NULL, NULL, OPT_FLAG, "use safe mode"),
                _lst("level", 2, "INTEGER", OPT_INTEGER,
                     "level of operation"),
                _lst("scale", 0.3, "FACTOR", OPT_REAL,
                     "output scaling factor"),
                _lst("help", NULL, NULL, OPT_HELP, "print this help"),
                _lst("version", "2.1.4", NULL, OPT_VERSION,
                     "print option number"));


   SEE ALSO: opt_parse.
 */
{
  n = _len(ops);
  if (n < 1) opt_error, "empty option list";
  options = array(string, n);
  tab = h_new(":options", options, ":usage", usage, ":brief", brief);
  k = 0;
  while (ops) {
    item = _nxt(ops);
    if (_len(item) != 5) {
      opt_error, swrite(format="syntax error in option list (%d)", k+1);
    }
    name  = _car(item, 1);
    value = _car(item, 2);
    units = _car(item, 3);
    type  = _car(item, 4);
    descr = _car(item, 5);
    if (strglob("*[: ]*", name)) {
      opt_error, swrite(format="bad option name \"%s\" in option list (%d)",
                    name, k+1);
    }
    options(++k) = name;
    h_set, tab,
      name + ":defval", value,
      name + ":units", units,
      name + ":type", type,
      name + ":descr", descr;
  }
  return tab;
}

OPT_FLAG = 0;
OPT_INTEGER = 1;
OPT_REAL = 2;
OPT_STRING = 3;
OPT_HELP = 4;
OPT_VERSION = 5;
OPT_LIST = 16;
OPT_INTEGER_LIST = (OPT_INTEGER | OPT_LIST);
OPT_REAL_LIST    = (OPT_REAL    | OPT_LIST);
OPT_STRING_LIST  = (OPT_STRING  | OPT_LIST);

func opt_parse(tab, &argv)
/* DOCUMENT opt = opt_parse(tab, argv);

     Parse command line options from array of string ARGV and according to
     compiled rules in TAB.  If one of the special "help" or "version" options
     appears in ARGV, then an empty result is returned.  Otherwise, the result
     is a hash-table with members properly set accounting for default values
     in TAB and to given values in ARGV.  Before return, ARGV is set with the
     remaining unprocessed options.

     The returned result is a hash table which can be used as opt("NAME") or
     opt.NAME to get the value of option NAME.


   SEE ALSO: opt_init, get_argv.
 */
{
  /* Get option table. */
  local options;
  eq_nocopy, options, tab(":options");

  /* Parse command line arguments. */
  local value;
  opt = h_new();
  nil = string();
  argc = numberof(argv);
  for (k = 1; k <= argc; ++k) {
    arg = argv(k);
    if (strpart(arg, 1:1) != "-") {
      argv = argv(k:);
      break;
    }
    if (strpart(arg, 2:2) == "-") {
      if (arg == "--") {
        argv = (++k < argc ? argv(k:) : []);
        break;
      }
      off = 2;
    } else {
      off = 1;
    }
    sel = strfind("=", arg, off);
    if (sel(2) >= 0) {
      name = strpart(arg, off+1:sel(1));
      value = strpart(arg, sel(1)+2:);
    } else {
      name = strpart(arg, off+1:);
      value = [];
    }
    type = tab(name + ":type");
    units = tab(name + ":units");
    if (is_void(type)) {
      opt_error, "unknown option: " + arg;
    }
    if (h_has(opt, name)) {
      opt_error, "duplicate option: " + arg;
    }
    if (is_void(value)) {
      if (type == OPT_FLAG) {
        value = 1n;
      } else if (type == OPT_HELP) {
        write, format="%s\n", tab(":usage");
        write, format="%s\n", tab(":brief");
        n = numberof(options);
        col1 = array(string, n);
        col2 = array(string, n);
        col3 = array(string, n);
        for (k = 1; k <= n; ++k) {
          name = options(k);
          value = opt(name);
          type = tab(name + ":type");
          units = tab(name + ":units");
          str1 = "-" + name;
          if (! is_void(units)) {
            str1 += "=" + tab(name + ":units");
          }
          str2 = tab(name + ":descr");
          if (! is_void(value) && type != OPT_VERSION) {
           str2 += " (default " + print(value) + ")";
          }
          col1(k) = str1;
          col2(k) = str2;
        }
        len = max(strlen(col1));
        fmt = swrite(format = "  %%-%ds  %%s\n", len);
        write, format=fmt, col1, col2, linesize=200;
        return;
      } else if (type == OPT_VERSION) {
        write, format="version: %s\n", opt(name);
        return;
      } else {
        opt_error, "option \"" + name + "\" takes a value";
      }
    } else {
      if (type == OPT_INTEGER) {
        temp = 0L;
        dummy = nil;
        if (sread(value, temp, dummy) != 1) {
          opt_error, "expecting integer value for option \"" + name + "\"";
        }
        value = temp;
      } else if (type == OPT_REAL) {
        temp = 0.0;
        dummy = nil;
        if (sread(value, temp, dummy) != 1) {
          opt_error, ("expecting floating-point value for option \""
                      + name + "\"");
        }
        value = temp;
      } else if (type == OPT_STRING) {
        if (strlen(value) <= 0) {
          value = nil;
        }
      } else if (type == OPT_INTEGER_LIST) {
        list = _opt_split_list(value);
        numb = numberof(list);
        vect = array(long, numb);
        temp = 0L;
        dummy = nil;
        for (i = 1; i <= numb; ++i) {
          if (sread(list(i), temp, dummy) != 1) {
            opt_error, "expecting list of integers for option \"" + name + "\"";
          }
          vect(i) = temp;
        }
        eq_nocopy, value, (numb == 1 ? vect(1) : vect);
      } else if (type == OPT_REAL_LIST) {
        list = _opt_split_list(value);
        numb = numberof(list);
        vect = array(double, numb);
        temp = 0.0;
        dummy = nil;
        for (i = 1; i <= numb; ++i) {
          if (sread(list(i), temp, dummy) != 1) {
            opt_error, "expecting list of reals for option \"" + name + "\"";
          }
          vect(i) = temp;
        }
        eq_nocopy, value, (numb == 1 ? vect(1) : vect);
      } else if (type == OPT_STRING_LIST) {
        value = _opt_split_list(value);
        if (numberof(value) == 1) {
          value = value(1);
        }
      } else {
        opt_error, "option \"" + name + "\" takes no value";
      }
    }
    h_set, opt, name, value;
  }

  /* Set defaults. */
  for (k = numberof(options); k >= 1; --k) {
    name = options(k);
    if (! h_has(opt, name)) {
      h_set, opt, name, tab(name + ":defval");
    }
  }
  return opt;
}

func _opt_split_list(arg, sep)
{
  arg = strtrim(arg, 3);
  len = strlen(arg);
  if (len < 1) {
    return arg;
  }
  if (is_void(sep)) {
    sep = " *, *";
  }
  sel = strgrep(sep, arg, n=len);
  i = where(sel < 0);
  if (is_array(i)) {
    i = i(1) - 2;
    if (i < 2) {
      return arg;
    }
    sel = sel(1:i);
  }
  n = numberof(sel)/2;
  res = array(string, n + 1);
  for (k = 0; k <= n; ++k) {
    i = (k < 1 ? 1 : sel(2*k) + 1);
    j = (k < n ? sel(2*k + 1) : len);
    res(k+1) = (i <= j ? strtrim(strpart(arg, i:j)) : "");
  }
  return res;
}

func opt_usage(tab, msg)
{
  if (msg && strlen(msg)) {
    msg += "\n" + tab(":usage");
  } else {
    msg = tab(":usage");
  }
  if (batch()) {
    write, format="%s\n", msg;
    quit;
  }
}

local opt_error;
/* DOCUMENT opt_error, msg;

     If in batch mode, this subroutine print the error message and exit
     Yorick.  Otherwise, this subroutine is just an alias to the error
     subroutine of Yorick.  Note that batch mode is detected when the source
     is compiled par Yorick parser *not* at runtime when the subroutine is
     called.

   SEE ALSO: error, batch, quit.
 */
func opt_error(msg)
{
  write, format="%s\n", msg;
  quit;
}
if (! batch()) {
  opt_error = error;
}
