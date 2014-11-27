/*
 * rgb.i --
 *
 * Deal with X11 color database in Yorick.
 *
 * ----------------------------------------------------------------------------
 *
 * This file is part of YLib (Yorick Library) which is licensed under the MIT
 * "Expat" License:
 *
 * Copyright (C) 1995-2014, Éric Thiébaut.
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
 * ----------------------------------------------------------------------------
 */

_RGB_VERSION = "$Date$";

func rgb_load(nil)
/* DOCUMENT rgb_load;
       -or- db = rgb_load();
     Loads RGB  color database (from  X11 distribution).  When called  as a
     subroutine, the  external symbol "rgb" get defined.   The database can
     be used as follows:

        plg, y, x, color=rgb.light_goldenrod
        plg, y, x, color=rgb.dark_slate_grey

     The  color names are  all lower  case with  un underscore  to separate
     words.  If  you prefer  using global names  for _all_ RBG  colors then
     just include "rgb1.i" (then the color names are prefixed with "rgb_"):

        include, "~/yorick/rgb1.i";
        plg, y, x, color=rgb_goldenrod;

   SEE ALSO: rgb_build_databases. */
{
  if (is_func(h_new) == 2) {
    /* use hash table */
    include, "~/yorick/rgb3.i", 1;
    db = _rgb_hash();
  } else {
    /* use structure */
    include, "~/yorick/rgb2.i", 1;
    db = _rgb_struct();
  }
  if (am_subroutine()) {
    extern rgb;
    eq_nocopy, rgb, db;
  } else {
    return db;
  }
}

func rgb_build_databases(file)
/* DOCUMENT rgb_build_databases
       -or- rgb_build_databases, file;
     Builds RGB database files "rgb.txt", "rgb1.i", "rgb2.i", and "rgb3.i"
     in directory "~/yorick" from X11 RGB database FILE (default
     "/usr/X11R6/lib/X11/rgb.txt").  Existing files get overwritten.

   SEE ALSO: rgb_load, rgb_uncapitalize. */
{
  dir = "~/yorick/";
  write, "This will overwrite files: rgb.txt, rgb1.i, rgb2.i, and rgb3.i in "+dir;
  s = string(0);
  read, prompt=" Are sure you want to continue? [y/n] ", s;
  if (s != "y" && s != "Y") return;

  /* Sort and clean-up RGB database. */
  if (is_void(file)) file = "/usr/X11R6/lib/X11/rgb.txt";
  if (structof(file) == string) file = open(file);
  shortname = "rgb.txt";
  longname = dir + shortname;
  write, format=" Writing \"%s\"...\n", longname;
  if (open(longname, "r" , 1)) remove, longname;
  output = popen("sort -u -b -k 4 > ${HOME}/yorick/rgb.txt", 1);
  r = g = b = 0;
  s1 = s2 = s3 = s4 = s5 = string(0);
  number = 0;
  while ((line = rdline(file))) {
    n = sread(line, format="%d%d%d%s%s%s%s%s", r, g, b, s1, s2, s3, s4, s5);
    if (n < 4) continue;
    name = rgb_uncapitalize(s1);
    if (n >= 5) name += "_" + rgb_uncapitalize(s2);
    if (n >= 6) name += "_" + rgb_uncapitalize(s3);
    if (n >= 7) name += "_" + rgb_uncapitalize(s4);
    if (n >= 8) name += "_" + rgb_uncapitalize(s5);
    write, output, format="%3d %3d %3d %s\n", r, g, b, name;
    ++number;
    //write, output, format="_%-22s = [%3dn,%3dn,%3dn];\n", name, r, g, b;
  }
  close, output; /* other wise rgb.txt may be empty or incomplete */

  /* Re-parses clean database and creates the Yorick files. */
  file = open("~/yorick/rgb.txt");
  names = array(string, number);
  rgb = array(char, 3, number);
  r = g = b = 0;
  name = string(0);
  number = 0;
  while ((line = rdline(file))) {
    if (sread(line, format="%d%d%d%s", r, g, b, name) == 4) {
      ++number;
      rgb(1, number) = r;
      rgb(2, number) = g;
      rgb(3, number) = b;
      names(number) = name;
    }
  }
  close, file;
  write, format=" Found %d colors in new database.\n", number;

  /* Write the Yorick files. */

  shortname = "rgb1.i";
  longname = dir + shortname;
  write, format=" Writing \"%s\"...\n", longname;
  file = open(longname, "w");
  write, file, format="/* %s\n * %s\n */\n",
    "rgb1.i - Color database with global names (automatically build",
    "         by rgb_parse in rgb.i).";
  for (i=1 ; i<=number ; ++i) {
    write, file, format="rgb_%-22s = [%3dn,%3dn,%3dn];\n",
      names(i), rgb(1, i), rgb(2, i), rgb(3, i);
  }
  write, file, format="/* end of %s */", shortname;
  close, file;


  shortname = "rgb2.i";
  longname = dir + shortname;
  write, format=" Writing \"%s\"...\n", longname;
  file = open(longname, "w");
  write, file, format="/* %s\n * %s\n */\n",
    "rgb2.i - Color database using Yorick structure (automatically build",
    "         by rgb_parse in rgb.i).";
  write, file, format="\n%s\n%s\n",
    "/* Definition of RGB structure. */",
    "struct _RGB_STRUCT {";
  write, file, format="  char %s(3);\n", names(1:number);
  write, file, format="%s\n\n%s\n%s\n%s\n%s\n",
    "}",
    "func _rgb_struct(nil)",
    "/* DOCUMENT _rgb_struct() - Returns instanciated RGB structure. */",
    "{",
    "  return _RGB_STRUCT(";
  for (i=1 ; i<=number ; ++i) {
    write, file, format="    %-22s = [%3d,%3d,%3d]%s\n",
      names(i), rgb(1, i), rgb(2, i), rgb(3, i),
      (i<number ? "," : ");");
  }
  write, file, format="}\n/* end of %s */", shortname;
  close, file;


  shortname = "rgb3.i";
  longname = dir + shortname;
  write, format=" Writing \"%s\"...\n", longname;
  file = open(longname, "w");
  write, file, format="/* %s\n * %s\n */\n",
    "rgb3.i - Color database using Yeti hash table (automatically build",
    "         by rgb_parse in rgb.i).";
  write, file, format="\n%s\n%s\n%s\n%s\n",
    "func _rgb_hash(nil)",
    "/* DOCUMENT _rgb_hash() - Returns instanciated RGB hash table. */",
    "{",
    "  return h_new(";
  for (i=1 ; i<=number ; ++i) {
    write, file, format="    %-22s = [%3d,%3d,%3d]%s\n",
      names(i), rgb(1, i), rgb(2, i), rgb(3, i),
      (i<number ? "," : ");");
  }
  write, file, format="}\n/* end of %s */", shortname;
  close, file;

}

func rgb_uncapitalize(s)
/* DOCUMENT rgb_uncapitalize(s)
     Returns uncapitalizeed version of array of strings S:

       "dark slate blue"  ->  "dark_slate_blue"
       "DarkSlateBlue"    ->  "dark_slate_blue"

   SEE ALSO: rgb_build_databases. */
{
  (lower = char(indgen(0:255)))(1+'A':1+'Z') = lower(1+'a':1+'z');
  n = numberof((r = array(string, dimsof(s))));
  for (i=1; i<=n; ++i) {
    w1 = *pointer(s(i));
    len = numberof(w1);
    if (len) {
      w2 = array(char, 2*len-1);
      j2 = 0;
      for (j1=1 ; j1<len ; ++j1) {
        if ((c = w1(j1)) >= 'A' && c <= 'Z') {
          c = lower(1 + c);
          if (j1 != 1) w2(++j2) = '_';
        }
        w2(++j2) = c;
      }
      r(i) = string(&w2);
    }
  }
  return r;
}

/*
 * Local Variables:
 * mode: Yorick
 * tab-width: 8
 * indent-tabs-mode: nil
 * c-basic-offset: 2
 * fill-column: 79
 * coding: utf-8
 * ispell-local-dictionary: "american"
 * End:
 */
