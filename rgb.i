/*
 * rgb.i --
 *
 * Deal with X11 color database in Yorick.
 *
 * ---------------------------------------------------------------------------
 *
 * This file is part of YLib available at <https://github.com/emmt/ylib> and
 * licensed under the MIT "Expat" License.
 *
 * Copyright (C) 1995-2022, Éric Thiébaut.
 *
 * ----------------------------------------------------------------------------
 */

local rgb;
func rgb_load(nil)
/* DOCUMENT rgb_load;
         or rgb = rgb_load();

     loads RGB color database (from X11 distribution).  When called as a
     subroutine, the external symbol "rgb" get defined.  The database can be
     used as follows:

        plg, y, x, color=rgb.light_goldenrod
        plg, y, x, color=rgb.dark_slate_grey

     The color names are all lower case with un underscore to separate words.
     If you prefer using global names for _all_ RBG colors then just include
     "rgb1.i" (then the color names are global variables prefixed with "rgb_"):

        include, "rgb1.i";
        plg, y, x, color=rgb_goldenrod;

   SEE ALSO: rgb_build_databases. */
{
    if (is_func(h_new)) {
        /* use hash table */
        include, "rgb3.i", 1;
        db = _rgb_hash();
    } else {
        /* use structure */
        include, "rgb2.i", 1;
        db = _rgb_struct();
    }
    if (am_subroutine()) {
        extern rgb;
        eq_nocopy, rgb, db;
    } else {
        return db;
    }
}

func rgb_build_databases(file, dir)
/* DOCUMENT rgb_build_databases[, file[, dir]];

     builds RGB database files "rgb1.i", "rgb1.i", "rgb2.i", and
     "rgb3.i" from X11 RGB database `file` (default
     "/usr/share/X11/rgb.txt") and save them in directory `dir` (the
     current working directory by default).  Existing files get overwritten.

   SEE ALSO: rgb_load, rgb_uncapitalize. */
{
    if (is_void(dir)) dir = "./";
    write, (
        "This will overwrite files: `rgb1.i`, `rgb2.i`, and `rgb3.i` in directory `"
        + dir + "`");
    if (!strglob("*/", dir)) dir += "/";
    s = string(0);
    read, prompt=" Are sure you want to continue? [y/n] ", s;
    if (s != "y" && s != "Y") return;

    /* Extract RGB database. */
    if (is_void(file)) file = "/usr/share/X11/rgb.txt";
    if (structof(file) == string) file = open(file);
    db = save();
    r = g = b = 0;
    s1 = s2 = s3 = s4 = s5 = string(0);
    while ((line = rdline(file))) {
        n = sread(line, format="%d%d%d%s%s%s%s%s", r, g, b, s1, s2, s3, s4, s5);
        if (n < 4) continue;
        name = rgb_uncapitalize(s1);
        if (n >= 5) name += "_" + rgb_uncapitalize(s2);
        if (n >= 6) name += "_" + rgb_uncapitalize(s3);
        if (n >= 7) name += "_" + rgb_uncapitalize(s4);
        if (n >= 8) name += "_" + rgb_uncapitalize(s5);
        save, db, noop(name), [r,g,b];
    }
    close, output; /* otherwise rgb.tmp may be empty or incomplete */

    /* Sort color names. */
    colors = db(*,);
    ncolors = numberof(colors);
    if (ncolors > 1) {
        colors = colors(sort(colors));
    }
    write, format=" Found %d unique colors in database.\n", ncolors;

    /* Write the Yorick files. */
    for (pass = 1; pass <= 3; ++pass) {
        shortname = swrite(format="rgb%d.i", pass);
        longname = dir + shortname;
        write, format=" Writing \"%s\"...\n", longname;
        file = open(longname, "w");
        write, file, format="// %s -\n//\n// Color database %s.\n//\n// %s %s.\n\n",
            shortname,
            (pass == 1 ? "with global names" : (
                pass == 2 ? "using Yorick structure" : "using Yeti hash table")),
            "This file has been automatically build by",
            "`rgb_build_databases` in `rgb.i`";
        if (pass == 1) {
            for (i = 1; i <= ncolors; ++i) {
                name = colors(i);
                vals = db(noop(name));
                write, file, format="rgb_%-22s = [%3dn,%3dn,%3dn];\n",
                    name, vals(1), vals(2), vals(3);
            }
        } else if (pass == 2) {
            write, file, format="%s\n%s\n",
                "// Definition of RGB structure.",
                "struct _RGB_STRUCT {";
            write, file, format="  char %s(3);\n", colors;
            write, file, format="%s\n\n%s\n%s\n%s\n%s\n",
                "}",
                "func _rgb_struct(nil)",
                "/* DOCUMENT _rgb_struct() - Returns instanciated RGB structure. */",
                "{",
                "  return _RGB_STRUCT(";
            for (i = 1; i <= ncolors; ++i) {
                name = colors(i);
                vals = db(noop(name));
                write, file, format="    %-22s = [%3d,%3d,%3d]%s\n",
                    name, vals(1), vals(2), vals(3),
                    (i<ncolors ? "," : ");\n}");
            }
        } else {
            write, file, format="%s\n%s\n%s\n%s\n",
                "func _rgb_hash(nil)",
                "/* DOCUMENT _rgb_hash() - Returns instanciated RGB hash table. */",
                "{",
                "  return h_new(";
            for (i = 1; i <= ncolors; ++i) {
                name = colors(i);
                vals = db(noop(name));
                write, file, format="    %-22s = [%3d,%3d,%3d]%s\n",
                    name, vals(1), vals(2), vals(3),
                    (i < ncolors ? "," : ");\n}");
            }
        }
        write, file, format="\n// end of `%s`\n", shortname;
        close, file;
    }
}

func rgb_uncapitalize(s)
/* DOCUMENT u = rgb_uncapitalize(s);

     returns uncapitalizeed version of array of strings S:

       "dark slate blue"  ->  "dark_slate_blue"
       "DarkSlateBlue"    ->  "dark_slate_blue"

   SEE ALSO: rgb_build_databases. */
{
    (lower = char(indgen(0:255)))(1+'A':1+'Z') = lower(1+'a':1+'z');
    n = numberof((r = array(string, dimsof(s))));
    for (i = 1; i <= n; ++i) {
        w1 = *pointer(s(i));
        len = numberof(w1);
        if (len) {
            w2 = array(char, 2*len-1);
            j2 = 0;
            for (j1 = 1; j1 < len; ++j1) {
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
