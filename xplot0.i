/*
 * xplot0.i --
 *
 * Low-level routines to deal with Yorick graphical system.
 *
 *-----------------------------------------------------------------------------
 *
 * Copyright (C) 2014-2017, Éric Thiébaut <eric.thiebaut@univ-lyon1.fr>
 *
 * This file is free software; as a special exception the author gives
 * unlimited permission to copy and/or distribute it, with or without
 * modifications, as long as this notice is preserved.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY, to the extent permitted by law; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 *-----------------------------------------------------------------------------
 */

/*
 * Naming Conventions
 * ==================
 *
 * Defined in "xplot0.i":
 *
 *   p_name = function to manage graphical elements
 *   P_NAME = global variable related to graphical system
 *
 *
 * Defined in "xplot.i":
 *
 *   pl_name = function to draw/plot something
 *   PL_NAME = global variable for plotting
 */

/* Save builtin version of the plotting commands. */
if (is_func(limits)   == 2) _p_builtin_limits   = limits;
if (is_func(range)    == 2) _p_builtin_range    = range;
if (is_func(viewport) == 2) _p_builtin_viewport = viewport;
if (is_func(logxy)    == 2) _p_builtin_logxy    = logxy;
if (is_func(gridxy)   == 2) _p_builtin_gridxy   = gridxy;
if (is_func(plsys)    == 2) _p_builtin_plsys    = plsys;
if (is_func(window)   == 2) _p_builtin_window   = window;

/* Miscellaneous constants. */
P_TRUE = 1n;
P_FALSE = 0n;

P_ISO_8859_1_MIDDLE_DOT = '\267';
P_ISO_8859_1_PLUS_MINUS = '\261';
P_ISO_8859_1_MULTIPLY   = '\327';

func p_pow10_labels(labs, ndig=, mult=)
/* DOCUMENT str = p_pow10_labels(labs);

     Convert numerical labels LABS into strings using power of 10 notation.
     The returned strings can be drawn on a graphical window with `plt`.

   SEE ALSO plt.
*/
{
  if (is_void(ndig)) ndig = 4;
  if (is_void(mult)) mult = P_ISO_8859_1_MULTIPLY;
  if (structof(mult) == char) mult = strchar(mult);
  if (mult == "%") mult = "%%";
  a = abs(labs);
  j = where(a);
  if (is_array(j)) {
    t = long(floor(log10(a(j))));
    p = array(min(t), dimsof(labs));
    p(j) = t;
  } else {
    p = array(long, dimsof(labs));
  }
  f = labs*exp(-log(10)*p);
  format = swrite(format="%%%d.%df %s 10^%%d", ndig+2, ndig-1, mult);
  return swrite(format=format, f, p);
}

func p_query_dpi(win)
/* DOCUMENT dpi = p_query_dpi();
         or dpi = p_query_dpi(win);

     Yield the dots per pixel (DPI) of window WIN, or of the current window if
     WIN is unspecified.  Zero is returned if the window does not exist.

   SEE ALSO window_geometry.
*/
{
  g = window_geometry(win);
  return (is_void(g) ? 0.0 : g(1));
}

func p_query_text_style(axis, sys=, win=)
/* DOCUMENT style = p_query_text_style();
         or style = p_query_text_style(axis);

     Query the text style(s) of the plot system in the form of a
     `GpTextAttribs` structure(s).  If AXIS is unspecified, an array of two
     `GpTextAttribs` is returned with the horizontal and vertical axis styles.
     Otherwise, the style for the specified axis (1 or 2 for horizontal and
     vertical respectively, although Yorick indexing rules are used) is
     returned.

     Keyword SYS can be used to query the settings of a specific plotting
     system.  By default, the current plotting system is considered.

     Keyword WIN can be used to query the settings of a specific window.  By
     default, the current window is considered.  Upon return the current window
     is left unchanged.

   SEE ALSO get_style, current_window, plsys, GpTextAttribs.
*/
{
  if (is_void(win)) {
    old_win = -1;
  } else {
    old_win = current_window();
    window, win;
  }
  local landscape, systems, legends, clegends;
  get_style, landscape, systems, legends, clegends;
  if (is_void(sys)) {
    sys = plsys();
    if (sys == 0) {
      if (old_win >= 0) {
        window, old_win;
      }
      error, "you must specify the plotting system with keyword sys=";
    }
  }
  if (old_win >= 0) {
    window, old_win;
  }
  system = systems(sys);
  if (is_void(axis)) {
    return [system.ticks.horiz.textStyle,
            system.ticks.vert.textStyle];
  } else {
    if (axis < 0) {
      /* Mimic Yorick indexing rule. */
      axis += 2;
    }
    if (axis == 1) {
      return system.ticks.horiz.textStyle;
    } else if (axis == 2) {
      return system.ticks.vert.textStyle;
    } else {
      error, "invalid axis value (must be 1 or 2)";
    }
  }
}

/*---------------------------------------------------------------------------*/
/* PARSING GRAPHIC KEYWORDS */

local P_COURIER, P_TIMES, P_HELVETICA, P_SYMBOL, P_NEWCENTURY, P_SCHOOLBOOK;
local P_GUI_FONT, P_BOLD, P_ITALIC, P_OPAQUE;
local _P_FONT_TABLE, p_font;
/* DOCUMENT p_font(arg);
         or p_font(arg, def);
         or p_font, arg;
         or p_font, arg, def;

     Convert the font argument ARG (or the default value DEF if ARG is void and
     DEF is specified) into something understandable by Yorick graphic
     routines.  When called as a subroutine, ARG is redefined with the result.

     Some font constants are pre-defined as global (external) symbols:

         Symbol       Name           Font
         --------------------------------------------------
         P_COURIER    "courier"      Courier
         P_TIMES      "times"        Times
         P_HELVETICA  "helvetica"    Helvetica
         P_SYMBOL     "symbol"       Symbol
         P_NEWCENTURY "schoolbook"   New Century Schoolbook
         P_SCHOOLBOOK idem           idem
         P_GUI_FONT
         --------------------------------------------------

     There are also some font flags/suffixes:

         Symbol       Suffix         Description
         --------------------------------------------------
         P_BOLD        +"B"          boldface
         P_ITALIC      +"I"          italic
         P_OPAQUE                    opaque flag
         --------------------------------------------------

     For instance "timesB" is the same as (P_TIMES|P_ITALIC).

   SEE ALSO: font, p_color, p_type.
*/
func p_font(&arg, val)
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (! is_scalar(val)) {
  invalid:
    error, "invalid font attribute";
  }
  if (is_string(val)) {
    tmp = p_get_member(_P_FONT_TABLE, val);
    if (is_void(tmp)) {
      error, ("unrecognized font: \"" + val + "\"");
    }
    val = tmp;
  } else if (! is_integer(val)) {
    goto invalid;
  }
  if (am_subroutine()) {
    arg = long(val);
  } else {
    return long(val);
  }
}
P_COURIER    =  0;
P_TIMES      =  4;
P_HELVETICA  =  8;
P_SYMBOL     = 12;
P_NEWCENTURY = 16;
P_SCHOOLBOOK = P_NEWCENTURY;
P_GUI_FONT   = 20;
P_BOLD       =  1;
P_ITALIC     =  2;
P_OPAQUE     = 32;

local P_SOLID, P_DASH, P_DOT, P_DASHDOT, P_DASHDOTDOT;
local _P_TYPE_TABLE, p_type;
/* DOCUMENT val = p_type(arg);
         or val = p_type(arg, def);
         or p_type, arg;
         or p_type, arg, def;


   Line types.
   SEE ALSO: type, p_font, p_color */
func p_type(&arg, val)
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (! is_scalar(val)) {
  invalid:
    error, "invalid line type";
  }
  if (is_string(val)) {
    tmp = p_get_member(_P_TYPE_TABLE, val);
    if (is_void(tmp)) {
      error, ("unrecognized line type: \"" + val + "\"");
    }
    val = tmp;
  } else if (! is_integer(val)) {
    goto invalid;
  }
  if (am_subroutine()) {
    arg = val;
  } else {
    return val;
  }
}
P_SOLID      = 1;
P_DASH       = 2;
P_DOT        = 3;
P_DASHDOT    = 4;
P_DASHDOTDOT = 5;
_P_TYPE_TABLE = save("solid", P_SOLID, "dash", P_DASH, "dot", P_DOT,
                     "dashdot", P_DASHDOT, "dashdotdot", P_DASHDOTDOT);

local P_BG, P_FG, P_BLACK, P_WHITE, P_RED, P_GREEN, P_BLUE, P_CYAN, P_MAGENTA;
local P_YELLOW, P_GRAYD, P_GRAYC, P_GRAYB, P_GRAYA, P_XOR, P_EXTRA;
local p_color, p_icolor;
/* DOCUMENT p_color(arg);              or p_icolor(arg);
         or p_color(arg, def);         or p_icolor(arg, def);
         or p_color, arg;              or p_icolor, arg;
         or p_color, arg, def;         or p_icolor, arg, def;

     Parse the color argument ARG or (if ARG is void) the default value DEF and
     return something understandable by Yorick graphic routines.  When called
     as a subroutine, ARG is redefined with the parsed color.

     Recognized colors are: color constants (see table below) or indexed
     colors, RGB triplets (as 3-element char array), packed RGB values, HTML
     colors like "#RRGGBB" (with "RR", "GG" and "BB" the red, green and blue
     levels in hexadecimal), X11 colors as "firebrick" (case is irrelevant),
     and Gist colors as "magenta".  A packed RGB color is a 32-bit integer
     built as:

        0x01000000 | (blue << 16) | (green << 8) | red

     the function `p_rgb()` implements this.  An indexed color is either an
     integer in the range [0,255] or a real in the range [0,1].

     The difference between `p_color()` and `p_icolor()` is that the former
     returns either an indexed color or a RGB triplet and is suitable for
     plotting routines while the latter returns either an indexed color or a
     packed RGB color and is suitable for graphic style sheets.

     Some color constants are pre-defined as global (external) symbols:

         Symbol     Color            Symbol     Color
         ---------------------       -------------------------
         P_BG       background       P_MAGENTA  magenta
         P_FG       foreground       P_YELLOW   yellow
         P_BLACK    black            P_GRAYD    darkest gray
         P_WHITE    white            P_GRAYC    gray
         P_RED      red              P_GRAYB    gray
         P_GREEN    green            P_GRAY     lightest gray
         P_BLUE     blue             P_XOR      toggle drawing
         P_CYAN     cyan             P_EXTRA    special color
         ---------------------       -------------------------

   SEE ALSO: color, p_rgb, p_type, p_font, p_justify, p_anchor.
*/
func p_color(&arg, def)
{
  local p_parse_color_type;
  color = p_parse_color((is_void(arg) ? def : arg));
  if (p_parse_color_type == 2) {
    color = p_packed_color_as_rgb_triplet(color);
  }
  if (am_subroutine()) {
    arg = color;
  } else {
    return color;
  }
}
func p_icolor(&arg, def)
{
  local p_parse_color_type;
  color = p_parse_color((is_void(arg) ? def : arg));
  if (p_parse_color_type == 3) {
    color = p_rgb_triplet_as_packed_color(color);
  }
  if (am_subroutine()) {
    arg = color;
  } else {
    return color;
  }
}
P_BG      = 255;
P_FG      = 254;
P_BLACK   = 253;
P_WHITE   = 252;
P_RED     = 251;
P_GREEN   = 250;
P_BLUE    = 249;
P_CYAN    = 248;
P_MAGENTA = 247;
P_YELLOW  = 246;
P_GRAYD   = 245; /* darkest gray */
P_GRAYC   = 244;
P_GRAYB   = 243;
P_GRAYA   = 242; /* lightest gray */
P_XOR     = 241;
P_EXTRA   = 240;

_P_PREDEFINED_COLORS = [0x01ffffffn,  // EXTRA (as WHITE)
                        0x01000000n,  // XOR (as BLACK)
                        0x01d6d6d6n,  // GRAYA
                        0x01bebeben,  // GRAYB
                        0x01969696n,  // GRAYC
                        0x01646464n,  // GRAYD
                        0x0100ffffn,  // YELLOW
                        0x01ff00ffn,  // MAGENTA
                        0x01ffff00n,  // CYAN
                        0x01ff0000n,  // BLUE
                        0x0100ff00n,  // GREEN
                        0x010000ffn,  // RED
                        0x01ffffffn,  // WHITE
                        0x01000000n,  // BLACK
                        0x01000000n,  // FG (assume black)
                        0x01ffffffn]; // BG (assume white)

func p_rgb_colors(color, dims, default)
/* DOCUMENT p_rgb_colors(col, dims, def);

     Returns a 3-by-DIMS array of RGB color(s) initialized with the values of
     COL (or DEF if COL is void).

   SEE ALSO: p_parse_color, p_color.
 */
{
  if (is_void(color)) {
    eq_nocopy, color, default;
  }

  /* Parse the color(s) as RGB triplet(s). */
  local p_parse_color_type;
  if (is_scalar(color)) {
    color = p_parse_color(color);
    if (p_parse_color_type == 1) {
      color = p_indexed_color_as_rgb_triplet(color);
    } else if (p_parse_color_type == 2) {
      color = p_packed_color_as_rgb_triplet(color);
    }
    if (is_void(dims)) return color;
  } else if (structof(color) == char) {
    cdims = dimsof(color);
    if (numberof(cdims) < 2 || cdims(2) != 3) {
      error, "must be RGB triplet in this case";
    }
    if (is_void(dims) && numberof(cdims) == 2) return color;
  } else if (is_string(color) || is_integer(color)) {
    tmp = array(char, 3, dimsof(color));
    for (k = 1; k <= n; ++k) {
      col = p_parse_color(color(k));
      if (p_parse_color_type == 1) {
        col = p_indexed_color_as_rgb_triplet(col);
      } else if (p_parse_color_type == 2) {
        col = p_packed_color_as_rgb_triplet(col);
      }
      tmp(,k) = col;
    }
    eq_nocopy, color, tmp;
  } else {
    error, "bad color(s)";
  }

  /* Broadcast color(s) in a 3-by-NDIMS array. */
  rgb = array(char, 3, dims);
  dims = dimsof(rgb, color);
  ntot = 1;
  for (k = numberof(dims); k > 1; --k) {
    ntot *= dims(k);
  }
  if (numberof(dims) != numberof(dimsof(rgb)) || ntot != numberof(rgb)) {
    error, "bad number (or dimensions) of colors";
  }
  return rgb + color;
}

/*
 * Below is how Yorick/Play deals with color values:
 *
 *  #define P_IS_NDX(color) ((p_col_t)(color)<256UL)
 *  #define P_IS_RGB(color) ((p_col_t)(color)>=256UL)
 *  #define P_R(color) ((p_col_t)(color)&0xffUL)
 *  #define P_G(color) (((p_col_t)(color)>>8)&0xffUL)
 *  #define P_B(color) (((p_col_t)(color)>>16)&0xffUL)
 *  #define P_RGB(r,g,b) ((p_col_t)(r) | ((p_col_t)(g)<<8) | \
 *                        ((p_col_t)(b)<<16) | 0x01000000)
 */
func p_rgb(r,g,b)
/* DOCUMENT p_rgb(r,g,b);

     Combine red, green, blue triplet into  a packed color.  All values must be
     in the range 0:255 (this is not checked for efficiency reasons).

   SEE ALSO: p_color, p_parse_color.
 */
{
  return (int(r) | (int(g) << 8n) | (int(b) << 16n) | 0x01000000n);
}

local p_indexed_color_as_packed_color, p_indexed_color_as_rgb_triplet;
local p_rgb_triplet_as_packed_color, p_packed_color_as_rgb_triplet;
/* DOCUMENT pkd = p_indexed_color_as_packed_color(idx);
         or rgb = p_indexed_color_as_rgb_triplet(idx);
         or pkd = p_rgb_triplet_as_packed_color(rgb);
         or rgb = p_packed_color_as_rgb_triplet(rgb);

      The functions converts between different color representations.  PKD is a
      packed color (red, green and blue levels encoded in a 32-bit int), IDX is
      an indexed color (in the range 0:255 modulo 256), RGB is a triplet of
      red, green and blue levels.  The types of the returned values are
      strictly enforced: `int` for a packed color, `long` for an indexed color
      and `char` for an RBG triplet.  Theses functions are however more
      tolerant on the type of their inputs.

   SEE ALSO: p_color, p_parse_color, p_rgb.
 */
func p_indexed_color_as_packed_color(idx)
{
  idx &= 0xff;  /* indexed colors are taken modulo 256 */
  if (idx >= 240) {
    /* Predefined colors. */
    return _P_PREDEFINED_COLORS(idx - 239);
  } else {
    /* Palette color. */
    local r, g, b;
    palette, r, g, b, query=1n;
    if ((i = idx + 1) > numberof(r)) {
      error, "out of range indexed color (set the palette first)";
    }
    return ((r(i)&0xffn) | ((g(i)&0xffn) << 8n) | ((b(i)&0xffn) << 16n)
            | 0x01000000n);
  }
}

func p_indexed_color_as_rgb_triplet(idx)
{
  idx &= 0xff;  /* indexed colors are taken modulo 256 */
  if (idx >= 240) {
    /* Predefined colors. */
    val = _P_PREDEFINED_COLORS(idx - 239);
    return char([val&0xffn, (val>>8)&0xffn, (val>>16)&0xffn]);
  } else {
    /* Palette color. */
    local r, g, b;
    palette, r, g, b, query=1n;
    if ((i = idx + 1) > numberof(r)) {
      error, "out of range indexed color (set the palette first)";
    }
    return [r(i), g(i), b(i)];
  }
}

func p_rgb_triplet_as_packed_color(rgb)
{
  return ((rgb(1)&0xffn) | ((rgb(2)&0xffn) << 8n) | ((rgb(1)&0xffn) << 16n)
          | 0x01000000n);
}

func p_packed_color_as_rgb_triplet(p)
{
  return char([p&0xff, (p>>8)&0xff, (p>>16)&0xff]);
}

local _P_COLOR_TABLE, p_add_named_colors, p_add_named_color;
/* DOCUMENT p_add_named_color, name, value;
         or p_add_named_colors, names, values;

     These subroutines add new named color(s) to the global database.

   SEE ALSO p_parse_color.
*/

if (is_void(_P_COLOR_TABLE)) _P_COLOR_TABLE = save();

func p_add_named_color(name, value)
{
  save, _P_COLOR_TABLE, strcase(0n, name), value;
}

func p_add_named_colors(names, values)
{
  n = numberof(names);
  if (numberof(values) != n) {
    error, "there must be as many color values as names";
  }
  for (i = 1; i <= n; ++i) {
    p_add_named_color, names(i), values(i);
  }
}

local p_parse_color_type;
func p_parse_color(val)
/* DOCUMENT clr = p_parse_color(val);

     Retrieve a color in a form understandable by Yorick plotting routines or
     suitable for graphic style sheets.  See `p_color()` for the various forms
     of a valid color value.

     The global variable `p_parse_color_type` can be used to check the type of
     color which is returned: 1 for an indexed color, 2 for a packed RGB color
     and 3 for a triplet of RGB values.  Alternately, the data type of the
     result informs about the color representation: `int` for a packed color,
     `long` for an indexed color and `char` for an RBG triplet.

   SEE ALSO: p_color, color.
 */
{
  extern p_parse_color_type;

  /* If argument is a scalar string, search the color databases (using an OXY
     object as a hash table is about 20 times faster than strfind). */
  name = [];
  named = 0n;
  if (is_string(val) && is_scalar(val)) {
    col = p_get_member(_P_COLOR_TABLE, strcase(0n, val));
    if (! is_void(col)) {
      eq_nocopy, name, val;
      eq_nocopy, val, col;
      named = 1n;
    }
  }

  /* Parse the color value. */
  if (is_scalar(val)) {
    if (is_integer(val)) {
      val = long(val);
      if ((val & 0x01000000) == 0x01000000) {
        /* Packed RGB color. */
        p_parse_color_type = 2;
        return int(val);
      } else if (val < 256) {
        /* Indexed color. */
        p_parse_color_type = 1;
        return (val&0xff); /* indexed colors are taken modulo 256 */
      }
      error, swrite(format="unrecognized color: 0x%08x", val);
    } else if (is_string(val)) {
      /* Try to parse HTML-like value. */
      r = g = b = 0;
      len = strlen(val);
      if (len == 7 && sread(val, format="#%2x%2x%2x", r, g, b) == 3) {
        value = (int(r) | (int(g) << 8n) | (int(b) << 16n) | 0x01000000n);
      } else if (len == 4 && sread(val, format="#%1x%1x%1x", r, g, b) == 3) {
        value = ((int(r) <<  4n) |
                 (int(g) << 12n) |
                 (int(b) << 20n) | 0x01000000n);

      } else if (len == 13 && sread(val, format="#%4x%4x%4x", r, g, b) == 3) {
        value = (((int(r) >> 8n) & 0x0000ffn) |
                 ( int(g)        & 0x00ff00n) |
                 ((int(b) << 8n) & 0xff0000n) | 0x01000000n);
      } else {
        error, ("unrecognized color: \"" + val + "\"");
      }
      if (named) {
        /* Save value for further speedup. */
        p_add_named_color, name, value;
      }
      p_parse_color_type = 2;
      return value;
    } else if (is_real(val)) {
      /* Fractional indexed color: map [0,1] to [0,239]. */
      p_parse_color_type = 1;
      return lround(239.0*max(0.0, min(val, 1.0)));
    }
  } else if (structof(val) == char && numberof(val) == 3) {
    p_parse_color_type = 3;
    return val;
  }
  error, "color must be a string, an integer or a real";
}

local P_NORMAL, P_LEFT, P_CENTER, P_RIGHT;
local P_TOP, P_CAPLINE, P_HALF, P_BASELINE, P_BOTTOM;
local _P_JUSTIFY_TABLE, p_justify;
/* DOCUMENT p_justify(arg);
         or p_justify(arg, def);
         or p_justify, arg;
         or p_justify, arg, def;

     Convert the argument ARG (or the default value DEF if ARG is void and DEF
     is specified) into an integer value for the justify graphic keyword of
     Yorick graphic routines.  When called as a subroutine, ARG is redefined
     with the result.

     The input justify value (of ARG or of DEF) can be a string built from the
     concatenation of the horizontal alignment and the vertical alignment (in
     that order) or an integer equal to the sum of the horizontal and the
     vertical alignment values.  The tables below summarize the different
     possibilities.

       Horizontal alignment        Vertical alignment
       --------------------        --------------------
       P_NORMAL     0   "N"        P_NORMAL     0   "N"
       P_LEFT       1   "L"        P_TOP        4   "T"
       P_CENTER     2   "C"        P_CAPLINE    8   "C"
       P_RIGHT      3   "R"        P_HALF      12   "H"
       --------------------        P_BASELINE  16   "A"
                                   P_BOTTOM    20   "B"
                                   --------------------

   SEE ALSO: justify, p_anchor, p_color, p_type, p_symbol
*/
func p_justify(&arg, val)
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (is_scalar(val)) {
    if (is_string(val)) {
      val = p_get_member(_P_JUSTIFY_TABLE, val);
    }
    if (is_integer(val)) {
      if (am_subroutine()) {
        arg = long(val);
        return;
      } else {
        return long(val);
      }
    }
  }
  error, "unrecognized justify value";
}
P_NORMAL   =  0;
P_LEFT     =  1;
P_CENTER   =  2; // horizontal alignment/justification/anchor
P_RIGHT    =  3;
P_TOP      =  4;
P_CAPLINE  =  8;
P_HALF     = 12; // vertical alignment/justification/anchor
P_BASELINE = 16;
P_BOTTOM   = 20;


local P_NORTH, P_EAST, P_SOUTH, P_WEST;
local P_NORTHWEST, P_NORTHEAST, P_SOUTHWEST, P_SOUTHEAST;
local _P_ANCHOR_TABLE, p_anchor;
/* DOCUMENT p_anchor(arg);
         or p_anchor(arg, def);
         or p_anchor, arg;
         or p_anchor, arg, def;

     Convert the argument ARG (or the default value DEF if ARG is void and DEF
     is specified) into an integer value for the `anchor` graphic keyword.
     When called as a subroutine, ARG is redefined with the result.

     The input anchor value (of ARG or of DEF) can be a string or an integer.
     The table below summarizes the different possibilities and the predefined
     global symbols.

       Symbol         Expression          Value  String
       -----------------------------------------------------------
                      P_CENTER|P_HALF      14    "c"   "center"
       P_NORTH        P_CENTER|P_TOP        6    "n"   "north"
       P_EAST         P_RIGHT|P_HALF       15    "e"   "east"
       P_SOUTH        P_CENTER|P_BOTTOM    22    "s"   "south"
       P_WEST         P_LEFT|P_HALF        13    "w"   "west"
       P_NORTHWEST    P_LEFT|P_TOP          5    "nw"  "northwest"
       P_NORTHEAST    P_RIGHT|P_TOP         7    "ne"  "northeast"
       P_SOUTHWEST    P_LEFT|P_BOTTOM      21    "sw"  "southwest"
       P_SOUTHEAST    P_RIGHT|P_BOTTOM     23    "se"  "southeast"
       -----------------------------------------------------------

   SEE ALSO: p_justify, p_color, p_type, p_symbol.
 */
func p_anchor(&arg, val)
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (is_scalar(val)) {
    if (is_string(val)) {
      val = p_get_member(_P_ANCHOR_TABLE, val);
    }
    if (is_integer(val)) {
      v = (val & 3);
      h = (val & 28);
      val = (((v != 0) ? v : 2) | ((h == 4 || h == 12 || h == 20) ? h : 12));
      if (am_subroutine()) {
        arg = val;
        return;
      } else {
        return val;
      }
    }
  }
  error, "unrecognized anchor value";
}
P_NORTH     = P_CENTER + P_TOP;
P_EAST      = P_RIGHT  + P_HALF;
P_SOUTH     = P_CENTER + P_BOTTOM;
P_WEST      = P_LEFT   + P_HALF;
P_NORTHWEST = P_LEFT   + P_TOP;
P_NORTHEAST = P_RIGHT  + P_TOP;
P_SOUTHWEST = P_LEFT   + P_BOTTOM;
P_SOUTHEAST = P_RIGHT  + P_BOTTOM;
_P_ANCHOR_TABLE = save("c",  P_CENTER|P_HALF, "center",    P_CENTER|P_HALF,
                       "n",  P_NORTH,         "north",     P_NORTH,
                       "e",  P_EAST,          "east",      P_EAST,
                       "s",  P_SOUTH,         "south",     P_SOUTH,
                       "w",  P_WEST,          "west",      P_WEST,
                       "nw", P_NORTHWEST,     "northwest", P_NORTHWEST,
                       "ne", P_NORTHEAST,     "northeast", P_NORTHEAST,
                       "sw", P_SOUTHWEST,     "southwest", P_SOUTHWEST,
                       "se", P_SOUTHEAST,     "southeast", P_SOUTHEAST);

local _P_POSITION_TABLE, p_position;
/* DOCUMENT p_position(arg);
         or p_position(arg, def);
         or p_position, arg;
         or p_position, arg, def;

     Convert the argument ARG (or the default value DEF if ARG is void and DEF
     is specified) into an integer value for the `position` graphic keyword.
     When called as a subroutine, ARG is redefined with the result.

     The input position value (of ARG or of DEF) can be a string or an integer.
     The table below summarizes the different possibilities and the predefined
     global symbols.

       Symbol         Value  String
       ---------------------------------------
       P_LEFT           1    "left", "west"
       P_CENTER         2    "center"
       P_RIGHT          3    "right", "east"
       P_TOP            4    "top", "north"
       P_BOTTOM        20    "bottom", "south"
       ---------------------------------------

   SEE ALSO: p_justify, p_color, p_type, p_symbol.
 */
func p_position(&arg, def)
{
  if (is_void(arg)) {
    eq_nocopy, arg, def;
  }
  if (is_scalar(arg)) {
    if (is_string(arg)) {
      arg = p_get_member(_P_POSITION_TABLE, arg);
    }
    if (is_integer(arg)) {
      if (am_subroutine()) {
        return;
      } else {
        return arg;
      }
    }
  }
  error, "unrecognized position value";
}
_P_POSITION_TABLE = save("center",    P_CENTER,
                         "top",       P_TOP,     "north",     P_TOP,
                         "bottom",    P_BOTTOM,  "south",     P_BOTTOM,
                         "left",      P_LEFT,    "west",      P_LEFT,
                         "right",     P_RIGHT,   "east",      P_RIGHT);

local P_SQUARE, P_PLUS, P_TRIANGLE, P_UP_TRIANGLE, P_CIRCLE;
local P_DIAMOND, P_CROSS, P_DOWN_TRIANGLE, P_STAR;
local _P_SYMBOL_TABLE, p_symbol;
/* DOCUMENT p_symbol(arg);
         or p_symbol(arg, def);
         or p_symbol, arg;
         or p_symbol, arg, def;

     Convert the argument ARG (or the default value DEF if ARG is void and DEF
     is specified) into an integer value for the `symbol` graphic keyword.
     When called as a subroutine, ARG is redefined with the result.

     The input symbol value (of ARG or of DEF) can be a string or an integer.
     The table below summarizes the different possibilities:

         Value                          Description
         --------------------------------------------------------------------
          0                             nothing (just draw error bars if any)
          1  . "point"                  a point
          2  + "plus"                   a plus sign
          3    "asterisk"               an asterisk
          4  o "circle"                 a circle (actually an hexagon)
          5  x "cross"                  an "X" cross
          6  # "square"                 a square
          7  @ "diamond"                a square rotated by 45 degrees
          8  * "star"                   a star
          9  ^ "triangle" "triangleup"  a triangle
         10  v "triangledown"           an upside down triangle
         11  < "triangleleft"           a triangle oriented toward the left
         12  > "triangleright"          a triangle oriented toward the right
         -32 <= N < 0                   a polygon with -N sides
         --------------------------------------------------------------------

     The one-character symbol may given as lower/upper case and as a string or
     a char.  For instance, 'v', 'V', "v" and "V" all stand for an upside down
     triangle.

     For convenience, global variables P_POINT, P_PLUS, P_ASTERISK, P_CIRCLE,
     P_CROSS, P_SQUARE, P_DIAMOND, P_STAR, P_TRIANGLE, P_TRIANGLE_UP,
     P_TRIANGLE_DOWN, P_TRIANGLE_LEFT, and P_TRIANGLE_RIGHT are defined with
     the corresponding symbol code.

   SEE ALSO: pl_pnt, p_marker, p_color.
 */
func p_symbol(&arg, val)
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (is_scalar(val)) {
    s = structof(val);
    if (s == char && val > ' ') {
      val = strchar(val);
      s = string;
    }
    if (s == string) {
      if (strlen(val) == 0) {
        val = 0;
      } else {
        val = p_get_member(_P_SYMBOL_TABLE, val);
      }
    }
    if (is_integer(val) && -32 <= val && val <= 12) {
      if (am_subroutine()) {
        arg = long(val);
        return;
      } else {
        return long(val);
      }
    }
  }
  error, "unrecognized symbol value";
}
P_POINT          =  1; // marker '\1'
P_PLUS           =  2; // marker '\2'
P_ASTERISK       =  3; // marker '\3'
P_CIRCLE         =  4; // marker '\4'
P_CROSS          =  5; // marker '\5'
P_SQUARE         =  6;
P_DIAMOND        =  7;
P_STAR           =  8;
P_TRIANGLE       =  9;
P_TRIANGLE_UP    =  9;
P_TRIANGLE_DOWN  = 10;
P_TRIANGLE_LEFT  = 11;
P_TRIANGLE_RIGHT = 12;
_P_SYMBOL_TABLE = save("point",P_POINT, ".",P_POINT,
                       "plus",P_PLUS, "+",P_PLUS,
                       "asterisk",P_ASTERISK,
                       "circle",P_CIRCLE, "o",P_CIRCLE, "O",P_CIRCLE,
                       "cross",P_CROSS, "x",P_CROSS, "X",P_CROSS,
                       "square",P_SQUARE, "#",P_SQUARE,
                       "diamond",P_DIAMOND, "@",P_DIAMOND,
                       "star",P_STAR, "*",P_STAR,
                       "triangle",P_TRIANGLE,
                       "triangleup",P_TRIANGLE_UP, "^",P_TRIANGLE_UP,
                       "triangledown",P_TRIANGLE_DOWN, "v",P_TRIANGLE_DOWN,
                       "V",P_TRIANGLE_DOWN,
                       "triangleleft",P_TRIANGLE_LEFT, "<",P_TRIANGLE_LEFT,
                       "triangleright",P_TRIANGLE_RIGHT, ">",P_TRIANGLE_RIGHT);

func p_marker(&arg, val)
/* DOCUMENT p_marker(arg);
         or p_marker(arg, def);
         or p_marker, arg;
         or p_marker, arg, def;

     Convert the argument ARG (or the default value DEF if ARG is void and DEF
     is specified) into a char value for the `marker` graphic keyword.  When
     called as a subroutine, ARG is redefined with the result.

     The input marker value (of ARG or of DEF) can be a string or an integer (a
     char or an integer ASCII code).  The table below summarizes the different
     possibilities:

         Symbol      Value   String       Marker
         ----------------------------------------------------------------
         P_POINT     1       "point"      a point
         P_PLUS      2       "plus"       a plus sign
         P_ASTERISK  3       "asterisk"   an asterisk (a small "x" cross)
         P_CIRCLE    4       "circle"     a circle (actually an "O")
         P_CROSS     5       "cross"      an "X" cross (actually an "X")
         ----------------------------------------------------------------
           char like 'A' / ASCII code     a letter
         ----------------------------------------------------------------

   SEE ALSO: marker, marks, p_symbol, p_color.
*/
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (is_scalar(val)) {
    s = structof(val);
    if (s == string && strlen(val) == 1) {
      val = strchar(val)(1);
      s = char;
    }
    if (s == char) {
      c = long(val);
      if ((31 < c && c < 127) || (0 < c && c < 6)) {
      accept:
        if (am_subroutine()) {
          arg = val;
          return;
        } else {
          return val;
        }
      }
      /* Convert the character into a string to use the table below. */
      val = strchar(val);
      s = string;
    }
    if (s == string) {
      /* Use the table of symbol names. */
      val = p_get_member(_P_SYMBOL_TABLE, val);
    }
    if (is_integer(val) &&
        ((31 < val && val < 127) || (0 < val && val < 6))) {
      val = char(val);
      goto accept;
    }
  }
  error, "unrecognized marker value";
}

local P_NDC, P_RELATIVE, P_PIXEL, P_WORLD, _P_UNITS_TABLE;
func p_units(&arg, val)
/* DOCUMENT p_units(arg);
         or p_units(arg, def);
         or p_units, arg;
         or p_units, arg, def;

     Convert the argument ARG (or the default value DEF if ARG is void and DEF
     is specified) into an integer value for the `units` graphic keyword.  When
     called as a subroutine, ARG is redefined with the result.

     The input units value (of ARG or of DEF) can be a string or an integer.
     The table below summarizes the different possibilities:

         Symbol      Value   String       Units
         ----------------------------------------------------------------
         P_NDC       0       "ndc", "NDC" Normalized Device Coordinates
         P_RELATIVE  1       "relative"   Relative units in [0,1]
         P_PIXEL     2       "pixel"      Pixel units
         P_WORLD     3       "world"      World (data) units
         ----------------------------------------------------------------

   SEE ALSO: p_style.
*/
{
  if (! is_void(arg)) {
    eq_nocopy, val, arg;
  }
  if (is_scalar(val)) {
    if (is_string(val)) {
      /* Use the table of symbol names. */
      val = p_get_member(_P_UNITS_TABLE, val);
    }
    if (is_integer(val) && 0 <= val && val <= 3) {
      if (am_subroutine()) {
        arg = long(val);
        return;
      } else {
        return long(val);
      }
    }
  }
  error, "unknown units value";
}
P_NDC = 0;
P_RELATIVE = 1;
P_PIXEL = 2;
P_WORLD = 3;
_P_UNITS_TABLE = save("ndc",P_NDC, "NDC",P_NDC, "relative",P_RELATIVE,
                      "pixel", P_PIXEL, "world", P_WORLD);

/*---------------------------------------------------------------------------*/
/* COLOR MAPS */

/* Make sure "cmap.i" is loaded and make an alias for the `cmap` function
   so that `cmap` can be used as a keyword. */
if (! (is_func(cmap) && is_string(gist_names) && is_string(msh_names) &&
       is_string(mpl_names) && is_string(gmt_names) && is_string(cb_names))) {
  include, Y_SITE + "i/cmap.i", 1;
  if (! is_func(cmap)) {
    error, "cmap function not defined";
  }
}
_p_original_cmap = cmap;
_P_COLORMAPS = [];

local _P_COLORMAPS;
func p_colormap(p, n, hist=, hsv=, hsl=, rev=, gamma=, lgamma=, usehue=, nmax=)
/* DOCUMENT p_colormap, p, n;
         or p_colormap(p, n);

     When called as a subroutine, set the current palette with the colormap
     defined by parameter P; when called as a function, returns the colormap as
     a list of colors.

     This function is a wrapper over `cmap` (help, cmap) whose behavior is only
     modified when P is the name of a colormap.  In that case, the colormap
     name can be prefixed with "gist:", "gmt:", "mpl:", "seq:", "msh:" or
     "brewer:" to explicitely specify a family of palettes and avoid conflicts.
     The name of a colormap is case insensitive.

   SEE ALSO: cmap.
*/
{
  if (is_string(p) && is_scalar(p)) {
    if (is_void(_P_COLORMAPS)) {
      /* Create a hash table of konown colormaps. */
      _P_COLORMAPS = save();

      /* Add installed Gist palettes. */
      files = lsdir(Y_SITE+"g");
      files = files(where(strpart(files, -2:0) == ".gp"));
      _p_add_colormaps, files, 8;

      /* Add known colormaps. */
      _p_add_colormaps, gist_names,  7, "gist:";
      _p_add_colormaps, gmt_names,   6, "gmt:";
      _p_add_colormaps, mpl_names,   5, "mpl:";
      _p_add_colormaps, seq_names,   4, "seq:";
      _p_add_colormaps, msh_names,   3, "msh:";
      _p_add_colormaps, "cubehelix", 2;
      _p_add_colormaps, cb_names,    1, "brewer:";
    }

    /* Parse name and extract parameters. */
    n0 = n;
    name = cmap_name(strcase(0, p), n, rev);
    value = p_get_member(_P_COLORMAPS, name);
    if (is_void(value)) {
      p = _cmap_gist_scan(name);
      if (is_void(p)) error, "unknown colormap \""+name+"\"";
    } else {
      type = value(1);
      index = value(2);
      if (type == 1) {
        /* Brewer colormap. */
        local t;
        p = cb_map(index, ((n && n<13)?n:[]), t);
        if ((n != n0) && is_void(hist)) hist = 1;
        n = n0;
        if (is_void(hist) && t == 3) hist = 1;
      } else if (type == 2) {
        /* Cubehelix colormap. */
        rgb = cubehelix(n, gamma=gamma);
        if (rev) rgb = rgb(::-1,);
        if (am_subroutine()) palette, rgb(,1), rgb(,2), rgb(,3);
        return rgb;
      } else if (type == 3) {
        /* MSH colormap */
        p = msh_maps(..,index);
        p = mshct(p(,1), p(,2), n0); // FIXME: transpose?
      } else if (type == 4) {
        /* Sequential colormap */
        h = seq_params(,index);
        l1 = h(2);
        if (l1 < 0) {
          l1 = rgb_saturate(seq_params(1,-l1), seq_params(2,-l1), cmax=1);
        }
        l2 = h(3);
        if (l2 < 0) {
          l2 = rgb_saturate(seq_params(1,-l2), seq_params(3,-l2), cmax=1);
        }
        h = h(1);
        p = seqct(h, n0, l1, 2); // FIXME: transpose?
      } else if (type == 5) {
        /* Matplotlib colormap */
        p = *mpl_maps(index);
      } else if (type == 6) {
        /* GMT (generic mapping tools) colormap */
        p = *gmt_maps(index);
        flags = gmt_flags(index);
        hist = (flags == 1);
        hsv = (flags == 2);
      } else if (type == 7) {
        /* Gist colormap */
        p = *gist_maps(index);
        if (name == "earth" || name == "gist:earth") hsv = 1;
      } else if (type == 8) {
        /* Gist palette */
        p = _cmap_gist_scan(Y_SITE + "g/" + name);
        if (is_void(p)) error, "unknown palette \""+name+"\"";
      }
    }
  }
  if (am_subroutine()) {
    _p_original_cmap, p, n, hist=hist, hsv=hsv, hsl=hsl, rev=rev,
      gamma=gamma, lgamma=lgamma, usehue=usehue, nmax=nmax;
  } else {
    return _p_original_cmap(p, n, hist=hist, hsv=hsv, hsl=hsl, rev=rev,
                            gamma=gamma, lgamma=lgamma, usehue=usehue,
                            nmax=nmax);
  }
}

func _p_add_colormaps(names, type, prefix)
{
  extern _P_COLORMAPS;
  n = numberof(names);
  for (i = 1; i <= n; ++i) {
    name = strcase(0, names(i));
    value = [type, i];
    save, _P_COLORMAPS, noop(name), value;
    if (! is_void(prefix)) {
      save, _P_COLORMAPS, prefix + name, value;
    }
  }
}

/*---------------------------------------------------------------------------*/
/* GRAPHIC STYLES */

/* Autoloading does not work for structures defined in "style.i"; we must
   therefore check whether the style functions are really loaded. */
if (is_void(GfakeSystem) != Y_STRUCTDEF) {
  include, Y_SITE + "i/style.i", 1;
}

func p_split_systems
/* DOCUMENT p_split_systems;
     Split each current plotting system in two systems sharing the same
     viewport, the first one having its axis at bottom and left, the second one
     having its axis at top and right.

   SEE ALSO: get_style, set_style, gridxy, logxy, limits.
 */
{
  local landscape, systems, legends, clegends;
  get_style, landscape, systems, legends, clegends;
  ticksHorizFlags = systems.ticks.horiz.flags;
  ticksVertFlags = systems.ticks.vert.flags;
  f1 = (P_TICK_BOTTOM_OR_LEFT|P_LABEL_BOTTOM_OR_LEFT);
  f2 = (P_TICK_TOP_OR_RIGHT|P_LABEL_TOP_OR_RIGHT);
  systems = systems(-:1:2,..);
  systems(1,..).ticks.horiz.flags = (ticksHorizFlags & ~f2) | f1;
  systems(1,..).ticks.vert.flags = (ticksVertFlags & ~f2) | f1;
  systems(2,..).ticks.horiz.flags = (ticksHorizFlags & ~f1) | f2;
  systems(2,..).ticks.vert.flags = (ticksVertFlags & ~f1) | f2;
  set_style, landscape, systems, legends, clegends;
}

func p_subdivide_viewport(g, w, h, vp, keepaspect=, dpi=)
/* DOCUMENT p_subdivide_viewport(g, w, h, vp);
         or p_subdivide_viewport(g, w, h);

     Subdivide a viewport into smaller viewports arranged on a grid.  The
     result is a 4-by-NVP array of coordinates with NVP the number of
     sub-viewports.

     Argument G is the description of the sub-viewports arrangement of a grid
     of rectangular cells, it is a 4-by-NVP array of integers:

       G(1,k) = initial column number of k-th sub-viewport;
       G(2,k) = initial row number of k-th sub-viewport;
       G(3,k) = width (in number of cells) of k-th sub-viewport;
       G(4,k) = height (in number of cells) of k-th sub-viewport;

     G can also be flattened into a vector of 4*NVP integers.  Column numbers
     run from left to right, row numbers from top to bottom.

     Arguments W and H gives respectively the width and the height of the cells
     of the grid.  They are both vectors of nonnegative values with a strictly
     positive sum.  W(j) is the width of the j-th column and H(i) is the height
     of the i-th row.  These sizes are in relative units, they are scaled so
     that the global bounding box of all the sub-viewports are included in VP
     (see below).  If keyword KEEPASPECT is true, then the scaling is the same
     for both dimensions.  If there is any remaining space, the global bounding
     box is centered in VP.  In order to insert spaces between sub-viewports,
     rows and columns (with appropriate sizes) have to be left between the
     viewports (see example below).

     Optional argument VP is the viewport [LEFT,RIGHT,BOTTOM,TOP] to subdivide
     (coordinates inclusive).  If it is missing, the size of the current
     viewport is used and NDC coordinates are assumed.  If VP is of integer
     type, it is assumed to be given in pixels.  Otherwise, VP must be of real
     type.  If VP is of real type and the DPI keyword (see below) is specified,
     then VP is assumed be in NDC coordinates and small adjustements are made
     to avoid overlapping sub-viewports.

     Keyword DPI can be used to specify the number of pixels per inch.  If
     unspecified, DPI is taken from the current graphics window.

  EXAMPLE:
     Using:

       g = [1, 1, 1, 1,  // a 1x1 cell starting at (1,1)
            3, 1, 1, 1,  // a 1x1 cell starting at (3,1)
            1, 3, 3, 1]; // a 3x1 cell starting at (1,3)

    and:

       v = p_subdivide_viewport(g, [400, 10, 100], [250, 10, 250]);

     will subdivide the current viewport in 3 sub-viewports as sketched in the
     left part of the figure below.  Without spacing, i.e. with:

       v = p_subdivide_viewport(g, [400, 0, 100], [250, 0, 250], dpi=75);

     will produce the result shown in the right.

         +-------+ +--+            +--------+---+
         |1      | |2 |            |1       |2  |
         |       | |  |            |        |   |
         +-------+ +--+            +--------+---+
         +------------+            |3           |
         |3           |            |            |
         |            |            |            |
         +------------+            +------------+

   SEE ALSO: viewport, p_geometry.
*/
{
  if (is_void(vp)) {
    vp = p_viewport_world();
  } else if (identof(vp) > Y_DOUBLE || numberof(vp) != 4) {
    error, "bad type/dimension for viewport";
  }

  if (identof(g) > Y_LONG) {
    error, "bad type for grid";
  }
  dims = dimsof(g);
  rank = dims(1);
  if ((rank == 1 && dims(2) % 4 == 0) ||
      (rank == 2 && dims(2) == 4)) {
    nvp = numberof(g)/4;
  } else {
    error, "bad dimensions for grid";
  }
  if (rank == 1) {
    c0 = g(1::4); // columns run from left to right
    r1 = g(2::4); // rows run from top to bottom
    nc = g(3::4);
    nr = g(4::4);
  } else {
    c0 = g(1,);
    r1 = g(2,);
    nc = g(3,);
    nr = g(4,);
  }
  if (min(c0) < 1 || min(r1) < 1) {
    error, "invalid sub-wiewport position(s)";
  }
  if (min(nc) < 1 || min(nr) < 1) {
    error, "invalid sub-wiewport size(s)";
  }
  c1 = c0 + nc;
  r0 = r1 + nr;
  ncols = max(c1) - 1;
  nrows = max(r0) - 1;

  if ((id = identof(w)) > Y_DOUBLE) {
    error, "bad type for W";
  } else if (min(w) < 0.0) {
    error, "bad value in W";
  } else if (is_scalar(w)) {
    w = array(double(w), ncols);
  } else if (! is_vector(w) || numberof(w) < ncols) {
    error, "bad dimensions for W";
  } else if (id != Y_DOUBLE) {
    w = double(w);
  }

  if ((id = identof(h)) > Y_DOUBLE) {
    error, "bad type for H";
  } else if (min(h) < 0.0) {
    error, "bad value in H";
  } else if (is_scalar(h)) {
    h = array(double(h), nrows);
  } else if (! is_vector(h) || numberof(h) < nrows) {
    error, "bad dimensions for H";
  } else if (id != Y_DOUBLE) {
    h = double(h);
  }

  if (is_real(vp)) {
    /* Adjustment by one pixel if DPI is specified. */
    if (is_void(dpi)) {
      geom = window_geometry();
      s = geom(2);
    } else {
      s = P_NDC_INCH/dpi; /* one pixel in NDC units */
    }
    vp = double(vp);
  } else {
    /* Assume pixels. */
    s = 1;
    vp = long(vp);
  }
  left = vp(1);
  right = vp(2);
  bottom = vp(3);
  top = vp(4);
  width = abs(right - left) + s;
  height = abs(top - bottom) + s;
  h_scale = double(height)/double(sum(h));
  w_scale = double(width)/double(sum(w));
  if (keepaspect) {
    min_scale = min(h_scale, w_scale);
    h *= min_scale;
    w *= min_scale;
  } else {
    h *= h_scale;
    w *= w_scale;
  }
  x = w(cum);
  y = h(cum);
  if (is_integer(vp)) {
    /* round to nearest integer *after* cumulative sum to have at most one
       pixel error */
    x = lround(x);
    y = lround(y);
  }

  /* Compute bounding boxes of grid cells and center them in the viewport.
     Note that column numbers run from left to right, row numbers from top to
     bottom (hence the uneven arithmetic). */
  if (right >= left) {
    x1 = (x0 = x) - s;
  } else {
    x1 = (x0 = -x) + s;
  }
  if (top >= bottom) {
    y1 = (y0 = -y) - s;
  } else {
    y1 = (y0 = y) + s;
  }
  xoff = ((left - x0(1)) + (right - x1(0)))/2.0;
  yoff = ((bottom - y0(1)) + (top - y1(0)))/2.0;
  if (is_integer(vp)) {
    xoff = lround(xoff);
    yoff = lround(yoff);
  }
  x0 += xoff;
  x1 += xoff;
  y0 += yoff;
  y1 += yoff;

  /* Compute bounding boxes (inclusive) of sub-viewports. */
  v = array(structof(x), 4, nvp);
  v(1,) = x0(c0);
  v(2,) = x1(c1);
  v(3,) = y0(r0);
  v(4,) = y1(r1);
  return v;
}

local P_NDC_POINT, P_NDC_INCH, P_NDC_CENTIMETER, P_NDC_MILLIMETER;
local P_NDC_PAGE_WIDTH, P_NDC_PAGE_HEIGHT;
/* DOCUMENT P_NDC_POINT        one point in NDC units,
         or P_NDC_INCH         one inch in NDC units,
         or P_NDC_CENTIMETER   one centimeter in NDC units,
         or P_NDC_MILLIMETER   one millimeter in NDC units,
         or P_NDC_PAGE_WIDTH   US letter width in DNC units,
         or P_NDC_PAGE_HEIGHT  US letter width in DNC units.

      The normalized device coordinates (NDC for short) are specific units used
      by Gist (the Yorick graphic engine) to measure dimensions in graphics in
      a device independent way (one NDC units is about 11 inches).

      In order to set dimensions in NDC units, you may use the following
      formulae:

          length_in_NDC = length_in_points*P_NDC_POINT;
                        = length_in_pixels*P_NDC_INCH/DPI;
                        = length_in_inches*P_NDC_INCH;
                        = length_in_centimeters*P_NDC_CENTIMETER;
                        = length_in_millimeters*P_NDC_MILLIMETER;

      where DPI (for dots per inch) is the resolution of the graphic device.
      Typically Yorick graphic windws have 75 or 100 dpi.
*/

/* US letter page size and some other constants (see gist/gist.c and
   gist/gist.h). */
P_NDC_PAGE_WIDTH  = 0.798584;        /* 8.5 inches in NDC units */
P_NDC_PAGE_HEIGHT = 1.033461;        /* 11 inches in NDC units */
P_NDC_POINT = 0.0013000;             /* one point in NDC units */
P_NDC_INCH = 72.27*P_NDC_POINT;      /* one inch in NDC units */
P_NDC_CENTIMETER = 2.54*P_NDC_INCH;  /* one centimeter in NDC units */
P_NDC_MILLIMETER = 0.254*P_NDC_INCH; /* one centimeter in NDC units */

func p_viewport_world(sys)
/* DOCUMENT vp = p_viewport_world();
         or vp = p_viewport_world(sys);
         or vp = p_viewport_ndc();
         or vp = p_viewport_ndc(sys);

     These functions return the viewport of a plotting system in world or in
     NDC units.  Optional parameter SYS is the plotting system number; if SYS
     is not specified, the current plotting system is considered.  The result
     is [xmin,xmax,ymin,ymax] or [0,0,0,0] if the plotting system is 0.

   SEE ALSO: limits, gridxy, get_style, plsys, viewport.
*/
{
  if (is_void(sys)) return _p_builtin_viewport();
  old = plsys(sys);
  res = _p_builtin_viewport();
  if (old != sys) plsys, old;
  return res;
}

func p_viewport_ndc(sys)
{
  local landscape, systems, legends, clegends;
  get_style, landscape, systems, legends, clegends;
  if (is_void(sys)) sys = plsys();
  return systems(sys).viewport;
}

func p_geometry(win, landscape=)
/* DOCUMENT g = p_geometry(win);

     Yield parameters to transform pixel coordinates to NDC coordinates for
     window WIN.  If WIN is unspecified, the current window is considered.  The
     result is a vector of 6 doubles:

          G = [DPI, ONE_PIXEL, XBIAS, YBIAS, WIDTH, HEIGHT]

      where:

          DPI ......... dot-per-inch of WIN;
          ONE_PIXEL ... pixel size in NDC units;
          XOFF ........ abscissa offset in NDC units;
          YOFF ........ ordinate offset in NDC units;
          WIDTH ....... width of drawable region in pixels;
          HEIGHT ...... height of drawable region in pixels;

     Coordinates (XPIX,YPIX) of drawable pixels run from top-left (0,0) to
     bottom-right (WIDTH-1,HEIGHT-1).  The conversion to NDC coordinates is:

          XNDC = XOFF + XPIX*ONE_PIXEL;
          YNDC = YOFF - YPIX*ONE_PIXEL;

     The result G is the same as `window_geometry` except that the size of the
     drawing area is correctly set and that the offsets are correctly computed
     even after a resize which may change the orientation of the page.  The
     drawing area may be smaller than the window size (which is returned by
     `window_geometry`) because it is limited to a page size (US letter format)
     and corresponds to the part which is correctly erased by `fma` and redraw
     by `redraw`.  The region used by Gist to display coordinates is not
     considered as part of the drawing region.

     Keyword LANDSCAPE can be explicitly set to force landscape or portrait
     mode.  The default page orientation is landscape if the width of the
     plotting area is strictly larger than its height; otherwise the
     orientation is portrait.

     If window WIN does not exists, all output values are zero.

   SEE ALSO: window_geometry.
 */
{
  /* The following notes result from reverse engineering and careful reading of
     Gist code (and tracking of the various coordinates transforms).  I hope
     that my explanations are clear.

     The geometry returned by window_geometry() is fetched from the current
     state of the graphic engine.  It does not take the decoration (notably the
     topmost message bar) into account; so the topMargin and leftMargin
     parameters are not needed.  In the C code (see gist/xbasic.c and
     gist/xfancy.c), these margins have to be added removed to/from the size of
     the window.  For instance, X11 "resize" event yields the size of the full
     window while Gist engine only stores the size of the plotting area.

     If the width of the plotting area is strictly larger than its height, then
     we are in landscape mode; otherwise, we are in portrait mode.

     The Gist engine assume plotting on a US letter page (8.5x11 inches).  The
     plotting area is centered on the page in landscape mode and centered on
     the topmost square part of the page in portrait mode.  If either dimension
     is big enough for the whole page, the page is left/top justified.
  */

  /* Get current size of plotting area. */
  g = window_geometry(win);
  if (is_void(g) || g(1) == 0.0) {
    /* Window does not exist. */
    return array(double, 6);
  }
  ONE_PIXEL = g(2); // one pixel in NDC units
  width = long(g(5));
  height = long(g(6));
  if (is_void(landscape)) {
    landscape = (width > height);
  }

  /* Compute left and top offsets (in pixels) of the plotting area with respect
     to the page borders according to the centering rules (see notes above) and
     with the same rounding/truncation conventions as in the C code (see
     gist/xfancy.c). */
  page_width = long(P_NDC_PAGE_WIDTH/ONE_PIXEL);
  page_height = long(P_NDC_PAGE_HEIGHT/ONE_PIXEL);
  top = max(0, (page_width - height)/2); // *before* accounting for orientation
  if (landscape) swap, page_width, page_height;
  left = max(0, (page_width - width)/2);

  /* The coordinate transform formulae are (the 0.5 offset if for rounding to
     the next integer and the top of the page in NDC coordinates is exactly
     PAGE_HEIGHT - ONE_PIXEL):

     FIXME: +/- 0.5 here

         x_win = (int)(x_ndc/ONE_PIXEL - left + 0.5);
         y_win = (int)((PAGE_HEIGHT - ONE_PIXEL - y_ndc)/ONE_PIXEL - top + 0.5);
               = (int)((PAGE_HEIGHT - y_ndc)/ONE_PIXEL - top - 0.5);

     thus the reciprocal transform is given by:

         x_ndc = (left + 0.5)*ONE_PIXEL + x_win*ONE_PIXEL;
         y_ndc = PAGE_HEIGHT - (top + 0.5)*ONE_PIXEL - y_win*ONE_PIXEL;

     with (X_NDC,Y_NDC) the page coordinates in NDC units (assumed to be
     non-negative), (X_WIN,Y_WIN) the window coordinates in pixels, (LEFT,TOP)
     the offsets for centering, and PAGE_HEIGHT the page height in NDC units
     (accounting for orientation). */

  xoff = (left + 0.5)*ONE_PIXEL;
  yoff = ((landscape ? P_NDC_PAGE_WIDTH : P_NDC_PAGE_HEIGHT)
          - (top + 0.5)*ONE_PIXEL);
  g(3) = xoff;
  g(4) = yoff;
  g(5) = min(width, page_width);
  g(6) = min(height, page_height);
  return g;
}

local p_get_style, p_set_style;
/* DOCUMENT ptr = p_get_style(win);
         or p_set_style, ptr;
         or p_set_style, ptr, win;

     The `p_get_style()` function retrieves the style of the graphic window
     WIN, or of the current window is WIN is unspecified.

     The `p_set_style()` function applies the style stored by PTR to the
     graphic window WIN, or to the current window is WIN is unspecified.

     If WIN is specified, the current graphic window remains unchanged.

     PTR is an array of 4 pointers:

         ptr = [&landscape, &systems, &legends, &clegends];

   SEE ALSO: get_style, set_style.
 */

func p_set_style(ptr, win)
{
  if (structof(ptr) != pointer || numberof(ptr) != 4) {
    error, "illegal argument";
  }
  if (is_void(win)) {
    oldwin = -1;
  } else {
    oldwin = current_window();
    window, win;
  }
  set_style, *ptr(1), *ptr(2), *ptr(3), *ptr(4);
  if (oldwin >= 0) {
    window, oldwin;
  }
}

func p_get_style(win)
{
  local landscape, systems, legends, clegends;
  if (is_void(win)) {
    oldwin = -1;
  } else {
    oldwin = current_window();
    window, win;
  }
  get_style, landscape, systems, legends, clegends;
  if (oldwin >= 0) {
    window, oldwin;
  }
  return [&landscape, &systems, &legends, &clegends];
}

func p_style(win, geom=, verb=, aspect=, viewport=, units=, size=, color=,
             linecolor=, linetype=, linewidth=,
             gridlevel=, gridtype=, gridcolor=, gridwidth=,
             frame=, frametype=, framecolor=, framewidth=,
             ticklen=, ticktype=, tickcolor=, tickwidth=, tickflags=,
             ndigits=, textcolor=, font=, textheight=)
/* DOCUMENT p_style, win, key=val, ...;
         or p_style(win, key=val, ...);

     Helper routine to build a style sheet from optional keywords and with
     otherwise sensible settings.  Optional argument WIN is the graphic window
     for which the style is designed, if unspecified, the current graphic
     window is used.  When called as a subroutine, the style of the graphic
     window is modified.  When called as a function, the current style is not
     modified and the return value is:

         [&landscape, &systems, &legends, &clegends]

     which can be applied to a given window with `p_set_style()`.

     If there are more than one viewport, they will all share the same
     settings.  If that is inappropriate, you can call `p_style()` as a
     function to get the attributes of all the plotting systems (as many a
     there are viewports), edit these attributes and finally set the style to
     the target window.


   KEYWORDS

     Keywords are used to specify the attributes of the graphic style elements.
     Some attributes inherit from others, so that several attributes can be set
     by a single keyword.  The values of the attributes are filtered by helpers
     like p_color, p_font, etc.; as a result, all X11 colors or HTML notation
     are supported.  The following keywords are supported:

     viewport = The viewport(s) as [XMIN,XMAX,YMIN,YMAX] or
            [[XMIN1,XMAX1,YMIN1,YMAX1],[XMIN2,XMAX2,YMIN2,YMAX2],...].  If
            unspecified, a single viewport is chosen to fit in the graphic
            window (with some margins for the axis labels and the titles) and
            according to the aspect ratio.  Keyword UNITS can be used to
            specify the units of the viewport(s) coordinates which are NDC
            (Normalized Device Coordinates) by default.

     units = Units for the viewport(s) coordinates.  Allowed values are "ndc"
            (the default), "relative", or "pixel" (see p_units).

     aspect = Aspect ratio (default to fit the window) as the width divided by
            the height of the viewport.

     size = Default size (in NDC units) for attributes such as tick length,
            font height, etc.  The default value is computed from the height of
            the viewport(s) to warrant a certain constant aspect of the plots.
            This is however "not quite an exact science" (Karadoc, in
            Kamelott).

     geom = Geometrical parameters of the window for which the style is
            designed (see p_geometry).  If not set, it is taken from WIN.

     ndigits = Number of digits for axis labels (default is 4).

     color = Default color for all items (default is foreground color).

     linetype = Default type for all lines; "solid" by default.

     linecolor = Default color for all lines; the foreground color by default.

     linewidth = Default width for all lines; 0.0 by default.

     frame = True to draw the viewport frame, default is false.

     frametype = Line type for the viewport frame; inherits from LINETYPE.

     framecolor = Color of the viewport frame; inherits from LINECOLOR.

     framewidth = Width of the viewport frame; inherits from LINEWIDTH.

     gridtype = Line type for the grid; "dot" by default.

     gridcolor = Color of the grid; inherits from LINECOLOR.

     gridwidth = Width of the grid lines; inherits from LINEWIDTH/2.

     gridlevel = The tick level up to which draw grid lines (default is 1, to
            draw grid lines only at major ticks).

     ticktype = Line type for the axis ticks; inherits from LINETYPE.

     tickcolor = Color of the axis ticks; inherits from LINECOLOR.

     tickwidth = Width of the axis ticks; inherits from LINEWIDTH.

     ticklen = Lenght(s) of the axis ticks.

     tickflags = Flags for the axis ticks.

     textheight = The height of the font for the labels in NDC units; inherits
            from SIZE.

     textcolor = The color for the labels; the COLOR attribute by default.

     font = The font for the labels; Helvetica by default.


   SEE ALSO p_set_style, p_geometry, p_color, p_font.

 */
{
  local landscape;
  if (is_void(geom)) {
    if (is_void(win) && ! am_subroutine()) {
      /* Takes the geometry of a standard 75 DPI Gist window to avoid the
         side effect of creating a graphic window. */
      geom = [75.0, 0.001252680, 0.117125580, 0.916335420, 450.0, 450.0];
    } else {
      geom = p_geometry(win, landscape=landscape);
    }
  } else if (is_real(geom) && numberof(geom) == 6 && geom(min:1:2) > 0.0 &&
             geom(min:5:6) > 0.0) {
    geom = double(geom);
  } else {
    error, "invalid geometry";
  }
  dpi       = geom(1);
  ONE_PIXEL = geom(2);
  xbias     = geom(3);
  ybias     = geom(4);
  width     = geom(5);
  height    = geom(6);
  if (is_void(viewport)) {
    /* Compute a default single viewport with some margins. */
    nvp = 1;
    xmargin = width/7.5;
    ymargin = height/7.5;
    if (is_void(aspect)) {
      aspect = (width - 2.0*xmargin)/(height - 2.0*ymargin);
    } else {
      /* Adjust the margins to have the specified aspect. */
      if ((width - 2.0*xmargin) < aspect*(height - 2.0*ymargin)) {
        ymargin = (height - (width - 2.0*xmargin)/aspect)/2.0;
      } else {
        xmargin = (width - aspect*(height - 2.0*ymargin))/2.0;
      }
    }
    if (is_void(landscape)) {
      landscape = (aspect > 1.0);
    }
    xmin = xbias + xmargin*ONE_PIXEL;
    xmax = xbias + (width - xmargin)*ONE_PIXEL;
    ymin = ybias - (height - ymargin)*ONE_PIXEL;
    ymax = ybias - ymargin*ONE_PIXEL;
    viewport = [xmin, xmax, ymin, ymax];
  } else {
    /* Use the specified viewport to guess the lanscape mode. */
    if (! is_void(aspect)) {
      error, "viewport and aspect cannot be both specified";
    }
    vpdims = dimsof(viewport);
    if (identof(viewport) > Y_DOUBLE || vpdims(1) < 1 || vpdims(2) != 4) {
      error, "invalid viewport";
    }
    viewport = double(viewport);
    xmin = viewport(1,..);
    xmax = viewport(2,..);
    ymin = viewport(3,..);
    ymax = viewport(4,..);
    nvp = numberof(xmin);
    p_units, units, P_NDC;
    if (units != P_NDC) {
      if (units == P_RELATIVE) {
        /* Relative size. */
        xscl = width*ONE_PIXEL;
        yscl = height*ONE_PIXEL;
        xoff = xbias;
        yoff = ybias - yscl;
      } else if (units == P_PIXEL) {
        /* Pixel units. */
        xscl = +ONE_PIXEL;
        yscl = -ONE_PIXEL;
        xoff = xbias;
        yoff = ybias;
      } else {
        error, "invalid units";
      }
      xmin = xoff + xscl*xmin;
      xmax = xoff + xscl*xmax;
      ymin = yoff + yscl*ymin;
      ymax = yoff + yscl*ymax;
      viewport(1,..) = xmin;
      viewport(2,..) = xmax;
      viewport(3,..) = ymin;
      viewport(4,..) = ymax;
    }
    if (is_void(landscape)) {
      landscape = ((max(max(xmin),max(xmax)) - min(min(xmin),min(xmax))) >
                   (max(max(ymin),max(ymax)) - min(min(ymin),min(ymax))));
    }
  }

  p_real, size, 0.035*max(abs(ymax - ymin));
  if (verb) {
    write, format="  size = %f NDC (equivallent to ~ %.1f pt)\n",
      size, size/P_NDC_POINT;
  }

  frame = (frame ? P_TRUE : P_FALSE);

  p_integer, ndigits, 4;
  p_integer, gridlevel, 1;

  /* Parse colors. */
  p_color, color, P_FG;
  p_color, textcolor, color;
  p_color, linecolor, color;
  p_color, gridcolor, linecolor;
  p_color, framecolor, linecolor;
  p_color, tickcolor, linecolor;

  /* Parse line types. */
  p_type, linetype, P_SOLID;
  p_type, gridtype, P_DOT;
  p_type, frametype, linetype;
  p_type, ticktype, linetype;

  /* Parse line widths. */
  p_real, linewidth, 0.0;
  p_real, gridwidth, 0.5*linewidth;
  p_real, framewidth, linewidth;
  p_real, tickwidth, linewidth;

  p_font, font, P_HELVETICA;

  if (is_void(textheight)) textheight = size;
  p_integer, tickflags, (P_TICK_BOTTOM_OR_LEFT | P_TICK_TOP_OR_RIGHT |
                         P_LABEL_BOTTOM_OR_LEFT | P_TICK_OUTWARD);

  legends = p_legend_box();
  clegends = p_legend_box();
  textstyle = p_text_attribs(color=textcolor, font=font, height=textheight);

  tickrellen = [1.0, 0.64, 0.36, 0.18, 0.09]; /* tick lengths relative to
                                                 the first one */
  if (is_void(ticklen)) {
    ticklen = (0.68*textheight)*tickrellen;
  } else if (is_scalar(ticklen)) {
    ticklen *= tickrellen;
  }

  /* Line styles. */
  linestyle = p_line_attribs(color=linecolor, type=linetype, width=linewidth);
  tickstyle = p_line_attribs(linestyle, color=tickcolor, type=ticktype,
                             width=tickwidth);
  gridstyle = p_line_attribs(linestyle, color=gridcolor, type=gridtype,
                             width=gridwidth);
  framestyle = p_line_attribs(linestyle, color=framecolor, type=frametype,
                              width=framewidth);

  axis = p_axis_style(nMajor = 7.5,
                      nMinor = 50,
                      logAdjMajor = 1.2,
                      logAdjMinor = 1.1,
                      nDigits = ndigits,
                      gridLevel = gridlevel,
                      flags = tickflags,
                      tickOff = 0.04*textheight,
                      labelOff = textheight,
                      tickLen = ticklen,
                      tickStyle = tickstyle,
                      gridStyle = gridstyle,
                      textStyle = textstyle);
  vert = p_axis_style(axis,
                      xOver = xmax - 10.0*textheight,
                      yOver = ymin -  5.0*textheight);
  horiz = p_axis_style(axis,
                       xOver = xmin -  7.0*textheight,
                       yOver = ymax -  3.0*textheight);
  ticks = p_tick_style(horiz=horiz, vert=vert,
                       frame=frame, frameStyle=framestyle);
  systems = p_system(viewport=viewport(,1), ticks=ticks, legend="Viewport 1");
  if (nvp > 1) {
    dims = vpdims(2:0);
    dims(1) = vpdims(1) - 1;
    systems = array(systems, dims);
    systems.viewport = viewport;
    legend = swrite(format="Viewport %d", indgen(nvp));
    reshape, legend, string, dims;
    systems.legend = legend;
  }
  if (am_subroutine()) {
    if (is_void(win)) {
      oldwin = -1;
    } else {
      oldwin = current_window();
      window, win;
    }
    set_style, landscape, systems, legends, clegends;
    if (oldwin >= 0) {
      window, oldwin;
    }
  } else {
    return [&landscape, &systems, &legends, &clegends];
  }
}

/* Ticks flags (add together the ones you want):
   0x001  Draw ticks on bottom or left edge of viewport
   0x002  Draw ticks on top or right edge of viewport
   0x004  Draw ticks centered on origin in middle of viewport
   0x008  Ticks project inward into viewport
   0x010  Ticks project outward away from viewport (0x18 for both)
   0x020  Draw tick label numbers on bottom or left edge of viewport
   0x040  Draw tick label numbers on top or right edge of viewport
   0x080  Draw all grid lines down to gridLevel
   0x100  Draw single grid line at origin
*/
P_TICK_BOTTOM_OR_LEFT  = 0x001; // Draw ticks on bottom or left edge of viewport
P_TICK_TOP_OR_RIGHT    = 0x002; // Draw ticks on top or right edge of viewport
P_TICK_CENTERED        = 0x004; // Draw ticks centered on origin in middle of viewport
P_TICK_INWARD          = 0x008; // Ticks project inward into viewport
P_TICK_OUTWARD         = 0x010; // Ticks project outward away from viewport (0x18 for both)
P_LABEL_BOTTOM_OR_LEFT = 0x020; // Draw tick label numbers on bottom or left edge of viewport
P_LABEL_TOP_OR_RIGHT   = 0x040; // Draw tick label numbers on top or right edge of viewport
P_GRID_LINES           = 0x080; // Draw all grid lines down to gridLevel
P_GRID_ORIGIN          = 0x100; // Draw single grid line at origin

local P_DEFAULT_LINE_ATTRIBS;
func p_line_attribs(def, color=, type=, width=)
/* DOCUMENT p_line_attribs(def, key1=val1, key2=val2, ...);

     Helper function to define a GpLineAttribs structure.  DEF is an optional
     GpLineAttribs structure to provide default values for the attributes which
     are not explicitly specified as keywords (key1=val1, key2=val2, ...).  If
     DEF is not specified, P_DEFAULT_LINE_ATTRIBS is used.  All the other
     arguments are attributes specified by keywords.  The available attributes
     are:

       color=
       type=
       width=


   SEE ALSO: get_style, set_style, color, type, p_axis_style, p_line_attribs,
             p_legend_box, p_system, p_text_attribs, p_tick_style.
 */
{
  if (is_void(def)) eq_nocopy, def, P_DEFAULT_LINE_ATTRIBS;
  return GpLineAttribs(color = p_icolor(color, def.color),
                       type = (is_void(type) ? def.type : type),
                       width = (is_void(width) ? def.width : width));
}
P_DEFAULT_LINE_ATTRIBS = GpLineAttribs(color=P_FG, type=P_SOLID, width=1);

local P_DEFAULT_TEXT_ATTRIBS;
func p_text_attribs(def, color=, font=, height=, orient=,
                    alignH=, alignV=, opaque=)
/* DOCUMENT p_text_attribs(def, key1=val1, key2=val2, ...);

     Helper function to define a GpTextAttribs structure.  DEF is an optional
     GpTextAttribs structure to provide default values for the attributes which
     are not explicitly specified as keywords (key1=val1, key2=val2, ...).  If
     DEF is not specified, P_DEFAULT_TEXT_ATTRIBS is used.  All the other
     arguments are attributes specified by keywords.  The available attributes
     are:

       color=
       font=
       height=
       orient=
       alignH=
       alignV=
       opaque=


   SEE ALSO: get_style, set_style, p_axis_style, p_line_attribs, p_legend_box,
             p_system, p_text_attribs, p_tick_style.
 */
{
  if (is_void(def)) eq_nocopy, def, P_DEFAULT_TEXT_ATTRIBS;
  return GpTextAttribs(color = p_icolor(color, def.color),
                       font = (is_void(font) ? def.font : font),
                       height = (is_void(height) ? def.height : height),
                       orient = (is_void(orient) ? def.orient : orient),
                       alignH = (is_void(alignH) ? def.alignH : alignH),
                       alignV = (is_void(alignV) ? def.alignV : alignV),
                       opaque = (is_void(opaque) ? def.opaque : opaque));
}
P_DEFAULT_TEXT_ATTRIBS = GpTextAttribs(color=P_FG, font=P_HELVETICA,
                                       height=0.0182, orient=0,
                                       alignH=0, alignV=0, opaque=0);


local P_DEFAULT_AXIS_STYLE, P_DEFAULT_VERT_AXIS_STYLE, P_DEFAULT_HORIZ_AXIS_STYLE;
func p_axis_style(def, nMajor=, nMinor=, logAdjMajor=, logAdjMinor=,
                  nDigits=, gridLevel=, flags=, tickOff=, labelOff=, tickLen=,
                  tickStyle=, gridStyle=, textStyle=, xOver=, yOver=)
/* DOCUMENT p_axis_style(def, key1=val1, key2=val2, ...);

     Helper function to define a GpTextAttribs structure.  DEF is an optional
     GpTextAttribs structure to provide default values for the attributes which
     are not explicitly specified as keywords (key1=val1, key2=val2, ...).  If
     DEF is not specified, P_DEFAULT_AXIS_STYLE is used.  All the other
     arguments are attributes specified by keywords.  The available attributes
     are:

       nMajor=
       nMinor=
       logAdjMajor=
       logAdjMinor=
       nDigits=
       gridLevel=
       flags=
       tickOff=
       labelOff=
       tickLen=
       tickStyle=
       gridStyle=
       textStyle=
       xOver= position for overflow label (NDC units)
       yOver= position for overflow label (NDC units)


   SEE ALSO: get_style, set_style, p_axis_style, p_line_attribs, p_legend_box,
             p_system, p_text_attribs, p_tick_style.
 */
{
  if (is_void(def)) eq_nocopy, def, P_DEFAULT_AXIS_STYLE;
  return GaAxisStyle(nMajor = (is_void(nMajor) ? def.nMajor : nMajor),
                     nMinor = (is_void(nMinor) ? def.nMinor : nMinor),
                     logAdjMajor = (is_void(logAdjMajor) ? def.logAdjMajor : logAdjMajor),
                     logAdjMinor = (is_void(logAdjMinor) ? def.logAdjMinor : logAdjMinor),
                     nDigits = (is_void(nDigits) ? def.nDigits : nDigits),
                     gridLevel = (is_void(gridLevel) ? def.gridLevel : gridLevel),
                     flags = (is_void(flags) ? def.flags : flags),
                     tickOff = (is_void(tickOff) ? def.tickOff : tickOff),
                     labelOff = (is_void(labelOff) ? def.labelOff : labelOff),
                     tickLen = (is_void(tickLen) ? def.tickLen : tickLen),
                     tickStyle = (is_void(tickStyle) ? def.tickStyle : tickStyle),
                     gridStyle = (is_void(gridStyle) ? def.gridStyle : gridStyle),
                     textStyle = (is_void(textStyle) ? def.textStyle : textStyle),
                     xOver = (is_void(xOver) ? def.xOver : xOver),
                     yOver = (is_void(yOver) ? def.yOver : yOver));
}
P_DEFAULT_AXIS_STYLE = GaAxisStyle(nMajor = 7.5,
                                   nMinor = 50,
                                   logAdjMajor = 1.2,
                                   logAdjMinor = 1.2,
                                   nDigits = 4,
                                   gridLevel = 1,
                                   flags = (P_TICK_BOTTOM_OR_LEFT | P_TICK_TOP_OR_RIGHT |
                                            P_LABEL_BOTTOM_OR_LEFT | P_TICK_OUTWARD),
                                   tickOff = 0.0007,
                                   labelOff = 0.0182,
                                   tickLen = [0.0143,0.0091,0.0052,0.0026,0.0013],
                                   tickStyle = p_line_attribs(type = P_SOLID),
                                   gridStyle = p_line_attribs(type = P_DOT),
                                   textStyle = P_DEFAULT_TEXT_ATTRIBS,
                                   xOver = 0.395,
                                   yOver = 0.37);
P_DEFAULT_HORIZ_AXIS_STYLE = p_axis_style(xOver=0.395, yOver=0.37);
P_DEFAULT_VERT_AXIS_STYLE = p_axis_style(xOver=0.15, yOver=0.37);

local P_DEFAULT_TICK_STYLE;
func p_tick_style(def, horiz=, vert=, frame=, frameStyle=)
/* DOCUMENT p_tick_style(def, key1=val1, key2=val2, ...);

     Helper function to define a GpTickAttribs structure.  DEF is an optional
     GpTickAttribs structure to provide default values for the attributes which
     are not explicitly specified as keywords (key1=val1, key2=val2, ...).  If
     DEF is not specified, P_DEFAULT_AXIS_STYLE is used.  All the other
     arguments are attributes specified by keywords.  The available attributes
     are:

       horiz = GaAxisStyle structure for the horizontal axis.
       vert  = GaAxisStyle structure for the vertical axis.
       frame = non-zero integer to draw the frame.
       frameStyle = GpLineAttribs structure for the frame line.

   SEE ALSO: get_style, set_style, p_axis_style, p_line_attribs, p_legend_box,
             p_system, p_text_attribs, p_tick_style.
 */
{
  if (is_void(def)) eq_nocopy, def, P_DEFAULT_TICK_STYLE;
  return GaTickStyle(horiz = (is_void(horiz) ? def.horiz : horiz),
                     vert = (is_void(vert) ? def.vert : vert),
                     frame = (is_void(frame) ? def.frame : frame),
                     frameStyle = (is_void(frameStyle) ? def.frameStyle : frameStyle));
}
P_DEFAULT_TICK_STYLE = GaTickStyle(horiz = P_DEFAULT_HORIZ_AXIS_STYLE,
                                   vert = P_DEFAULT_VERT_AXIS_STYLE,
                                   frame = 0,
                                   frameStyle = P_DEFAULT_LINE_ATTRIBS);

local P_DEFAULT_VIEWPORT, P_DEFAULT_LEGEND, P_DEFAULT_SYSTEM;
func p_system(def, viewport=, ticks=, legend=)
/* DOCUMENT p_system(def, key1=val1, key2=val2, ...);

     Helper function to define a GFakeSystem structure.  DEF is an optional
     GFakeSystem structure to provide default values for the attributes which
     are not explicitly specified as keywords (key1=val1, key2=val2, ...).  If
     DEF is not specified, P_DEFAULT_SYSTEM is used.  All the other arguments
     are attributes specified by keywords.  The available attributes are:

       viewport = the viewport coordinates [xmin,xmax,ymin,ymax] in NDC units.
       ticks    = GaTickStyle structure for the axis and the frame.
       legend   = a string.

   SEE ALSO: get_style, set_style, p_axis_style, p_line_attribs, p_legend_box,
             p_system, p_text_attribs, p_tick_style.
 */
{
  if (is_void(def)) eq_nocopy, def, P_DEFAULT_SYSTEM;
  return GfakeSystem(viewport = (is_void(viewport) ? def.viewport : viewport),
                     ticks = (is_void(ticks) ? def.ticks : ticks),
                     legend = (is_void(legend) ? def.legend : legend));
}
P_DEFAULT_VIEWPORT = [0.19,0.6,0.44,0.85];
P_DEFAULT_LEGEND = "System 0";
P_DEFAULT_SYSTEM = GfakeSystem(viewport = P_DEFAULT_VIEWPORT,
                               ticks = P_DEFAULT_TICK_STYLE,
                               legend = P_DEFAULT_LEGEND);

local P_DEFAULT_LEGEND_BOX, P_DEFAULT_CLEGEND_BOX;
func p_legend_box(def, x=, y=, dx=, dy=, textStyle=, nchars=, nlines=, nwrap=)
/* DOCUMENT p_system(def, key1=val1, key2=val2, ...);

     Helper function to define a GeLegendBox structure.  DEF is an optional
     GeLegendBox structure to provide default values for the attributes which
     are not explicitly specified as keywords (key1=val1, key2=val2, ...).  If
     DEF is not specified, P_DEFAULT_LEGEND_BOX is used.  All the other
     arguments are attributes specified by keywords.  The available attributes
     are:

       x, y      - NDC location of this legend box;
       dx, dy    - if non-zero, offset to 2nd column;
       textStyle - font, size, etc. of these legends
       nchars    - max. number of characters per line;
       nlines    - max. number of lines;
       nwrap     - max. number of lines to wrap long legends;

   SEE ALSO: get_style, set_style, p_axis_style, p_line_attribs, p_legend_box,
             p_system, p_text_attribs, p_tick_style.
*/
{
  if (is_void(def)) eq_nocopy, def, P_DEFAULT_LEGEND_BOX;
  return GeLegendBox(x = (is_void(x) ? def.x : x),
                     y = (is_void(y) ? def.y : y),
                     dx = (is_void(dx) ? def.dx : dx),
                     dy = (is_void(dy) ? def.dy : dy),
                     textStyle = (is_void(textStyle) ? def.textStyle : textStyle),
                     nchars = (is_void(nchars) ? def.nchars : nchars),
                     nlines = (is_void(nlines) ? def.nlines : nlines),
                     nwrap = (is_void(nwrap) ? def.nwrap : nwrap));
}
P_DEFAULT_LEGEND_BOX = GeLegendBox(x = 0.04698,
                                   y = 0.36,
                                   dx= 0.3758,
                                   dy = 0,
                                   textStyle = GpTextAttribs(color = P_FG,
                                                             font = P_COURIER,
                                                             height = 0.0156,
                                                             orient = 0,
                                                             alignH = 1,
                                                             alignV = 1,
                                                             opaque = 0),
                                   nchars = 36,
                                   nlines = 20,
                                   nwrap = 2);
P_DEFAULT_CLEGEND_BOX = GeLegendBox(x = 0.6182, y = 0.8643, dx = 0, dy = 0,
                                    textStyle = P_DEFAULT_LEGEND_BOX.textStyle,
                                    nchars = 14,
                                    nlines = 28,
                                    nwrap = 1);
/*---------------------------------------------------------------------------*/
/* X11 COLORS */

_P_X11_COLOR_NAMES = \
["aliceblue", "antiquewhite", "antiquewhite1", "antiquewhite2",
 "antiquewhite3", "antiquewhite4", "aquamarine", "aquamarine1", "aquamarine2",
 "aquamarine3", "aquamarine4", "azure", "azure1", "azure2", "azure3", "azure4",
 "beige", "bisque", "bisque1", "bisque2", "bisque3", "bisque4",
 "blanchedalmond", "blue1", "blue2", "blue3", "blue4", "blueviolet", "brown",
 "brown1", "brown2", "brown3", "brown4", "burlywood", "burlywood1",
 "burlywood2", "burlywood3", "burlywood4", "cadetblue", "cadetblue1",
 "cadetblue2", "cadetblue3", "cadetblue4", "chartreuse", "chartreuse1",
 "chartreuse2", "chartreuse3", "chartreuse4", "chocolate", "chocolate1",
 "chocolate2", "chocolate3", "chocolate4", "coral", "coral1", "coral2",
 "coral3", "coral4", "cornflowerblue", "cornsilk", "cornsilk1", "cornsilk2",
 "cornsilk3", "cornsilk4", "cyan1", "cyan2", "cyan3", "cyan4", "darkblue",
 "darkcyan", "darkgoldenrod", "darkgoldenrod1", "darkgoldenrod2",
 "darkgoldenrod3", "darkgoldenrod4", "darkgray", "darkgreen", "darkgrey",
 "darkkhaki", "darkmagenta", "darkolivegreen", "darkolivegreen1",
 "darkolivegreen2", "darkolivegreen3", "darkolivegreen4", "darkorange",
 "darkorange1", "darkorange2", "darkorange3", "darkorange4", "darkorchid",
 "darkorchid1", "darkorchid2", "darkorchid3", "darkorchid4", "darkred",
 "darksalmon", "darkseagreen", "darkseagreen1", "darkseagreen2",
 "darkseagreen3", "darkseagreen4", "darkslateblue", "darkslategray",
 "darkslategray1", "darkslategray2", "darkslategray3", "darkslategray4",
 "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "deeppink1",
 "deeppink2", "deeppink3", "deeppink4", "deepskyblue", "deepskyblue1",
 "deepskyblue2", "deepskyblue3", "deepskyblue4", "dimgray", "dimgrey",
 "dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4",
 "firebrick", "firebrick1", "firebrick2", "firebrick3", "firebrick4",
 "floralwhite", "forestgreen", "gainsboro", "ghostwhite", "gold", "gold1",
 "gold2", "gold3", "gold4", "goldenrod", "goldenrod1", "goldenrod2",
 "goldenrod3", "goldenrod4", "gray", "gray0", "gray1", "gray10", "gray100",
 "gray11", "gray12", "gray13", "gray14", "gray15", "gray16", "gray17",
 "gray18", "gray19", "gray2", "gray20", "gray21", "gray22", "gray23", "gray24",
 "gray25", "gray26", "gray27", "gray28", "gray29", "gray3", "gray30", "gray31",
 "gray32", "gray33", "gray34", "gray35", "gray36", "gray37", "gray38",
 "gray39", "gray4", "gray40", "gray41", "gray42", "gray43", "gray44", "gray45",
 "gray46", "gray47", "gray48", "gray49", "gray5", "gray50", "gray51", "gray52",
 "gray53", "gray54", "gray55", "gray56", "gray57", "gray58", "gray59", "gray6",
 "gray60", "gray61", "gray62", "gray63", "gray64", "gray65", "gray66",
 "gray67", "gray68", "gray69", "gray7", "gray70", "gray71", "gray72", "gray73",
 "gray74", "gray75", "gray76", "gray77", "gray78", "gray79", "gray8", "gray80",
 "gray81", "gray82", "gray83", "gray84", "gray85", "gray86", "gray87",
 "gray88", "gray89", "gray9", "gray90", "gray91", "gray92", "gray93", "gray94",
 "gray95", "gray96", "gray97", "gray98", "gray99", "green1", "green2",
 "green3", "green4", "greenyellow", "grey", "grey0", "grey1", "grey10",
 "grey100", "grey11", "grey12", "grey13", "grey14", "grey15", "grey16",
 "grey17", "grey18", "grey19", "grey2", "grey20", "grey21", "grey22", "grey23",
 "grey24", "grey25", "grey26", "grey27", "grey28", "grey29", "grey3", "grey30",
 "grey31", "grey32", "grey33", "grey34", "grey35", "grey36", "grey37",
 "grey38", "grey39", "grey4", "grey40", "grey41", "grey42", "grey43", "grey44",
 "grey45", "grey46", "grey47", "grey48", "grey49", "grey5", "grey50", "grey51",
 "grey52", "grey53", "grey54", "grey55", "grey56", "grey57", "grey58",
 "grey59", "grey6", "grey60", "grey61", "grey62", "grey63", "grey64", "grey65",
 "grey66", "grey67", "grey68", "grey69", "grey7", "grey70", "grey71", "grey72",
 "grey73", "grey74", "grey75", "grey76", "grey77", "grey78", "grey79", "grey8",
 "grey80", "grey81", "grey82", "grey83", "grey84", "grey85", "grey86",
 "grey87", "grey88", "grey89", "grey9", "grey90", "grey91", "grey92", "grey93",
 "grey94", "grey95", "grey96", "grey97", "grey98", "grey99", "honeydew",
 "honeydew1", "honeydew2", "honeydew3", "honeydew4", "hotpink", "hotpink1",
 "hotpink2", "hotpink3", "hotpink4", "indianred", "indianred1", "indianred2",
 "indianred3", "indianred4", "ivory", "ivory1", "ivory2", "ivory3", "ivory4",
 "khaki", "khaki1", "khaki2", "khaki3", "khaki4", "lavender", "lavenderblush",
 "lavenderblush1", "lavenderblush2", "lavenderblush3", "lavenderblush4",
 "lawngreen", "lemonchiffon", "lemonchiffon1", "lemonchiffon2",
 "lemonchiffon3", "lemonchiffon4", "lightblue", "lightblue1", "lightblue2",
 "lightblue3", "lightblue4", "lightcoral", "lightcyan", "lightcyan1",
 "lightcyan2", "lightcyan3", "lightcyan4", "lightgoldenrod", "lightgoldenrod1",
 "lightgoldenrod2", "lightgoldenrod3", "lightgoldenrod4",
 "lightgoldenrodyellow", "lightgray", "lightgreen", "lightgrey", "lightpink",
 "lightpink1", "lightpink2", "lightpink3", "lightpink4", "lightsalmon",
 "lightsalmon1", "lightsalmon2", "lightsalmon3", "lightsalmon4",
 "lightseagreen", "lightskyblue", "lightskyblue1", "lightskyblue2",
 "lightskyblue3", "lightskyblue4", "lightslateblue", "lightslategray",
 "lightslategrey", "lightsteelblue", "lightsteelblue1", "lightsteelblue2",
 "lightsteelblue3", "lightsteelblue4", "lightyellow", "lightyellow1",
 "lightyellow2", "lightyellow3", "lightyellow4", "limegreen", "linen",
 "magenta1", "magenta2", "magenta3", "magenta4", "maroon", "maroon1",
 "maroon2", "maroon3", "maroon4", "mediumaquamarine", "mediumblue",
 "mediumorchid", "mediumorchid1", "mediumorchid2", "mediumorchid3",
 "mediumorchid4", "mediumpurple", "mediumpurple1", "mediumpurple2",
 "mediumpurple3", "mediumpurple4", "mediumseagreen", "mediumslateblue",
 "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue",
 "mintcream", "mistyrose", "mistyrose1", "mistyrose2", "mistyrose3",
 "mistyrose4", "moccasin", "navajowhite", "navajowhite1", "navajowhite2",
 "navajowhite3", "navajowhite4", "navy", "navyblue", "oldlace", "olivedrab",
 "olivedrab1", "olivedrab2", "olivedrab3", "olivedrab4", "orange", "orange1",
 "orange2", "orange3", "orange4", "orangered", "orangered1", "orangered2",
 "orangered3", "orangered4", "orchid", "orchid1", "orchid2", "orchid3",
 "orchid4", "palegoldenrod", "palegreen", "palegreen1", "palegreen2",
 "palegreen3", "palegreen4", "paleturquoise", "paleturquoise1",
 "paleturquoise2", "paleturquoise3", "paleturquoise4", "palevioletred",
 "palevioletred1", "palevioletred2", "palevioletred3", "palevioletred4",
 "papayawhip", "peachpuff", "peachpuff1", "peachpuff2", "peachpuff3",
 "peachpuff4", "peru", "pink", "pink1", "pink2", "pink3", "pink4", "plum",
 "plum1", "plum2", "plum3", "plum4", "powderblue", "purple", "purple1",
 "purple2", "purple3", "purple4", "red1", "red2", "red3", "red4", "rosybrown",
 "rosybrown1", "rosybrown2", "rosybrown3", "rosybrown4", "royalblue",
 "royalblue1", "royalblue2", "royalblue3", "royalblue4", "saddlebrown",
 "salmon", "salmon1", "salmon2", "salmon3", "salmon4", "sandybrown",
 "seagreen", "seagreen1", "seagreen2", "seagreen3", "seagreen4", "seashell",
 "seashell1", "seashell2", "seashell3", "seashell4", "sienna", "sienna1",
 "sienna2", "sienna3", "sienna4", "skyblue", "skyblue1", "skyblue2",
 "skyblue3", "skyblue4", "slateblue", "slateblue1", "slateblue2", "slateblue3",
 "slateblue4", "slategray", "slategray1", "slategray2", "slategray3",
 "slategray4", "slategrey", "snow", "snow1", "snow2", "snow3", "snow4",
 "springgreen", "springgreen1", "springgreen2", "springgreen3", "springgreen4",
 "steelblue", "steelblue1", "steelblue2", "steelblue3", "steelblue4", "tan",
 "tan1", "tan2", "tan3", "tan4", "thistle", "thistle1", "thistle2", "thistle3",
 "thistle4", "tomato", "tomato1", "tomato2", "tomato3", "tomato4", "turquoise",
 "turquoise1", "turquoise2", "turquoise3", "turquoise4", "violet", "violetred",
 "violetred1", "violetred2", "violetred3", "violetred4", "wheat", "wheat1",
 "wheat2", "wheat3", "wheat4", "whitesmoke", "yellow1", "yellow2", "yellow3",
 "yellow4", "yellowgreen"];
_P_X11_COLOR_VALUES = \
 [0x01fff8f0n, 0x01d7ebfan, 0x01dbefffn, 0x01ccdfeen, 0x01b0c0cdn, 0x0178838bn,
  0x01d4ff7fn, 0x01d4ff7fn, 0x01c6ee76n, 0x01aacd66n, 0x01748b45n, 0x01fffff0n,
  0x01fffff0n, 0x01eeeee0n, 0x01cdcdc1n, 0x018b8b83n, 0x01dcf5f5n, 0x01c4e4ffn,
  0x01c4e4ffn, 0x01b7d5een, 0x019eb7cdn, 0x016b7d8bn, 0x01cdebffn, 0x01ff0000n,
  0x01ee0000n, 0x01cd0000n, 0x018b0000n, 0x01e22b8an, 0x012a2aa5n, 0x014040ffn,
  0x013b3been, 0x013333cdn, 0x0123238bn, 0x0187b8den, 0x019bd3ffn, 0x0191c5een,
  0x017daacdn, 0x0155738bn, 0x01a09e5fn, 0x01fff598n, 0x01eee58en, 0x01cdc57an,
  0x018b8653n, 0x0100ff7fn, 0x0100ff7fn, 0x0100ee76n, 0x0100cd66n, 0x01008b45n,
  0x011e69d2n, 0x01247fffn, 0x012176een, 0x011d66cdn, 0x0113458bn, 0x01507fffn,
  0x015672ffn, 0x01506aeen, 0x01455bcdn, 0x012f3e8bn, 0x01ed9564n, 0x01dcf8ffn,
  0x01dcf8ffn, 0x01cde8een, 0x01b1c8cdn, 0x0178888bn, 0x01ffff00n, 0x01eeee00n,
  0x01cdcd00n, 0x018b8b00n, 0x018b0000n, 0x018b8b00n, 0x010b86b8n, 0x010fb9ffn,
  0x010eadeen, 0x010c95cdn, 0x0108658bn, 0x01a9a9a9n, 0x01006400n, 0x01a9a9a9n,
  0x016bb7bdn, 0x018b008bn, 0x012f6b55n, 0x0170ffcan, 0x0168eebcn, 0x015acda2n,
  0x013d8b6en, 0x01008cffn, 0x01007fffn, 0x010076een, 0x010066cdn, 0x0100458bn,
  0x01cc3299n, 0x01ff3ebfn, 0x01ee3ab2n, 0x01cd329an, 0x018b2268n, 0x0100008bn,
  0x017a96e9n, 0x018fbc8fn, 0x01c1ffc1n, 0x01b4eeb4n, 0x019bcd9bn, 0x01698b69n,
  0x018b3d48n, 0x014f4f2fn, 0x01ffff97n, 0x01eeee8dn, 0x01cdcd79n, 0x018b8b52n,
  0x014f4f2fn, 0x01d1ce00n, 0x01d30094n, 0x019314ffn, 0x019314ffn, 0x018912een,
  0x017610cdn, 0x01500a8bn, 0x01ffbf00n, 0x01ffbf00n, 0x01eeb200n, 0x01cd9a00n,
  0x018b6800n, 0x01696969n, 0x01696969n, 0x01ff901en, 0x01ff901en, 0x01ee861cn,
  0x01cd7418n, 0x018b4e10n, 0x012222b2n, 0x013030ffn, 0x012c2ceen, 0x012626cdn,
  0x011a1a8bn, 0x01f0faffn, 0x01228b22n, 0x01dcdcdcn, 0x01fff8f8n, 0x0100d7ffn,
  0x0100d7ffn, 0x0100c9een, 0x0100adcdn, 0x0100758bn, 0x0120a5dan, 0x0125c1ffn,
  0x0122b4een, 0x011d9bcdn, 0x0114698bn, 0x01bebeben, 0x01000000n, 0x01030303n,
  0x011a1a1an, 0x01ffffffn, 0x011c1c1cn, 0x011f1f1fn, 0x01212121n, 0x01242424n,
  0x01262626n, 0x01292929n, 0x012b2b2bn, 0x012e2e2en, 0x01303030n, 0x01050505n,
  0x01333333n, 0x01363636n, 0x01383838n, 0x013b3b3bn, 0x013d3d3dn, 0x01404040n,
  0x01424242n, 0x01454545n, 0x01474747n, 0x014a4a4an, 0x01080808n, 0x014d4d4dn,
  0x014f4f4fn, 0x01525252n, 0x01545454n, 0x01575757n, 0x01595959n, 0x015c5c5cn,
  0x015e5e5en, 0x01616161n, 0x01636363n, 0x010a0a0an, 0x01666666n, 0x01696969n,
  0x016b6b6bn, 0x016e6e6en, 0x01707070n, 0x01737373n, 0x01757575n, 0x01787878n,
  0x017a7a7an, 0x017d7d7dn, 0x010d0d0dn, 0x017f7f7fn, 0x01828282n, 0x01858585n,
  0x01878787n, 0x018a8a8an, 0x018c8c8cn, 0x018f8f8fn, 0x01919191n, 0x01949494n,
  0x01969696n, 0x010f0f0fn, 0x01999999n, 0x019c9c9cn, 0x019e9e9en, 0x01a1a1a1n,
  0x01a3a3a3n, 0x01a6a6a6n, 0x01a8a8a8n, 0x01abababn, 0x01adadadn, 0x01b0b0b0n,
  0x01121212n, 0x01b3b3b3n, 0x01b5b5b5n, 0x01b8b8b8n, 0x01bababan, 0x01bdbdbdn,
  0x01bfbfbfn, 0x01c2c2c2n, 0x01c4c4c4n, 0x01c7c7c7n, 0x01c9c9c9n, 0x01141414n,
  0x01ccccccn, 0x01cfcfcfn, 0x01d1d1d1n, 0x01d4d4d4n, 0x01d6d6d6n, 0x01d9d9d9n,
  0x01dbdbdbn, 0x01dededen, 0x01e0e0e0n, 0x01e3e3e3n, 0x01171717n, 0x01e5e5e5n,
  0x01e8e8e8n, 0x01ebebebn, 0x01edededn, 0x01f0f0f0n, 0x01f2f2f2n, 0x01f5f5f5n,
  0x01f7f7f7n, 0x01fafafan, 0x01fcfcfcn, 0x0100ff00n, 0x0100ee00n, 0x0100cd00n,
  0x01008b00n, 0x012fffadn, 0x01bebeben, 0x01000000n, 0x01030303n, 0x011a1a1an,
  0x01ffffffn, 0x011c1c1cn, 0x011f1f1fn, 0x01212121n, 0x01242424n, 0x01262626n,
  0x01292929n, 0x012b2b2bn, 0x012e2e2en, 0x01303030n, 0x01050505n, 0x01333333n,
  0x01363636n, 0x01383838n, 0x013b3b3bn, 0x013d3d3dn, 0x01404040n, 0x01424242n,
  0x01454545n, 0x01474747n, 0x014a4a4an, 0x01080808n, 0x014d4d4dn, 0x014f4f4fn,
  0x01525252n, 0x01545454n, 0x01575757n, 0x01595959n, 0x015c5c5cn, 0x015e5e5en,
  0x01616161n, 0x01636363n, 0x010a0a0an, 0x01666666n, 0x01696969n, 0x016b6b6bn,
  0x016e6e6en, 0x01707070n, 0x01737373n, 0x01757575n, 0x01787878n, 0x017a7a7an,
  0x017d7d7dn, 0x010d0d0dn, 0x017f7f7fn, 0x01828282n, 0x01858585n, 0x01878787n,
  0x018a8a8an, 0x018c8c8cn, 0x018f8f8fn, 0x01919191n, 0x01949494n, 0x01969696n,
  0x010f0f0fn, 0x01999999n, 0x019c9c9cn, 0x019e9e9en, 0x01a1a1a1n, 0x01a3a3a3n,
  0x01a6a6a6n, 0x01a8a8a8n, 0x01abababn, 0x01adadadn, 0x01b0b0b0n, 0x01121212n,
  0x01b3b3b3n, 0x01b5b5b5n, 0x01b8b8b8n, 0x01bababan, 0x01bdbdbdn, 0x01bfbfbfn,
  0x01c2c2c2n, 0x01c4c4c4n, 0x01c7c7c7n, 0x01c9c9c9n, 0x01141414n, 0x01ccccccn,
  0x01cfcfcfn, 0x01d1d1d1n, 0x01d4d4d4n, 0x01d6d6d6n, 0x01d9d9d9n, 0x01dbdbdbn,
  0x01dededen, 0x01e0e0e0n, 0x01e3e3e3n, 0x01171717n, 0x01e5e5e5n, 0x01e8e8e8n,
  0x01ebebebn, 0x01edededn, 0x01f0f0f0n, 0x01f2f2f2n, 0x01f5f5f5n, 0x01f7f7f7n,
  0x01fafafan, 0x01fcfcfcn, 0x01f0fff0n, 0x01f0fff0n, 0x01e0eee0n, 0x01c1cdc1n,
  0x01838b83n, 0x01b469ffn, 0x01b46effn, 0x01a76aeen, 0x019060cdn, 0x01623a8bn,
  0x015c5ccdn, 0x016a6affn, 0x016363een, 0x015555cdn, 0x013a3a8bn, 0x01f0ffffn,
  0x01f0ffffn, 0x01e0eeeen, 0x01c1cdcdn, 0x01838b8bn, 0x018ce6f0n, 0x018ff6ffn,
  0x0185e6een, 0x0173c6cdn, 0x014e868bn, 0x01fae6e6n, 0x01f5f0ffn, 0x01f5f0ffn,
  0x01e5e0een, 0x01c5c1cdn, 0x0186838bn, 0x0100fc7cn, 0x01cdfaffn, 0x01cdfaffn,
  0x01bfe9een, 0x01a5c9cdn, 0x0170898bn, 0x01e6d8adn, 0x01ffefbfn, 0x01eedfb2n,
  0x01cdc09an, 0x018b8368n, 0x018080f0n, 0x01ffffe0n, 0x01ffffe0n, 0x01eeeed1n,
  0x01cdcdb4n, 0x018b8b7an, 0x0182ddeen, 0x018becffn, 0x0182dceen, 0x0170becdn,
  0x014c818bn, 0x01d2fafan, 0x01d3d3d3n, 0x0190ee90n, 0x01d3d3d3n, 0x01c1b6ffn,
  0x01b9aeffn, 0x01ada2een, 0x01958ccdn, 0x01655f8bn, 0x017aa0ffn, 0x017aa0ffn,
  0x017295een, 0x016281cdn, 0x0142578bn, 0x01aab220n, 0x01face87n, 0x01ffe2b0n,
  0x01eed3a4n, 0x01cdb68dn, 0x018b7b60n, 0x01ff7084n, 0x01998877n, 0x01998877n,
  0x01dec4b0n, 0x01ffe1can, 0x01eed2bcn, 0x01cdb5a2n, 0x018b7b6en, 0x01e0ffffn,
  0x01e0ffffn, 0x01d1eeeen, 0x01b4cdcdn, 0x017a8b8bn, 0x0132cd32n, 0x01e6f0fan,
  0x01ff00ffn, 0x01ee00een, 0x01cd00cdn, 0x018b008bn, 0x016030b0n, 0x01b334ffn,
  0x01a730een, 0x019029cdn, 0x01621c8bn, 0x01aacd66n, 0x01cd0000n, 0x01d355ban,
  0x01ff66e0n, 0x01ee5fd1n, 0x01cd52b4n, 0x018b377an, 0x01db7093n, 0x01ff82abn,
  0x01ee799fn, 0x01cd6889n, 0x018b475dn, 0x0171b33cn, 0x01ee687bn, 0x019afa00n,
  0x01ccd148n, 0x018515c7n, 0x01701919n, 0x01fafff5n, 0x01e1e4ffn, 0x01e1e4ffn,
  0x01d2d5een, 0x01b5b7cdn, 0x017b7d8bn, 0x01b5e4ffn, 0x01addeffn, 0x01addeffn,
  0x01a1cfeen, 0x018bb3cdn, 0x015e798bn, 0x01800000n, 0x01800000n, 0x01e6f5fdn,
  0x01238e6bn, 0x013effc0n, 0x013aeeb3n, 0x0132cd9an, 0x01228b69n, 0x0100a5ffn,
  0x0100a5ffn, 0x01009aeen, 0x010085cdn, 0x01005a8bn, 0x010045ffn, 0x010045ffn,
  0x010040een, 0x010037cdn, 0x0100258bn, 0x01d670dan, 0x01fa83ffn, 0x01e97aeen,
  0x01c969cdn, 0x0189478bn, 0x01aae8een, 0x0198fb98n, 0x019aff9an, 0x0190ee90n,
  0x017ccd7cn, 0x01548b54n, 0x01eeeeafn, 0x01ffffbbn, 0x01eeeeaen, 0x01cdcd96n,
  0x018b8b66n, 0x019370dbn, 0x01ab82ffn, 0x019f79een, 0x018968cdn, 0x015d478bn,
  0x01d5efffn, 0x01b9daffn, 0x01b9daffn, 0x01adcbeen, 0x0195afcdn, 0x0165778bn,
  0x013f85cdn, 0x01cbc0ffn, 0x01c5b5ffn, 0x01b8a9een, 0x019e91cdn, 0x016c638bn,
  0x01dda0ddn, 0x01ffbbffn, 0x01eeaeeen, 0x01cd96cdn, 0x018b668bn, 0x01e6e0b0n,
  0x01f020a0n, 0x01ff309bn, 0x01ee2c91n, 0x01cd267dn, 0x018b1a55n, 0x010000ffn,
  0x010000een, 0x010000cdn, 0x0100008bn, 0x018f8fbcn, 0x01c1c1ffn, 0x01b4b4een,
  0x019b9bcdn, 0x0169698bn, 0x01e16941n, 0x01ff7648n, 0x01ee6e43n, 0x01cd5f3an,
  0x018b4027n, 0x0113458bn, 0x017280fan, 0x01698cffn, 0x016282een, 0x015470cdn,
  0x01394c8bn, 0x0160a4f4n, 0x01578b2en, 0x019fff54n, 0x0194ee4en, 0x0180cd43n,
  0x01578b2en, 0x01eef5ffn, 0x01eef5ffn, 0x01dee5een, 0x01bfc5cdn, 0x0182868bn,
  0x012d52a0n, 0x014782ffn, 0x014279een, 0x013968cdn, 0x0126478bn, 0x01ebce87n,
  0x01ffce87n, 0x01eec07en, 0x01cda66cn, 0x018b704an, 0x01cd5a6an, 0x01ff6f83n,
  0x01ee677an, 0x01cd5969n, 0x018b3c47n, 0x01908070n, 0x01ffe2c6n, 0x01eed3b9n,
  0x01cdb69fn, 0x018b7b6cn, 0x01908070n, 0x01fafaffn, 0x01fafaffn, 0x01e9e9een,
  0x01c9c9cdn, 0x0189898bn, 0x017fff00n, 0x017fff00n, 0x0176ee00n, 0x0166cd00n,
  0x01458b00n, 0x01b48246n, 0x01ffb863n, 0x01eeac5cn, 0x01cd944fn, 0x018b6436n,
  0x018cb4d2n, 0x014fa5ffn, 0x01499aeen, 0x013f85cdn, 0x012b5a8bn, 0x01d8bfd8n,
  0x01ffe1ffn, 0x01eed2een, 0x01cdb5cdn, 0x018b7b8bn, 0x014763ffn, 0x014763ffn,
  0x01425ceen, 0x01394fcdn, 0x0126368bn, 0x01d0e040n, 0x01fff500n, 0x01eee500n,
  0x01cdc500n, 0x018b8600n, 0x01ee82een, 0x019020d0n, 0x01963effn, 0x018c3aeen,
  0x017832cdn, 0x0152228bn, 0x01b3def5n, 0x01bae7ffn, 0x01aed8een, 0x0196bacdn,
  0x01667e8bn, 0x01f5f5f5n, 0x0100ffffn, 0x0100eeeen, 0x0100cdcdn, 0x01008b8bn,
  0x0132cd9an];

/*---------------------------------------------------------------------------*/
/* UTILITIES */

func p_default(&arg, def)
/* DOCUMENT p_default(arg, def);
         or p_default, arg, def;

     When called as a function, returns DEF if ARG is void; returns ARG
     otherwise.  When called as a subroutine, if ARG is undefined, it is
     redefined with the value of DEF.

   SEE ALSO: p_boolean, p_integer, p_real, p_string.
*/
{
  if (am_subroutine()) {
    if (is_void(arg)) {
      eq_nocopy, arg, def;
    }
  } else {
    return (is_void(arg) ? def : arg);
  }
}

func p_boolean(&arg, def)
/* DOCUMENT p_boolean(arg, def);
         or p_boolean, arg, def;

     When called as a function, returns a boolean value given by ARG if it is
     non-void and DEF otherwise.  When called as a subroutine, ARG is redefined
     with the result.

   SEE ALSO: p_default, p_integer, p_real, p_string.
*/
{
  local val;
  eq_nocopy, val, (is_void(arg) ? def : arg);
  if (is_array(val) && ! is_scalar(val)) {
    error, "unexpected non-scalar array value";
  }
  if (am_subroutine()) {
    arg = (val ? 1n : 0n);
  } else {
    return (val ? 1n : 0n);
  }
}

func p_integer(&arg, def)
/* DOCUMENT p_integer(arg, def);
         or p_integer, arg, def;

     When called as a function, returns a long integer value given by ARG if it
     is non-void and DEF otherwise.  When called as a subroutine, ARG is
     redefined with the result.

   SEE ALSO: p_default, p_boolean, p_real, p_string.
*/
{
  local val;
  eq_nocopy, val, (is_void(arg) ? def : arg);
  if (! is_scalar(val) || ! is_integer(val)) {
    error, "expecting an integer value";
  }
  if (am_subroutine()) {
    arg = long(val);
  } else {
    return long(val);
  }
}

func p_real(&arg, def)
/* DOCUMENT p_real(arg, def);
         or p_real, arg, def;

     When called as a function, returns a double precision floating point value
     given by ARG if it is non-void and DEF otherwise.  When called as a
     subroutine, ARG is redefined with the result.

   SEE ALSO: p_default, p_boolean, p_integer, p_string.
*/
{
  local val;
  eq_nocopy, val, (is_void(arg) ? def : arg);
  if (! is_scalar(val) || ! (is_real(val) || is_integer(val))) {
    error, "expecting a floating point value";
  }
  if (am_subroutine()) {
    arg = double(val);
  } else {
    return double(val);
  }
}

func p_string(&arg, def)
/* DOCUMENT p_string(arg, def);
         or p_string, arg, def;

     When called as a function, returns a scalar string value given by ARG if
     it is non-void and DEF otherwise.  When called as a subroutine, ARG is
     redefined with the result.

   SEE ALSO: p_default, p_boolean, p_integer, p_real.
*/
{
  local val;
  use_default = is_void(arg);
  eq_nocopy, val, (use_default ? def : arg);
  if (! is_scalar(val) || ! is_string(val)) {
    error, "expecting a floating point value";
  }
  if (am_subroutine()) {
    if (use_default) {
      eq_nocopy, arg, val;
    }
  } else {
    return val;
  }
}

func p_real_vector(&a, n, name)
/* DOCUMENT na = p_real_vector(a, n, name);

     Make argument A into a vector of reals and return its length.  If N > 0,
     the vector length must be N (if A is a scalar, it is duplicated N times).
     Argument NAME is used for error messages.

   SEE ALSO: is_vector.
 */
{
  if ((type = identof(a)) <= Y_DOUBLE) {
    if (is_vector(a)) {
      na = numberof(a);
      if (n > 0 && na != n) {
        error, ("incompatible dimensions for "
                + (is_void(name) ? "argument" : name));
      }
      if (type != Y_DOUBLE) a = double(a);
      return na;
    } else if (is_scalar(a)) {
      a = array(double(a), max(1, n));
      return numberof(a);
    }
  }
  error, ((is_void(name) ? "argument" : name)
          + " must be  a 1-D real/integer array");
}

local p_has_member, p_get_member;
/* DOCUMENT tst = p_has_member(grp, key);
         or val = p_get_member(grp, key);

     The `p_has_member()` function checks whether KEY is a member of Yorick
     group object GRP.

     The `p_get_member()` function returns the value VAL stored in Yorick group
     object GRP for the string KEY.  The result is the same as GRP(noop(KEY))
     except that an empty result is returned if the key is not found (or if GRP
     is not a group object).

   SEE ALSO: save, oxy, is_obj.
 */
func p_has_member(grp, key)
{
  return (is_obj(grp, noop(key), 1n) >= 0n);
}
func p_get_member(grp, key)
{
  if (is_obj(grp, noop(key), 1n) != -1n) {
    return grp(noop(key));
  }
}

func p_span(a, b, n)
/* DOCUMENT p_span(a, b, n);

     The function p_span() returns an array of N doubles equally spaced from A
     to B.  This function is more simple than built-in span() -- in particular,
     A and B must be scalars -- but avoids rounding errors.

   SEE ALSO: span.
 */
{
  a += 0.0; // make sure A is floating point
  if (a == b || n == 1) return array(a, n);
  return ((b - a)/(n - 1.0))*(indgen(n) - (n + 1)/2.0) + (a + b)/2.0;
}

local p_printf, p_fprintf, p_sprintf, p_warn, p_error;
/* DOCUMENT n = p_printf(fmt, ...);
         or n = p_fprintf(file, fmt, ...);
         or str = p_sprintf(file, fmt, ...);
         or p_warn, fmt, ...;
         or p_error, fmt, ...;

     The functions p_printf, p_fprintf and p_sprintf micimics the C functions
     printf, fprintf and sprintf: p_printf writes a formatted message to
     standard output, p_fprintf writes a formatted message to the text stream
     FILE and p_sprintf returns a formatted string.  The format is specified by
     FMT which is a string with "%" directives for each other arguments.

     The subroutines p_warn and p_error are simple wrappers to print warnings
     or raise errors with a formatted message.

   SEE ALSO: write, swrite, error.
 */
func p_printf(a) /* DOCUMENTED */
{
  return _p_printf(a);
}
errs2caller, p_printf;
wrap_args, p_printf;

func p_fprintf(a) /* DOCUMENTED */
{
  return _p_fprintf(a);
}
errs2caller, p_fprintf;
wrap_args, p_fprintf;

func p_sprintf(a) /* DOCUMENTED */
{
  return _p_sprintf(a);
}
errs2caller, p_sprintf;
wrap_args, p_sprintf;

func p_warn(a) /* DOCUMENTED */
{
  write, format="WARNING - %s\n", _p_sprintf(a);
}
errs2caller, p_warn;
wrap_args, p_warn;

func p_error(a) /* DOCUMENTED */
{
  error, _p_sprintf(a);
}
errs2caller, p_error;
wrap_args, p_error;

func _p_printf(a) /* PRIVATE */
{
  n = a(*);
  if (n ==  1) return write(linesize=999, format="%s", a(1));
  if (n ==  2) return write(linesize=999, format=a(1), a(2));
  if (n ==  3) return write(linesize=999, format=a(1), a(2), a(3));
  if (n ==  4) return write(linesize=999, format=a(1), a(2), a(3), a(4));
  if (n ==  5) return write(linesize=999, format=a(1), a(2), a(3), a(4), a(5));
  if (n ==  6) return write(linesize=999, format=a(1), a(2), a(3), a(4), a(5), a(6));
  if (n ==  7) return write(linesize=999, format=a(1), a(2), a(3), a(4), a(5), a(6), a(7));
  if (n ==  8) return write(linesize=999, format=a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8));
  if (n ==  9) return write(linesize=999, format=a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9));
  if (n == 10) return write(linesize=999, format=a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10));
  error, (n < 1 ? "too few arguments" : "too many arguments");
}

func _p_fprintf(a) /* PRIVATE */
{
  n = a(*);
  if (n ==  2) return write(a(1), linesize=999, format="%s", a(2));
  if (n ==  3) return write(a(1), linesize=999, format=a(2), a(3));
  if (n ==  4) return write(a(1), linesize=999, format=a(2), a(3), a(4));
  if (n ==  5) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5));
  if (n ==  6) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5), a(6));
  if (n ==  7) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5), a(6), a(7));
  if (n ==  8) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5), a(6), a(7), a(8));
  if (n ==  9) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9));
  if (n == 10) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10));
  if (n == 11) return write(a(1), linesize=999, format=a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11));
  error, (n < 2 ? "too few arguments" : "too many arguments");
}

func _p_sprintf(a) /* PRIVATE */
{
  n = a(*);
  if (n ==  1) return swrite(format="%s", a(1));
  if (n ==  2) return swrite(format=a(1), a(2));
  if (n ==  3) return swrite(format=a(1), a(2), a(3));
  if (n ==  4) return swrite(format=a(1), a(2), a(3), a(4));
  if (n ==  5) return swrite(format=a(1), a(2), a(3), a(4), a(5));
  if (n ==  6) return swrite(format=a(1), a(2), a(3), a(4), a(5), a(6));
  if (n ==  7) return swrite(format=a(1), a(2), a(3), a(4), a(5), a(6), a(7));
  if (n ==  8) return swrite(format=a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8));
  if (n ==  9) return swrite(format=a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9));
  if (n == 10) return swrite(format=a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10));
  error, (n < 1 ? "too few arguments" : "too many arguments");
}

/*---------------------------------------------------------------------------*/
/* DEAL WITH LOG SCALES (this implies replacing `logxy` and `limits` commands
   by specific wrappers). */

/*
 * For labeling algorithm, see: Talbot, J., Lin, S. & Hanrahan, P. "An
 * Extension of Wilkinson’s Algorithm for Positioning Tick Labels on Axes,"
 * IEEE VGTC sponsored conference proceedings, (2010)
 * http://www.justintalbot.com/research/axis-labeling/
 *
 * Log axes should have major ticks at integer power of 10 (no none should
 * be allowed to do a log-plot if the range is less than one decade because
 * the plot would look almost the same in linear scale FIXME: CHECK THIS)
 * and at most one level of minor ticks at multiples {2,3,4,5,6,7,8,9}.
 *
 * For linearly scaled axes, having a label at zero should be favored.  It
 * is not possible to have such a label for logarithmically scaled axes.
 */

P_LIM_XMIN     = 0x001;
P_LIM_XMAX     = 0x002;
P_LIM_YMIN     = 0x004;
P_LIM_YMAX     = 0x008;
P_LIM_RESTRICT = 0x010;
P_LIM_NICE     = 0x020;
P_LIM_SQUARE   = 0x040;
P_LIM_LOGX     = 0x080;
P_LIM_LOGY     = 0x100;
P_LIM_ZOOMED   = 0x200;
func _p_limits(xmin, xmax, ymin, ymax, square=, nice=, restrict=)
/* DOCUMENT limits
         or limits, xmin, xmax, ymin, ymax,
                    square=0/1, nice=0/1, restrict=0/1
         or old_limits = limits()
         or limits, old_limits

     In the first form, restores all four plot limits to extreme values.

     In the second form, sets the plot limits in the current coordinate system
     to XMIN, XMAX, YMIN, YMAX, which may be nil or omitted to leave the
     corresponding limit unchanged, a number to fix the corresponding limit to
     a specified value, or the string "e" to make the corresponding limit take
     on the extreme value of the currently displayed data.

     If present, the square keyword determines whether limits marked as extreme
     values will be adjusted to force the x and y scales to be equal (square=1)
     or not (square=0, the default).  If present, the nice keyword determines
     whether limits will be adjusted to nice values (nice=1) or not (nice=0,
     the default).  There is a subtlety in the meaning of "extreme value" when
     one or both of the limits on the OPPOSITE axis have fixed values -- does
     the "extreme value" of the data include points which will not be plotted
     because their other coordinate lies outside the fixed limit on the
     opposite axis (restrict=0, the default), or not (restrict=1)?

     If called as a function, limits returns an array of 5 doubles;
     OLD_LIMITS(1:4) are the current xmin, xmax, ymin, and ymax, and
     int(OLD_LIMITS(5)) is a set of flags indicating extreme values and the
     square, nice, restrict, and log flags.

     In the fourth form, OLD_LIMITS is as returned by a previous limits call,
     to restore the limits to a previous state.

     In an X window, the limits may also be adjusted interactively with the
     mouse.  Drag left to zoom in and pan (click left to zoom in on a point
     without moving it), drag middle to pan, and click (and drag) right to zoom
     out (and pan).  If you click just above or below the plot, these
     operations will be restricted to the x-axis; if you click just to the left
     or right, the operations are restricted to the y-axis.  A ctrl-left click,
     drag, and release will expand the box you dragged over to fill the plot
     (other popular software zooms with this paradigm).  If the rubber band box
     is not visible with ctrl-left zooming, try ctrl-middle or ctrl-right for
     alternate XOR masks.  Such mouse-set limits are equivalent to a limits
     command specifying all four limits EXCEPT that the unzoom command can
     revert to the limits before a series of mouse zooms and pans.

     Holding the shift key and pressing the left mouse button is equivalent to
     pressing the middle mouse button.  Similarly, pressing meta-left is
     equivalent to the right button.  This permits access to the middle and
     right button functions on machines (e.g.- most laptops) with two button or
     one button mice.

     The limits you set using the limits or range functions carry over to the
     next plot -- that is, an fma operation does NOT reset the limits to
     extreme values.

   SEE ALSO: plsys, range, logxy, zoom_factor, unzoom, plg, viewport
 */
{
  /* In order to mimic the builtin "limits" function, it must be called in
     a number of different ways depending on which arguments and keywords
     are defined. */
  local old_limits;
  if (am_subroutine()) {
    if (is_void(xmax) && is_void(xmin) && is_void(ymin) && is_void(ymax)) {
      if (is_void(square) && is_void(nice) && is_void(restrict)) {
        _p_builtin_limits;
      } else {
      _p_builtin_limits, square=square, nice=nice, restrict=restrict;
      }
    } else {
      _p_builtin_limits, xmin, xmax, ymin, ymax,
        square=square, nice=nice, restrict=restrict;
    }
  } else {
    old_limits = _p_builtin_limits(xmin, xmax, ymin, ymax, square=square,
                                   nice=nice, restrict=restrict);
  }
  if (numberof(xmin) == 5) {
    new_flags = long(xmin(5));
    old_flags = long(old_limits(5));
    xflag = (new_flags & P_LIM_LOGX);
    yflag = (new_flags & P_LIM_LOGY);
    if ((old_flags & P_LIM_LOGX) != xflag ||
        (old_flags & P_LIM_LOGY) != yflag) {
      _p_logxy, xflag, yflag;
    }
  }
  return old_limits;
}
errs2caller, _p_limits;
limits = _p_limits;

_P_LIN_TICKS_RELATIVE_LENGTH = [1.0, 0.64, 0.36, 0.18, 0.09];
_P_LOG_TICKS_RELATIVE_LENGTH = [1.0, 0.0, 0.0, 0.0, 0.0];
func _p_logxy(xflag, yflag)
/* DOCUMENT logxy, xflag, yflag;

     Sets the linear/log axis scaling flags for the current coordinate system.
     XFLAG and YFLAG may be nil or omitted to leave the corresponding axis
     scaling unchanged, 0 to select linear scaling, or 1 to select log scaling.

    SEE ALSO: plsys, limits, range, plg, gridxy
 */
{
  local land, gsys, legs, clegs;
  get_style, land, gsys, legs, clegs;
  n = plsys();
  set = 0n;
  if (1 <= n && n <= numberof(gsys)) {
    ticks = gsys(n).ticks;
    if (ticks.horiz.logAdjMinor != 1.1) {
      ticks.horiz.logAdjMinor = 1.1;
      set = 1n;
    }
    if (ticks.vert.logAdjMinor != 1.1) {
      ticks.vert.logAdjMinor = 1.1;
      set = 1n;
    }
    len = (xflag ? _P_LOG_TICKS_RELATIVE_LENGTH :
           _P_LIN_TICKS_RELATIVE_LENGTH)*ticks.horiz.tickLen(1);
    if (anyof(ticks.horiz.tickLen != len)) {
      ticks.horiz.tickLen = len;
      set = 1n;
    }
    len = (yflag ? _P_LOG_TICKS_RELATIVE_LENGTH :
           _P_LIN_TICKS_RELATIVE_LENGTH)*ticks.vert.tickLen(1);
    if (anyof(ticks.vert.tickLen != len)) {
      ticks.vert.tickLen = len;
      set = 1n;
    }
    if (set) {
      gsys(n).ticks = ticks;
      set_style, land, gsys, legs, clegs;
    }
  }
  _p_builtin_logxy, xflag, yflag;
}
errs2caller, _p_logxy;
logxy = _p_logxy;

/*---------------------------------------------------------------------------*/
/* INITIALIZATION */

if (is_void(P_DEBUG)) P_DEBUG = P_FALSE;
func _p_init
{
  /* Build the database of Gist colors. */
  names = ["bg", "fg", "black", "white", "red", "green", "blue", "cyan",
           "magenta", "yellow", "grayd", "grayc", "grayb", "graya",
           "extra", "xor"];
  n = numberof(names);
  for (i = 1; i <= n; ++i) {
    name = names(i);
    value = symbol_def("P_" + strcase(1n, name));
    p_add_named_color, name, value;
  }

  /* Build the X11 color database (and destroy the original arrays to save some
     memory). */
  extern _P_X11_COLOR_NAMES, _P_X11_COLOR_VALUES;
  p_add_named_colors, _P_X11_COLOR_NAMES, _P_X11_COLOR_VALUES;
  if (! P_DEBUG) {
    _P_X11_COLOR_NAMES = [];
    _P_X11_COLOR_VALUES = [];
  }

  /* Build the database of fonts. */
  extern _P_FONT_TABLE;
  names = ["courier", "times", "helvetica", "symbol", "schoolbook"];
  _P_FONT_TABLE = save();
  n = numberof(names);
  for (i = 1; i <= n; ++i) {
    name = names(i);
    value = symbol_def("P_" + strcase(1n, name));
    save, _P_FONT_TABLE,
      noop(name),   value,
      name + "I",  (value | P_ITALIC),
      name + "B",  (value | P_BOLD),
      name + "BI", (value | P_BOLD | P_ITALIC),
      name + "IB", (value | P_BOLD | P_ITALIC);
  }

  /* Build the justify table. */
  extern _P_JUSTIFY_TABLE;
  horiz = save("N", P_NORMAL, "L", P_LEFT, "C", P_CENTER, "R", P_RIGHT);
  vert = save("N", P_NORMAL, "T", P_TOP, "C", P_CAPLINE, "H", P_HALF,
              "A", P_BASELINE, "B", P_BOTTOM);
  hcount = horiz(*);
  hkeys = horiz(*,);
  vcount = vert(*);
  vkeys = vert(*,);
  _P_JUSTIFY_TABLE = save();
  for (i = 1; i <= hcount; ++i) {
    h = hkeys(i);
    for (i = 1; i <= vcount; ++i) {
      v = vkeys(i);
      save, _P_JUSTIFY_TABLE, h + v, (horiz(noop(h)) | vert(noop(v)));
    }
  }

  if (! P_DEBUG) {
    errs2caller, p_color, p_icolor, p_parse_color, p_font, p_type,
      p_symbol, p_marker, p_default, p_string, p_boolean, p_integer,
      p_real, p_real_vector;
  }
}
_p_init;

/*---------------------------------------------------------------------------*/
