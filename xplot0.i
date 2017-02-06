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
if (is_func(limits)   == 2) _p_orig_limits   = limits;
if (is_func(range)    == 2) _p_orig_range    = range;
if (is_func(viewport) == 2) _p_orig_viewport = viewport;
if (is_func(logxy)    == 2) _p_orig_logxy    = logxy;
if (is_func(gridxy)   == 2) _p_orig_gridxy   = gridxy;
if (is_func(plsys)    == 2) _p_orig_plsys    = plsys;
if (is_func(window)   == 2) _p_orig_window   = window;

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

local _P_LABEL_MULTIPLIERS, _P_LABEL_MULTIPLY, _P_LABEL_OPTIONS;
local P_LABEL_SIGN, P_LABEL_ZERO, P_LABEL_POW10, P_LABEL_TRIM;
func p_labels(a, b, n, eps=, ndig=, opt=, mult=)
/* DOCUMENT ptr = p_labels(a, b, n);

     Make nice labels in the range [A,B] for graphics.  N is the aproximative
     number of labels and D is the minimum number of significant digits (D = 5
     if unspecified).  The result is an array of 2 pointers:

         *ptr(1) = label values;
         *ptr(2) = label strings;

     If A <= B, the returned values are in ascending order; in descending order
     otherwise.

     Keyword NDIG may be set to specify the minimum number of significant
     digits.

     Keyword EPS (default 1e-5) may be set with a small nonnegative relative
     tolerance used to favor nice values near the endpoints but perhaps
     slightly outside the interval.  EPS should be small enough that the effect
     is unnoticeable at the resolution of the graphic device.

     Keyword OPT may be set with a combination of bits to format the labels.
     Set bit P_LABEL_SIGN to force the sign of the mantissa; set P_LABEL_ZERO
     to strip exponent for zero matissa (also strip trailing zeros for the
     corresponding label); set P_LABEL_POW10 to force the power of ten to be
     displayed.

     Keyword MULT can be set with the string used to indicate multiplication by
     the power of ten.  The default value is " x " where the 'x' is the
     ISO-8859-1 code for the multiply sign.

   SEE ALSO pl_cbar.
 */
{
  if (is_void(opt)) opt = _P_LABEL_OPTIONS;
  a += 0.0;
  b += 0.0;
  reverse = (a > b);
  if (reverse) {
    swap, a, b;
  }
  if (a == b) {
    /* A single label is returned. */
    if (a == 0) {
      p = 0;
      val = 0.0;
      str = "0";
      return [&val, &str];
    } else {
      d = (is_void(ndig) ? 5 : ndig);
      p = long(floor(log10(abs(a))));
      r = 10.0^(d - p);
      val = round(a*r)/r;
    }
  } else {
    if (is_void(eps)) eps = 1e-5;
    h = (b - a)*eps;
    a -= h;
    b += h;
    if (n >= 3) {
      /* Try to find solution with N ≥ 3 labels. */
      best_n = 0;
      best_q = 0;
      best_r = 0;
      for (i = numberof(_P_LABEL_MULTIPLIERS); i >= 1; --i) {
        s = double(_P_LABEL_MULTIPLIERS(i));
        q = long(floor(log10((b - a)/(2*s))));
        for (;;) {
          r = s*10.0^q;
          np = long(floor(b/r) - ceil(a/r)) + 1;
          if (best_n == 0 || abs(n - np) < abs(n - best_n)) {
            best_n = np;
            best_q = q;
            best_r = r;
          }
          if (np > n) {
            break;
          }
          --q;
        }
      }
      n = best_n;
      if (n >= 3) {
        q = best_q;
        r = best_r;
        val = (indgen(0:n-1) + ceil(a/r))*r;
        p = long(floor(log10(max(abs(val)))));
        d = p - q + 1;
        if (! is_void(ndig) && ndig > d) {
          d = ndig;
        }
      }
    }
    if (n < 3) {
      /* Find a solution with 2 labels and at least 1 significant digit. */
      d = (is_void(ndig) || ndig <= 1 ? 1 : ndig);
      if (a <= 0 && b >= 0) {
        /* Zero belongs to [A,B] */
        if (abs(a) < abs(b)) {
          x1 = 0.0;
          p = long(floor(log10(abs(b))));
          r = 10.0^(p - d + 1);
          x2 = floor(b/r)*r;
        } else {
          p = long(floor(log10(abs(a))));
          r = 10.0^(p - d + 1);
          x1 = ceil(a/r)*r;
          x2 = 0.0;
        }
      } else {
        /* Find the least number of significant digits for which an uppper
           approximation of A and a lower approximation of B are different. */
        p = long(floor(log10(max(abs(a),abs(b)))));
        for (;;) {
          r = 10.0^(p - d + 1);
          ap = ceil(a/r);
          bp = floor(b/r);
          if (ap < bp) {
            x1 = ap*r;
            x2 = bp*r;
            break;
          }
          ++d;
        }
      }
      val = [x1, x2];
    }
  }

  /* Reorder the values. */
  if (reverse) {
    val = val(::-1);
  }

  /* Format the labels. */
  sgn = ((opt & P_LABEL_SIGN) != 0 ? "+" : "")
  if ((opt & P_LABEL_POW10) != 0 || p < -3 || p > 3) {
    /* Make labels with a power of 10. */
    if (is_void(mult)) {
      eq_nocopy, mult, _P_LABEL_MULTIPLY;
    }
    fmt = swrite(format="%%%s.%df%s10^%d", sgn, max(0, d - 1), mult, p);
    str = swrite(format=fmt, val*10.0^(-p));
  } else {
    fmt = swrite(format="%%%s.%df", sgn, max(0, d - 1 - p));
    str = swrite(format=fmt, val);
  }
  if ((opt & P_LABEL_ZERO) != 0) {
    j = where(! val);
    if (is_array(j)) {
      str(j) = ((opt & P_LABEL_SIGN) != 0 ? " 0" : "0");
    }
  }
  return [&val, &str];
}
P_LABEL_SIGN  = (1 << 0); // force the sign of the mantissa
P_LABEL_ZERO  = (1 << 1); // the label for zero is just "0"
P_LABEL_POW10 = (1 << 2); // for the power of 10 to be displayed
P_LABEL_TRIM  = (1 << 3); // remove trailing zeros from the mantissa
_P_LABEL_OPTIONS = (P_LABEL_ZERO);
_P_LABEL_MULTIPLIERS = [1,2,5];
_P_LABEL_MULTIPLY = strchar([' ', P_ISO_8859_1_MULTIPLY, ' ']);

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
     levels in hexadecimal), X11 or SVG colors as "firebrick" (case is
     irrelevant), and Gist colors as "magenta".  A packed RGB color is a 32-bit
     integer built as:

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

func p_add_gist_colors
/* DOCUMENT p_add_gist_colors;

     Add the named colors recognized by Gist in the global database.

   SEE ALSO: p_add_named_colors, p_add_svg_colors, p_add_x11_colors;
*/
{
  p_add_named_colors, "bg",P_BG, "fg",P_FG, "black",P_BLACK, "white",P_WHITE,
    "red",P_RED, "green",P_GREEN, "blue",P_BLUE, "cyan",P_CYAN,
    "magenta",P_MAGENTA, "yellow",P_YELLOW, "grayd",P_GRAYD, "grayc",P_GRAYC,
    "grayb",P_GRAYB, "graya",P_GRAYA, "extra",P_EXTRA, "xor",P_XOR;
}

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
         or p_add_named_colors, names, values, ...;

     These subroutines add new named color(s) to the global database.  The
     first routine can only accept a single color definition, the second
     routine be used to define many colors.

   SEE ALSO p_parse_color.
*/

if (is_void(_P_COLOR_TABLE)) _P_COLOR_TABLE = save();

func p_add_named_color(name, value)
{
  save, _P_COLOR_TABLE, strcase(0n, name), value;
}

func p_add_named_colors(..)
{
  local names, values;
  while (more_args()) {
    eq_nocopy, names, next_arg();
    eq_nocopy, values, (more_args() ? next_arg() : []);
    n = numberof(names);
    if (numberof(values) != n) {
      error, "there must be as many color values as names";
    }
    for (i = 1; i <= n; ++i) {
      p_add_named_color, names(i), values(i);
    }
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
  if (is_void(sys)) return _p_orig_viewport();
  old = plsys(sys);
  res = _p_orig_viewport();
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

func p_add_x11_colors
/* DOCUMENT p_add_x11_colors;

     Add the named colors of the X11 database in the global database.

   SEE ALSO: p_add_named_colors, p_add_gist_colors, p_add_svg_colors;
*/
{
  p_add_named_colors, "aliceblue",0x01fff8f0n, "antiquewhite",0x01d7ebfan,
    "antiquewhite1",0x01dbefffn, "antiquewhite2",0x01ccdfeen,
    "antiquewhite3",0x01b0c0cdn, "antiquewhite4",0x0178838bn,
    "aquamarine",0x01d4ff7fn, "aquamarine1",0x01d4ff7fn,
    "aquamarine2",0x01c6ee76n, "aquamarine3",0x01aacd66n,
    "aquamarine4",0x01748b45n, "azure",0x01fffff0n, "azure1",0x01fffff0n,
    "azure2",0x01eeeee0n, "azure3",0x01cdcdc1n, "azure4",0x018b8b83n,
    "beige",0x01dcf5f5n, "bisque",0x01c4e4ffn, "bisque1",0x01c4e4ffn,
    "bisque2",0x01b7d5een, "bisque3",0x019eb7cdn, "bisque4",0x016b7d8bn,
    "blanchedalmond",0x01cdebffn, "blue1",0x01ff0000n, "blue2",0x01ee0000n,
    "blue3",0x01cd0000n, "blue4",0x018b0000n, "blueviolet",0x01e22b8an,
    "brown",0x012a2aa5n, "brown1",0x014040ffn, "brown2",0x013b3been,
    "brown3",0x013333cdn, "brown4",0x0123238bn, "burlywood",0x0187b8den,
    "burlywood1",0x019bd3ffn, "burlywood2",0x0191c5een,
    "burlywood3",0x017daacdn, "burlywood4",0x0155738bn,
    "cadetblue",0x01a09e5fn, "cadetblue1",0x01fff598n,
    "cadetblue2",0x01eee58en, "cadetblue3",0x01cdc57an,
    "cadetblue4",0x018b8653n, "chartreuse",0x0100ff7fn,
    "chartreuse1",0x0100ff7fn, "chartreuse2",0x0100ee76n,
    "chartreuse3",0x0100cd66n, "chartreuse4",0x01008b45n,
    "chocolate",0x011e69d2n, "chocolate1",0x01247fffn,
    "chocolate2",0x012176een, "chocolate3",0x011d66cdn,
    "chocolate4",0x0113458bn, "coral",0x01507fffn, "coral1",0x015672ffn,
    "coral2",0x01506aeen, "coral3",0x01455bcdn, "coral4",0x012f3e8bn,
    "cornflowerblue",0x01ed9564n, "cornsilk",0x01dcf8ffn,
    "cornsilk1",0x01dcf8ffn, "cornsilk2",0x01cde8een, "cornsilk3",0x01b1c8cdn,
    "cornsilk4",0x0178888bn, "cyan1",0x01ffff00n, "cyan2",0x01eeee00n,
    "cyan3",0x01cdcd00n, "cyan4",0x018b8b00n, "darkblue",0x018b0000n,
    "darkcyan",0x018b8b00n, "darkgoldenrod",0x010b86b8n,
    "darkgoldenrod1",0x010fb9ffn, "darkgoldenrod2",0x010eadeen,
    "darkgoldenrod3",0x010c95cdn, "darkgoldenrod4",0x0108658bn,
    "darkgray",0x01a9a9a9n, "darkgreen",0x01006400n, "darkgrey",0x01a9a9a9n,
    "darkkhaki",0x016bb7bdn, "darkmagenta",0x018b008bn,
    "darkolivegreen",0x012f6b55n, "darkolivegreen1",0x0170ffcan,
    "darkolivegreen2",0x0168eebcn, "darkolivegreen3",0x015acda2n,
    "darkolivegreen4",0x013d8b6en, "darkorange",0x01008cffn,
    "darkorange1",0x01007fffn, "darkorange2",0x010076een,
    "darkorange3",0x010066cdn, "darkorange4",0x0100458bn,
    "darkorchid",0x01cc3299n, "darkorchid1",0x01ff3ebfn,
    "darkorchid2",0x01ee3ab2n, "darkorchid3",0x01cd329an,
    "darkorchid4",0x018b2268n, "darkred",0x0100008bn, "darksalmon",0x017a96e9n,
    "darkseagreen",0x018fbc8fn, "darkseagreen1",0x01c1ffc1n,
    "darkseagreen2",0x01b4eeb4n, "darkseagreen3",0x019bcd9bn,
    "darkseagreen4",0x01698b69n, "darkslateblue",0x018b3d48n,
    "darkslategray",0x014f4f2fn, "darkslategray1",0x01ffff97n,
    "darkslategray2",0x01eeee8dn, "darkslategray3",0x01cdcd79n,
    "darkslategray4",0x018b8b52n, "darkslategrey",0x014f4f2fn,
    "darkturquoise",0x01d1ce00n, "darkviolet",0x01d30094n,
    "deeppink",0x019314ffn, "deeppink1",0x019314ffn, "deeppink2",0x018912een,
    "deeppink3",0x017610cdn, "deeppink4",0x01500a8bn,
    "deepskyblue",0x01ffbf00n, "deepskyblue1",0x01ffbf00n,
    "deepskyblue2",0x01eeb200n, "deepskyblue3",0x01cd9a00n,
    "deepskyblue4",0x018b6800n, "dimgray",0x01696969n, "dimgrey",0x01696969n,
    "dodgerblue",0x01ff901en, "dodgerblue1",0x01ff901en,
    "dodgerblue2",0x01ee861cn, "dodgerblue3",0x01cd7418n,
    "dodgerblue4",0x018b4e10n, "firebrick",0x012222b2n,
    "firebrick1",0x013030ffn, "firebrick2",0x012c2ceen,
    "firebrick3",0x012626cdn, "firebrick4",0x011a1a8bn,
    "floralwhite",0x01f0faffn, "forestgreen",0x01228b22n,
    "gainsboro",0x01dcdcdcn, "ghostwhite",0x01fff8f8n, "gold",0x0100d7ffn,
    "gold1",0x0100d7ffn, "gold2",0x0100c9een, "gold3",0x0100adcdn,
    "gold4",0x0100758bn, "goldenrod",0x0120a5dan, "goldenrod1",0x0125c1ffn,
    "goldenrod2",0x0122b4een, "goldenrod3",0x011d9bcdn,
    "goldenrod4",0x0114698bn, "gray",0x01bebeben, "green1",0x0100ff00n,
    "green2",0x0100ee00n, "green3",0x0100cd00n, "green4",0x01008b00n,
    "greenyellow",0x012fffadn, "grey",0x01bebeben, "honeydew",0x01f0fff0n,
    "honeydew1",0x01f0fff0n, "honeydew2",0x01e0eee0n, "honeydew3",0x01c1cdc1n,
    "honeydew4",0x01838b83n, "hotpink",0x01b469ffn, "hotpink1",0x01b46effn,
    "hotpink2",0x01a76aeen, "hotpink3",0x019060cdn, "hotpink4",0x01623a8bn,
    "indianred",0x015c5ccdn, "indianred1",0x016a6affn,
    "indianred2",0x016363een, "indianred3",0x015555cdn,
    "indianred4",0x013a3a8bn, "ivory",0x01f0ffffn, "ivory1",0x01f0ffffn,
    "ivory2",0x01e0eeeen, "ivory3",0x01c1cdcdn, "ivory4",0x01838b8bn,
    "khaki",0x018ce6f0n, "khaki1",0x018ff6ffn, "khaki2",0x0185e6een,
    "khaki3",0x0173c6cdn, "khaki4",0x014e868bn, "lavender",0x01fae6e6n,
    "lavenderblush",0x01f5f0ffn, "lavenderblush1",0x01f5f0ffn,
    "lavenderblush2",0x01e5e0een, "lavenderblush3",0x01c5c1cdn,
    "lavenderblush4",0x0186838bn, "lawngreen",0x0100fc7cn,
    "lemonchiffon",0x01cdfaffn, "lemonchiffon1",0x01cdfaffn,
    "lemonchiffon2",0x01bfe9een, "lemonchiffon3",0x01a5c9cdn,
    "lemonchiffon4",0x0170898bn, "lightblue",0x01e6d8adn,
    "lightblue1",0x01ffefbfn, "lightblue2",0x01eedfb2n,
    "lightblue3",0x01cdc09an, "lightblue4",0x018b8368n,
    "lightcoral",0x018080f0n, "lightcyan",0x01ffffe0n,
    "lightcyan1",0x01ffffe0n, "lightcyan2",0x01eeeed1n,
    "lightcyan3",0x01cdcdb4n, "lightcyan4",0x018b8b7an,
    "lightgoldenrod",0x0182ddeen, "lightgoldenrod1",0x018becffn,
    "lightgoldenrod2",0x0182dceen, "lightgoldenrod3",0x0170becdn,
    "lightgoldenrod4",0x014c818bn, "lightgoldenrodyellow",0x01d2fafan,
    "lightgray",0x01d3d3d3n, "lightgreen",0x0190ee90n, "lightgrey",0x01d3d3d3n,
    "lightpink",0x01c1b6ffn, "lightpink1",0x01b9aeffn,
    "lightpink2",0x01ada2een, "lightpink3",0x01958ccdn,
    "lightpink4",0x01655f8bn, "lightsalmon",0x017aa0ffn,
    "lightsalmon1",0x017aa0ffn, "lightsalmon2",0x017295een,
    "lightsalmon3",0x016281cdn, "lightsalmon4",0x0142578bn,
    "lightseagreen",0x01aab220n, "lightskyblue",0x01face87n,
    "lightskyblue1",0x01ffe2b0n, "lightskyblue2",0x01eed3a4n,
    "lightskyblue3",0x01cdb68dn, "lightskyblue4",0x018b7b60n,
    "lightslateblue",0x01ff7084n, "lightslategray",0x01998877n,
    "lightslategrey",0x01998877n, "lightsteelblue",0x01dec4b0n,
    "lightsteelblue1",0x01ffe1can, "lightsteelblue2",0x01eed2bcn,
    "lightsteelblue3",0x01cdb5a2n, "lightsteelblue4",0x018b7b6en,
    "lightyellow",0x01e0ffffn, "lightyellow1",0x01e0ffffn,
    "lightyellow2",0x01d1eeeen, "lightyellow3",0x01b4cdcdn,
    "lightyellow4",0x017a8b8bn, "limegreen",0x0132cd32n, "linen",0x01e6f0fan,
    "magenta1",0x01ff00ffn, "magenta2",0x01ee00een, "magenta3",0x01cd00cdn,
    "magenta4",0x018b008bn, "maroon",0x016030b0n, "maroon1",0x01b334ffn,
    "maroon2",0x01a730een, "maroon3",0x019029cdn, "maroon4",0x01621c8bn,
    "mediumaquamarine",0x01aacd66n, "mediumblue",0x01cd0000n,
    "mediumorchid",0x01d355ban, "mediumorchid1",0x01ff66e0n,
    "mediumorchid2",0x01ee5fd1n, "mediumorchid3",0x01cd52b4n,
    "mediumorchid4",0x018b377an, "mediumpurple",0x01db7093n,
    "mediumpurple1",0x01ff82abn, "mediumpurple2",0x01ee799fn,
    "mediumpurple3",0x01cd6889n, "mediumpurple4",0x018b475dn,
    "mediumseagreen",0x0171b33cn, "mediumslateblue",0x01ee687bn,
    "mediumspringgreen",0x019afa00n, "mediumturquoise",0x01ccd148n,
    "mediumvioletred",0x018515c7n, "midnightblue",0x01701919n,
    "mintcream",0x01fafff5n, "mistyrose",0x01e1e4ffn, "mistyrose1",0x01e1e4ffn,
    "mistyrose2",0x01d2d5een, "mistyrose3",0x01b5b7cdn,
    "mistyrose4",0x017b7d8bn, "moccasin",0x01b5e4ffn,
    "navajowhite",0x01addeffn, "navajowhite1",0x01addeffn,
    "navajowhite2",0x01a1cfeen, "navajowhite3",0x018bb3cdn,
    "navajowhite4",0x015e798bn, "navy",0x01800000n, "navyblue",0x01800000n,
    "oldlace",0x01e6f5fdn, "olivedrab",0x01238e6bn, "olivedrab1",0x013effc0n,
    "olivedrab2",0x013aeeb3n, "olivedrab3",0x0132cd9an,
    "olivedrab4",0x01228b69n, "orange",0x0100a5ffn, "orange1",0x0100a5ffn,
    "orange2",0x01009aeen, "orange3",0x010085cdn, "orange4",0x01005a8bn,
    "orangered",0x010045ffn, "orangered1",0x010045ffn,
    "orangered2",0x010040een, "orangered3",0x010037cdn,
    "orangered4",0x0100258bn, "orchid",0x01d670dan, "orchid1",0x01fa83ffn,
    "orchid2",0x01e97aeen, "orchid3",0x01c969cdn, "orchid4",0x0189478bn,
    "palegoldenrod",0x01aae8een, "palegreen",0x0198fb98n,
    "palegreen1",0x019aff9an, "palegreen2",0x0190ee90n,
    "palegreen3",0x017ccd7cn, "palegreen4",0x01548b54n,
    "paleturquoise",0x01eeeeafn, "paleturquoise1",0x01ffffbbn,
    "paleturquoise2",0x01eeeeaen, "paleturquoise3",0x01cdcd96n,
    "paleturquoise4",0x018b8b66n, "palevioletred",0x019370dbn,
    "palevioletred1",0x01ab82ffn, "palevioletred2",0x019f79een,
    "palevioletred3",0x018968cdn, "palevioletred4",0x015d478bn,
    "papayawhip",0x01d5efffn, "peachpuff",0x01b9daffn,
    "peachpuff1",0x01b9daffn, "peachpuff2",0x01adcbeen,
    "peachpuff3",0x0195afcdn, "peachpuff4",0x0165778bn, "peru",0x013f85cdn,
    "pink",0x01cbc0ffn, "pink1",0x01c5b5ffn, "pink2",0x01b8a9een,
    "pink3",0x019e91cdn, "pink4",0x016c638bn, "plum",0x01dda0ddn,
    "plum1",0x01ffbbffn, "plum2",0x01eeaeeen, "plum3",0x01cd96cdn,
    "plum4",0x018b668bn, "powderblue",0x01e6e0b0n, "purple",0x01f020a0n,
    "purple1",0x01ff309bn, "purple2",0x01ee2c91n, "purple3",0x01cd267dn,
    "purple4",0x018b1a55n, "red1",0x010000ffn, "red2",0x010000een,
    "red3",0x010000cdn, "red4",0x0100008bn, "rosybrown",0x018f8fbcn,
    "rosybrown1",0x01c1c1ffn, "rosybrown2",0x01b4b4een,
    "rosybrown3",0x019b9bcdn, "rosybrown4",0x0169698bn,
    "royalblue",0x01e16941n, "royalblue1",0x01ff7648n,
    "royalblue2",0x01ee6e43n, "royalblue3",0x01cd5f3an,
    "royalblue4",0x018b4027n, "saddlebrown",0x0113458bn, "salmon",0x017280fan,
    "salmon1",0x01698cffn, "salmon2",0x016282een, "salmon3",0x015470cdn,
    "salmon4",0x01394c8bn, "sandybrown",0x0160a4f4n, "seagreen",0x01578b2en,
    "seagreen1",0x019fff54n, "seagreen2",0x0194ee4en, "seagreen3",0x0180cd43n,
    "seagreen4",0x01578b2en, "seashell",0x01eef5ffn, "seashell1",0x01eef5ffn,
    "seashell2",0x01dee5een, "seashell3",0x01bfc5cdn, "seashell4",0x0182868bn,
    "sienna",0x012d52a0n, "sienna1",0x014782ffn, "sienna2",0x014279een,
    "sienna3",0x013968cdn, "sienna4",0x0126478bn, "skyblue",0x01ebce87n,
    "skyblue1",0x01ffce87n, "skyblue2",0x01eec07en, "skyblue3",0x01cda66cn,
    "skyblue4",0x018b704an, "slateblue",0x01cd5a6an, "slateblue1",0x01ff6f83n,
    "slateblue2",0x01ee677an, "slateblue3",0x01cd5969n,
    "slateblue4",0x018b3c47n, "slategray",0x01908070n,
    "slategray1",0x01ffe2c6n, "slategray2",0x01eed3b9n,
    "slategray3",0x01cdb69fn, "slategray4",0x018b7b6cn,
    "slategrey",0x01908070n, "snow",0x01fafaffn, "snow1",0x01fafaffn,
    "snow2",0x01e9e9een, "snow3",0x01c9c9cdn, "snow4",0x0189898bn,
    "springgreen",0x017fff00n, "springgreen1",0x017fff00n,
    "springgreen2",0x0176ee00n, "springgreen3",0x0166cd00n,
    "springgreen4",0x01458b00n, "steelblue",0x01b48246n,
    "steelblue1",0x01ffb863n, "steelblue2",0x01eeac5cn,
    "steelblue3",0x01cd944fn, "steelblue4",0x018b6436n, "tan",0x018cb4d2n,
    "tan1",0x014fa5ffn, "tan2",0x01499aeen, "tan3",0x013f85cdn,
    "tan4",0x012b5a8bn, "thistle",0x01d8bfd8n, "thistle1",0x01ffe1ffn,
    "thistle2",0x01eed2een, "thistle3",0x01cdb5cdn, "thistle4",0x018b7b8bn,
    "tomato",0x014763ffn, "tomato1",0x014763ffn, "tomato2",0x01425ceen,
    "tomato3",0x01394fcdn, "tomato4",0x0126368bn, "turquoise",0x01d0e040n,
    "turquoise1",0x01fff500n, "turquoise2",0x01eee500n,
    "turquoise3",0x01cdc500n, "turquoise4",0x018b8600n, "violet",0x01ee82een,
    "violetred",0x019020d0n, "violetred1",0x01963effn,
    "violetred2",0x018c3aeen, "violetred3",0x017832cdn,
    "violetred4",0x0152228bn, "wheat",0x01b3def5n, "wheat1",0x01bae7ffn,
    "wheat2",0x01aed8een, "wheat3",0x0196bacdn, "wheat4",0x01667e8bn,
    "whitesmoke",0x01f5f5f5n, "yellow1",0x0100ffffn, "yellow2",0x0100eeeen,
    "yellow3",0x0100cdcdn, "yellow4",0x01008b8bn, "yellowgreen",0x0132cd9an;

  /* Add the 100 grey shades. */
  levels = int(lround(2.55*indgen(0:100)));
  n = numberof(levels);
  for (i = 1; i <= n; ++i) {
    s = swrite(format="%d", i - 1);
    g = levels(i);
    rgb = p_rgb(g,g,g);
    p_add_named_color, "grey"+s, rgb;
    p_add_named_color, "gray"+s, rgb;
  }
}


/*---------------------------------------------------------------------------*/
/* SVG COLORS */

func p_add_svg_colors
/* DOCUMENT p_add_svg_colors;

     Add the named colors recognized by SVG in the global database.

   SEE ALSO: p_add_named_colors, p_add_gist_colors, p_add_x11_colors;
*/
{
  p_add_named_colors, "aliceblue",0x01fff8f0n, "antiquewhite",0x01d7ebfan,
    "aqua",0x01ffff00n, "aquamarine",0x01d4ff7fn, "azure",0x01fffff0n,
    "beige",0x01dcf5f5n, "bisque",0x01c4e4ffn, "black",0x01000000n,
    "blanchedalmond",0x01cdebffn, "blue",0x01ff0000n, "blueviolet",0x01e22b8an,
    "brown",0x012a2aa5n, "burlywood",0x0187b8den, "cadetblue",0x01a09e5fn,
    "chartreuse",0x0100ff7fn, "chocolate",0x011e69d2n, "coral",0x01507fffn,
    "cornflowerblue",0x01ed9564n, "cornsilk",0x01dcf8ffn,
    "crimson",0x013c14dcn, "cyan",0x01ffff00n, "darkblue",0x018b0000n,
    "darkcyan",0x018b8b00n, "darkgoldenrod",0x010b86b8n,
    "darkgray",0x01a9a9a9n, "darkgreen",0x01006400n, "darkgrey",0x01a9a9a9n,
    "darkkhaki",0x016bb7bdn, "darkmagenta",0x018b008bn,
    "darkolivegreen",0x012f6b55n, "darkorange",0x01008cffn,
    "darkorchid",0x01cc3299n, "darkred",0x0100008bn, "darksalmon",0x017a96e9n,
    "darkseagreen",0x018fbc8fn, "darkslateblue",0x018b3d48n,
    "darkslategray",0x014f4f2fn, "darkslategrey",0x014f4f2fn,
    "darkturquoise",0x01d1ce00n, "darkviolet",0x01d30094n,
    "deeppink",0x019314ffn, "deepskyblue",0x01ffbf00n, "dimgray",0x01696969n,
    "dimgrey",0x01696969n, "dodgerblue",0x01ff901en, "firebrick",0x012222b2n,
    "floralwhite",0x01f0faffn, "forestgreen",0x01228b22n,
    "fuchsia",0x01ff00ffn, "gainsboro",0x01dcdcdcn, "ghostwhite",0x01fff8f8n,
    "gold",0x0100d7ffn, "goldenrod",0x0120a5dan, "gray",0x01808080n,
    "green",0x01008000n, "greenyellow",0x012fffadn, "grey",0x01808080n,
    "honeydew",0x01f0fff0n, "hotpink",0x01b469ffn, "indianred",0x015c5ccdn,
    "indigo",0x0182004bn, "ivory",0x01f0ffffn, "khaki",0x018ce6f0n,
    "lavender",0x01fae6e6n, "lavenderblush",0x01f5f0ffn,
    "lawngreen",0x0100fc7cn, "lemonchiffon",0x01cdfaffn,
    "lightblue",0x01e6d8adn, "lightcoral",0x018080f0n, "lightcyan",0x01ffffe0n,
    "lightgoldenrodyellow",0x01d2fafan, "lightgray",0x01d3d3d3n,
    "lightgreen",0x0190ee90n, "lightgrey",0x01d3d3d3n, "lightpink",0x01c1b6ffn,
    "lightsalmon",0x017aa0ffn, "lightseagreen",0x01aab220n,
    "lightskyblue",0x01face87n, "lightslategray",0x01998877n,
    "lightslategrey",0x01998877n, "lightsteelblue",0x01dec4b0n,
    "lightyellow",0x01e0ffffn, "lime",0x0100ff00n, "limegreen",0x0132cd32n,
    "linen",0x01e6f0fan, "magenta",0x01ff00ffn, "maroon",0x01000080n,
    "mediumaquamarine",0x01aacd66n, "mediumblue",0x01cd0000n,
    "mediumorchid",0x01d355ban, "mediumpurple",0x01db7093n,
    "mediumseagreen",0x0171b33cn, "mediumslateblue",0x01ee687bn,
    "mediumspringgreen",0x019afa00n, "mediumturquoise",0x01ccd148n,
    "mediumvioletred",0x018515c7n, "midnightblue",0x01701919n,
    "mintcream",0x01fafff5n, "mistyrose",0x01e1e4ffn, "moccasin",0x01b5e4ffn,
    "navajowhite",0x01addeffn, "navy",0x01800000n, "oldlace",0x01e6f5fdn,
    "olive",0x01008080n, "olivedrab",0x01238e6bn, "orange",0x0100a5ffn,
    "orangered",0x010045ffn, "orchid",0x01d670dan, "palegoldenrod",0x01aae8een,
    "palegreen",0x0198fb98n, "paleturquoise",0x01eeeeafn,
    "palevioletred",0x019370dbn, "papayawhip",0x01d5efffn,
    "peachpuff",0x01b9daffn, "peru",0x013f85cdn, "pink",0x01cbc0ffn,
    "plum",0x01dda0ddn, "powderblue",0x01e6e0b0n, "purple",0x01800080n,
    "red",0x010000ffn, "rosybrown",0x018f8fbcn, "royalblue",0x01e16941n,
    "saddlebrown",0x0113458bn, "salmon",0x017280fan, "sandybrown",0x0160a4f4n,
    "seagreen",0x01578b2en, "seashell",0x01eef5ffn, "sienna",0x012d52a0n,
    "silver",0x01c0c0c0n, "skyblue",0x01ebce87n, "slateblue",0x01cd5a6an,
    "slategray",0x01908070n, "slategrey",0x01908070n, "snow",0x01fafaffn,
    "springgreen",0x017fff00n, "steelblue",0x01b48246n, "tan",0x018cb4d2n,
    "teal",0x01808000n, "thistle",0x01d8bfd8n, "tomato",0x014763ffn,
    "turquoise",0x01d0e040n, "violet",0x01ee82een, "wheat",0x01b3def5n,
    "white",0x01ffffffn, "whitesmoke",0x01f5f5f5n, "yellow",0x0100ffffn,
    "yellowgreen",0x0132cd9an;
}

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
func _p_hack_limits(xmin, xmax, ymin, ymax, square=, nice=, restrict=)
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
        _p_orig_limits;
      } else {
      _p_orig_limits, square=square, nice=nice, restrict=restrict;
      }
    } else {
      _p_orig_limits, xmin, xmax, ymin, ymax,
        square=square, nice=nice, restrict=restrict;
    }
  } else {
    old_limits = _p_orig_limits(xmin, xmax, ymin, ymax, square=square,
                                   nice=nice, restrict=restrict);
  }
  if (numberof(xmin) == 5) {
    new_flags = long(xmin(5));
    old_flags = long(old_limits(5));
    xflag = (new_flags & P_LIM_LOGX);
    yflag = (new_flags & P_LIM_LOGY);
    if ((old_flags & P_LIM_LOGX) != xflag ||
        (old_flags & P_LIM_LOGY) != yflag) {
      _p_hack_logxy, xflag, yflag;
    }
  }
  return old_limits;
}
errs2caller, _p_hack_limits;

_P_LIN_TICKS_RELATIVE_LENGTH = [1.0, 0.64, 0.36, 0.18, 0.09];
_P_LOG_TICKS_RELATIVE_LENGTH = [1.0, 0.0, 0.0, 0.0, 0.0];
func _p_hack_logxy(xflag, yflag)
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
  _p_orig_logxy, xflag, yflag;
}
errs2caller, _p_hack_logxy;

/*---------------------------------------------------------------------------*/
/* INITIALIZATION */

if (is_void(P_DEBUG)) P_DEBUG = P_FALSE;
func _p_init
{
  /* Add X11, SVG and then Gist colors. */
  p_add_x11_colors;
  p_add_svg_colors;
  p_add_gist_colors;

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
