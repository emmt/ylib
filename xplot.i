/*
 * xplot.i --
 *
 * Extends Yorick plotting system.
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

/* Load prerequisites. */
require, "xplot0.i";

/* When substituting original plotting commands, the trick is to take into
   account interleaved strings for each significant arguments (see YpQuine).

   Command                   Number of named arguments
   ---------------------------------------------------
   plc, z, y, x, ireg;       4
   pldj, x0, y0, x1, y1;     4
   plf, z, y, x, ireg;       4
   plg, x, y;                2
   pli, z;                   1 (not x0, y0, x1, y1)
   plm, y, x, ireg;          3
   plv, vy, vx, y, x, ireg;  4 (not ireg?)
   ---------------------------------------------------
*/

/* Save builtin version of the plotting commands. */
if (is_func(plc)  == 2) _pl_orig_plc  = plc;
if (is_func(pldj) == 2) _pl_orig_pldj = pldj;
if (is_func(plf)  == 2) _pl_orig_plf  = plf;
if (is_func(plg)  == 2) _pl_orig_plg  = plg;
if (is_func(pli)  == 2) _pl_orig_pli  = pli;
if (is_func(plm)  == 2) _pl_orig_plm  = plm;
if (is_func(plt)  == 2) _pl_orig_plt  = plt;
if (is_func(plv)  == 2) _pl_orig_plv  = plv;

func pl_hack(hack)
/* DOCUMENT pl_hack, 0/1;

     If argument is true, install improved substitutes for some standard
     graphic commands; otherwise, restore original commands.

   SEE ALSO: pli, plg, limits, logxy, xpli, xplg.
 */
{
  extern limits, logxy, pli, plg;
  if (is_func(_p_orig_limits) != 2) error, "corrupted limits wrapper";
  if (is_func(_p_orig_logxy) != 2) error, "corrupted logxy wrapper";
  if (is_func(_pl_orig_pli) != 2) error, "corrupted pli wrapper";
  if (is_func(_pl_orig_plg) != 2) error, "corrupted plg wrapper";

  if (hack) {
    limits = _p_hack_limits;
    logxy = _p_hack_logxy;
    pli = _pl_hack_pli;
    plg = _pl_hack_plg;
  } else {
    limits = _p_orig_limits;
    logxy = _p_orig_logxy;
    pli = _pl_orig_pli;
    plg = _pl_orig_plg;
  }
}

local _pl_hack_pli, _xpli;
func xpli(z, x0, y0, x1, y1, legend=, hide=, top=, cmin=, cmax=, cmap=)
/* DOCUMENT xpli, z;
          or xpli, z, x1, y1;
          or xpli, z, x0, y0, x1, y1;

      Plots the image Z as a cell array -- an array of equal rectangular cells
      colored according to the 2-D array Z.  The first dimension of Z is
      plotted along x, the second dimension is along y.  If Z is of type char,
      it is used "as is", otherwise it is linearly scaled to fill the current
      palette, as with the bytscl function.  (See the bytscl function for
      explanation of top, cmin, cmax.)

      As for plf and plfp, Z may also be a 3D array with 1st dimension 3 of
      char giving the [r,g,b] components of each color.  See the color keyword
      for cautions about using this if you do not have a true color display.

      If X1 and Y1 are given, they represent the coordinates of the upper right
      corner of the image.  If X0, and Y0 are given, they represent the
      coordinates of the lower left corner, which is at (0.5,0.5) by default.
      If only the Z array is given, each cell will be a 1x1 unit square, with
      the lower left corner of the image at (0.5,0.5).

      Compared to the original Yorick PLI command, this version correctly set
      the default coordinates (X0,Y0) and adds the CMAP keyword.

      Keyword CMAP can be used to specify a colormap (see cmap, or p_colormap).

      The following keywords are legal (each has a separate help entry):
    KEYWORDS: legend, hide, top, cmin, cmax
    SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, cmap, p_colormap,
              limits, logxy, range, fma, hcp, palette, bytscl, histeq_scale
*/
{
  _xpli, "z";
}

/* This function is a possible replacement for the builtin pli. */
func _pl_hack_pli(z, _str1, x0, y0, x1, y1, legend=, hide=, top=, cmin=, cmax=,
                  cmap=)
{
  _xpli, _str1;
}

/* This function is the real worker, all another parameters than the string(s)
   inserted by the parser are passed as external for maximum efficiency. */
func _xpli(_str1)
{
  extern z, x0, y0, x1, y1, legend, hide, top, cmin, cmax, cmap;
  mode = (is_void(x0) | (is_void(y0) << 1) | (is_void(x1) << 2) |
          (is_void(y1) << 3));
  if (mode == 3) {
    x1 = x0;
    y1 = y0;
    x0 = 0.5;
    y0 = 0.5;
  } else if (mode == 15) {
    dims = dimsof(z);
    rank = numberof(dims) - 1;
    if (rank == 2) {
      nx = dims(2);
      ny = dims(3);
    } else if (rank == 3 && dims(2) == 3 && structof(z) == char) {
      nx = dims(3);
      ny = dims(4);
    } else {
      error, "expecting 2D array (or 3-by-N1-by-N2 array of chars)";
    }
    x0 = 0.5;
    y0 = 0.5;
    x1 = nx + 0.5;
    y1 = ny + 0.5;
  } else if (mode != 0) {
    error, "pli needs either 0, 1, or 2 corner (x,y) points";
  }
  if (! is_void(cmap)) p_colormap, cmap;
  _pl_orig_pli, z, _str1, x0, y0, x1, y1, hide=hide, top=top,
    legend=legend, cmin=cmin, cmax=cmax;
}

errs2caller, xpli, _xpli, _pl_hack_pli;

local _xplg, _pl_hack_plg;
func xplg(y, x, legend=, hide=, color=, type=, width=, marks=, mcolor=,
          marker=, msize=, mspace=, mphase=, rays=, arrowl=, arroww=, rspace=,
          rphase=, closed=, smooth=, n=)
/* DOCUMENT xplg, y, x;
         or xplg, y;

      Plots a graph of Y versus X.  Y and X must be 1-D arrays of equal length;
      if X is omitted, it defaults to [1, 2, ..., numberof(Y)].  A keyword
      n=[n1,n2,n3,...nN] can be used to add N curves.  In this case, sum(n)
      must be numberof(y).

      Compared to the original Yorick PLG command, this version automatically
      converts the values of keywords COLOR, MCOLOR, MARKER and TYPE with
      p_color, p_color, p_marker and p_type respectively.

      The following keywords are legal (each has a separate help entry):

    KEYWORDS: legend, hide
              type, width, color, closed, smooth
              marks, marker, mspace, mphase
              rays, arrowl, arroww, rspace, rphase
    SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmk
              limits, logxy, range, fma, hcp
  */
{
  _xplg, "y", (is_void(x) ? [] : "x");
}

/* This function is a possible replacement for the builtin plg. */
func _pl_hack_plg(y, _str1, x, _str2, legend=, hide=, color=, type=, width=,
                  marks=, mcolor=, marker=, msize=, mspace=, mphase=, rays=,
                  arrowl=, arroww=, rspace=, rphase=, closed=, smooth=, n=)
{
  _xplg, _str1, _str2;
}

/* This function is the real worker, all another parameters than the string(s)
   inserted by the parser are passed as external for maximum efficiency. */
func _xplg(_str1, _str2)
{
  extern y, x, legend, hide, color, type, width, marks, mcolor, marker, msize,
    mspace, mphase, rays, arrowl, arroww, rspace, rphase, closed, smooth, n;
  if (! is_void(color)) p_color, color;
  if (! is_void(mcolor)) p_color, mcolor;
  if (! is_void(marker) && structof(marker) != char) p_marker, marker;
  if (! is_void(type) && structof(type) != string) p_type, type;
  if (is_void(_str2)) {
    _pl_orig_plg, y, _str1,
      legend=legend, hide=hide, color=color, type=type, width=width,
      marks=marks, mcolor=mcolor, marker=marker, msize=msize, mspace=mspace,
      mphase=mphase, rays=rays, arrowl=arrowl, arroww=arroww, rspace=rspace,
      rphase=rphase, closed=closed, smooth=smooth, n=n;
  } else {
    _pl_orig_plg, y, _str1, x, _str2,
      legend=legend, hide=hide, color=color, type=type, width=width,
      marks=marks, mcolor=mcolor, marker=marker, msize=msize, mspace=mspace,
      mphase=mphase, rays=rays, arrowl=arrowl, arroww=arroww, rspace=rspace,
      rphase=rphase, closed=closed, smooth=smooth, n=n;
  }
}

errs2caller, xplg, _xplg, _pl_hack_plg;

func pl_graph(y, x, legend=, hide=, color=, type=, width=,
              marks=, mcolor=, marker=, msize=, mspace=, mphase=,
              rays=, arrowl=, arroww=, rspace=, rphase=,
              closed=, smooth=, n=)
{
  if (! is_void(color)) p_color, color;
  if (! is_void(marker) && structof(marker) != char) p_marker, marker;
  if (! is_void(type) && structof(type) != string) p_type, type;
  plg, y, x, legend=legend, hide=hide, color=color,
    type=type, width=width, marks=marks, mcolor=mcolor, marker=marker,
    msize=msize, mspace=mspace, mphase=mphase, rays=rays, arrowl=arrowl,
    arroww=arroww, rspace=rspace, rphase=rphase, closed=closed,
    smooth=smooth, n=n;
}

func pl_bars(y, x, color=, fill=, edges=, ecolor=, ewidth=,
             thickness=, base=, rotate=)
/* DOCUMENT pl_bars, y;
         or pl_bars, y, x;
         or pl_bars, x, rotate=1;
         or pl_bars, x, y, rotate=1;

     This subroutine plots a bar chart of Y versus X (X default to
     indgen(numberof(Y)) if not specified).  The output looks like:

     :    +-+
     :    | |           +-+
     :    | | +-+       | |
     :    | | | |       | |
     :    +-+-+ +- ... -+-+

     Keyword COLOR can be used to specify the color(s) of the bars.  By default
     all bars are colored in gray.  There may be different colors for the
     different bars.

     Keyword THICKNESS can be used to specify the width of the bars relatively
     to the spacing of their abscissae.  By default THICKNESS is 0.8.

     Keyword BASE can be used to specify the ordinate(s) of the base of the
     bars.  By default BASE is 0.

     If keyword ROTATE is true the bars are drawn horizontally and Y gives the
     abscissae of the bars while X gives the ordinates.

     Keywords EDGES, ECOLOR and EWIDTH can be used to specify whether to draw
     the edges of the bars, their color and the width of the line to use.


   SEE ALSO: plf, p_color, pl_fbox, pl_hist.
 */
{
  /* Get coordinates and baseline position. */
  n =  p_real_vector(y, 0, (rotate ? "X" : "Y"));
  if (is_void(base)) {
    base = array(double, n);
  } else {
    p_real_vector, base, n, "BASE";
  }
  if (is_void(x)) {
    x = double(indgen(n));
  } else {
    p_real_vector, x, n, (rotate ? "Y" : "X");
  }

  /* Compute the thickness of the bars. */
  if (is_void(thickness)) {
    thickness = 0.8;
  } else if (! is_scalar(thickness) || ! is_real(thickness) ||
             thickness <= 0.0 || thickness >= 1) {
    error, "THICKNESS must be a scalar strictly in the range (0,1)";
  }
  if (n <= 1) {
    s = 1.0;
  } else {
    s = x(dif);
    if (min(s)*max(s) <= 0.0) {
      error, ((rotate ? "Y" : "X") + "must be strictly monotonic");
    }
  }
  s *= (thickness/2.0);

  /* Each bar is separated by an undrawn cell, so there are 2*N - 1 cells
     delimited by coordinates BX and BY of dimensions 2*N-by-2.  The colors of
     the bars are stored into an (2*N - 1)-by-1 array BZ. The IREG array has
     same size as BX and BY.  Regions are drawn in same order as their number
     given by IREG.  IREG = 0 for undrawn cells, IREG > 0 otherwise.  First row
     and column of IREG must be zero. */
  local x0, x1, y0, y1;
  eq_nocopy, y0, base;
  eq_nocopy, y1, y;
  x0 = x - grow(s(1), s);
  x1 = x + grow(s, s(0));
  if (rotate) {
    swap, x0, y0;
    swap, x1, y1;
  }
  /* Draw the plot. */
  _pl_fbox;
}

local plh;
func pl_steps(y, x, just=, justify=, legend=, hide=, type=, width=, color=,
              marks=, marker=, mspace=, mphase=)
/* DOCUMENT pl_steps, y, x;
         or pl_steps, y;

     Plots a graph of Y versus X in an "histogram" style (i.e., with steps).  Y
     and X must be 1-D arrays of equal length; if X is omitted, it defaults to
     [1, 2, ..., numberof(Y)].

     The optional keyword JUSTIFY set the justification of the histogram along
     the X axis.  See p_justify for valid values, only the horizontal
     justification is taken into account.  Default is centered.

     Other plotting keywords (legend, hide, type, width, color, marks, marker,
     mspace, and mphase) are passed to the plg routine.

   SEE ALSO: pl_bars, p_color, p_type, p_justify, plg.
*/
{
  /* Parse/check arguments. */
  if (! is_vector(y) || (n = numberof(y)) < 2) {
    error, "Y must be a vector of at least 2 elements";
  }
  if (is_void(x)) {
    x = double(indgen(numberof(y)));
  } else if (! is_vector(x) || numberof(x) != n) {
    error, "X must be a vector of same length as Y";
  }
  if (! is_void(just)) {
    if (is_void(justify)) {
      eq_nocopy, justify, just;
    } else {
      error, "keyword JUST is provided for backward compatibility and cannot be specified with keyword JUSTIFY";
    }
  }
  justify = p_justify(justify, P_CENTER)&3;
  if (! is_void(color)) p_color, color;
  if (! is_void(type)) p_type, type;
  if (! is_void(marker) && structof(marker) != char) p_marker, marker;

  /* build new X vector */
  n2 = 2*n;
  x2 = array(double, n2);
  if (justify == P_LEFT) {
    /* Left justified. */
    x2(1::2) = x;
    x2(2:-1:2) = x(2:);
    x2(0) = 2*x(0) - x(-1);
  } else if (justify == P_NORMAL || justify == P_CENTER) {
    /* Centered. */
    d = 0.5*x(dif);
    dx = d(1);
    grow, dx, d, d(0);
    d = [];
    x2(1::2) = x - dx(:-1);
    x2(2::2) = x + dx(2:);
    dx = [];
  } else if (justify == P_RIGHT) {
    /* Right justified. */
    x2(1) = 2*x(1) - x(2);
    x2(2::2) = x;
    x2(3::2) = x(:-1);
  } else {
    error, "bad value for keyword JUSTIFY";
  }

  /* Build new Y vector. */
  y2 = array(double, n2);
  y2(1::2) = y2(2::2) = y;

  /* Plot the graph. */
  nil = string();
  _pl_orig_plg, y2, nil, x2, nil,
    legend=legend, hide=hide, type=type, width=width, color=color,
    marks=marks, marker=marker, mspace=mspace, mphase=mphase;
}

plh = pl_steps;

func pl_img(img, clear=, cmin=, cmax=, cmap=,
            pixelbias=, pixelsize=, pixelref=, pixelunits=,
            cbar=, vert=, viewport=, adjust=,
            nlabs=, labels=, levels=,
            color=, font=, height=, opaque=, orient=,
            width=, ticklen=, laboff=, thickness=, format=)
/* DOCUMENT pl_img, img;

     Plot image IMG in current graphics window.

     Keywords CMIN and CMAX can be used to specify cut levels for the
     display (see pli).

     Keyword CLEAR can be used to call fma command prior to drawing the image
     (CLEAR > 0) or after drawing the image (CLEAR < 0, useful in animate
     mode).

     Keywords PIXELSIZE, PIXELBIAS and PIXELREF can be set to specify the pixel
     coordinates.  The coordinate of the center of j-th pixel is given by:

         PIXELBIAS + PIXELSIZE*(j - PIXELREF)

     The default settings are PIXELBIAS=1.0, PIXELSCALE=1.0 and PIXELREF=1.0
     (i.e. to yield the same coordinates as Yorick's indexing rules).  To have
     the same coordinates as in C, use PIXLEBIAS=0.

     Keyword PIXELUNITS can be used to specify the label(s) of the axis.

     PIXELSIZE, PIXELBIAS, PIXELREF and PIXELUNITS can have 1 or 2 elements
     to specify same or different settings for the X and Y axis.

     Keyword CBAR can be set to a non-zero value to draw a color bar (see
     pl_cbar) whose position is given by the value of CBAR.  Keywords VIEWPORT,
     ADJUST, NLABS, LABELS, LEVELS, COLOR, FONT, HEIGHT, OPAQUE, ORIENT, WIDTH,
     TICKLEN, LABOFF, THICKNESS, and FORMAT can be used to customize the color
     bar.


   SEE ALSO: pli, fma, animate, pl_cbar, pl_title.
*/
{
  dims = dimsof(img);
  ndims = numberof(dims) - 1;
  if (ndims == 2) {
    xdim = dims(2);
    ydim = dims(3);
  } else if (ndims == 3 && dims(2) == 3 && structof(img) == char) {
    if (cbar) error, "drawing color bar is not supported for RGB images";
    xdim = dims(3);
    ydim = dims(4);
  } else {
    error, "expecting an image";
  }

  if (is_void(pixelref)) pixelref = 1.0;
  if (is_void(pixelbias)) pixelbias = 1.0;
  if (is_void(pixelsize)) pixelsize = 1.0;
  if (is_void(cmin)) cmin = min(img);
  if (is_void(cmax)) cmax = max(img);

  xsize = pixelsize(1);
  ysize = pixelsize(0);
  xbias = pixelbias(1);
  ybias = pixelbias(0);
  xref = pixelref(1);
  yref = pixelref(0);

  x0 = xbias + xsize*(0.5 - xref);
  x1 = xbias + xsize*(xdim + 0.5 - xref);
  y0 = ybias + ysize*(0.5 - yref);
  y1 = ybias + ysize*(ydim + 0.5 - yref);

  if (clear && clear > 0) {
    fma;
  }
  xpli, img, x0, y0, x1, y1, cmin=cmin, cmax=cmax, cmap=cmap;
  if (! is_void(cbar)) {
    pl_cbar, cmin=cmin, cmax=cmax, position=cbar, viewport=viewport,
      adjust=adjust, nlabs=nlabs, labels=labels, levels=levels,
      color=color, font=font, height=height, opaque=opaque, orient=orient,
      width=width, ticklen=ticklen, laboff=laboff, thickness=thickness,
      format=format;
  }
  if (! is_void(pixelunits)) {
    pl_title, pixelunits(1), pixelunits(0),
      color=color, font=font, height=height;
  }
  if (clear && clear < 0) {
    fma;
  }
}

func pl_cbar(z, cmin=, cmax=, position=, viewport=, adjust=,
             nlabs=, labels=, levels=, ncolors=,
             labeloptions=, labeldigits=,
             linetype=, frametype=, ticktype=,
             width=, linewidth=, framewidth=, tickwidth=,
             color=, linecolor=, textcolor=, framecolor=, tickcolor=,
             font=, height=, opaque=, orient=, textstyle=,
             ticklen=, laboff=, thickness=, format=)
/* DOCUMENT pl_cbar, z;
         or pl_cbar, cmin=CMIN, cmax=CMAX;

     Draw a color bar next to the current coordinate system.  The colors and
     the default associated label values are from min(Z) to max(Z);
     alternatively, keywords CMIN and CMAX can be specified.

     Keyword POSITION can be set with the position of the color bar relative to
     the current coordinate system.  For now only "bottom" (P_BOTTOM) and
     "right" (P_RIGHT) are possible.  By default, the color bar appears to the
     right of the current coordinate system.

     By default, the colorbar is drawn next to the current viewport; other
     viewport coordinates can be given by VIEWPORT=[xmin,xmax,ymin,ymax].
     Keyword ADJUST can be used to move the bar closer to (adjust<0) or further
     from (adjust>0) the viewport.

     Keyword LEVELS can be used to specify the positions of the labels and
     ticks to draw in units of Z.

     Keyword LABELS can be used to specify the labels to print along the color
     bar.  If keyword LABELS is not specified, the labels are the textual
     values of the levels and the format of the labels can be specified with
     keyword FORMAT.  FORMAT can be a format string to use with swrite or an
     integer, say N, to use a scientific notation with N significant digits.
     By default FORMAT="%.3g" or FORMAT=3 dependending on the magnitude of the
     label values.  The font type, font height and text orientation for the
     labels can be set with keywords FONT (default FONT="helvetica"), HEIGHT
     (default HEIGHT=14 points) and ORIENT respectively.  By default, the same
     parameters as the axis labels are used for the labels.  The TEXTSTYLE
     keyword can be used to specify a GpTextAttribs structure with text
     attributes.

     If neither LEVELS nor LABELS are specified, keyword NLABS can be used to
     choose the approximate number of displayed labels; by default, NLABS=5;
     use NLABS=0 to suppress all labels.

     Keyword NCOLORS can be used to specify the number of color cells in the
     color bar.  By default, the number of colors of the current palette is
     used.

     Keywords COLOR, LINECOLOR, TEXTCOLOR, FRAMECOLOR and TICKCOLOR can be used
     to specify the colors of different elements in a hierarchical way (see
     figure below).  TEXTCOLOR is used for the labels, FRAMECOLOR is used for
     the frame and TICKCOLOR is used for the ticks.

     |    TEXTCOLOR <------ TEXTSTYLE.color <--+
     |    FRAMECOLOR <--+                      +-- COLOR <--- foreground color
     |                  +-- LINECOLOR <--------+
     |    TICKCOLOR <---+


     Keyword WIDTH can be used to set the width of the lines used to draw the
     frame and the ticks of the colorbar.  If WIDTH is negative, no frame and
     no ticks are drawn.

     Keyword TICKLEN can be used to set the length (in NDC units) of the ticks.
     Default is 0.40*HEIGHT.  If TICKLEN is zero, no ticks get drawn, if
     TICKLEN is negative, the ticks get drawn inward.

     Keyword LABOFF can be used to set the offset of the label with respect to
     the tip of the ticks.

     Keyword THICKNESS can be used to set the thickness of the colorbar (in NDC
     units).  Default is 0.020 NDC.


    SEE ALSO: pl_img, p_span, pli, plt, pldj, plg, viewport.
 */
{
  nil = string();
  if (is_void(cmin)) {
    if (is_void(z)) error, "keyword CMIN must be given";
    cmin = min(z);
  }
  if (is_void(cmax)) {
    if (is_void(z)) error, "keyword CMAX must be given";
    cmax = max(z);
  }
  cmin = double(cmin); /* make sure CMIN is double */
  cmax = double(cmax); /* make sure CMAX is double */

  p_position, position, P_RIGHT;
  if (position != P_RIGHT && position != P_BOTTOM) {
    error, "only \"right\" and \"bottom\" positions are implemented";
  }
  vert = (position == P_RIGHT || position == P_LEFT);
  if (is_void(viewport)) viewport = _p_orig_viewport();
  if (is_void(textstyle) && (is_void(font) || is_void(color) ||
                             is_void(height) || is_void(orient))) {
    textstyle = p_query_text_style((vert ? 2 : 1));
  }
  p_real, height, (is_void(textstyle) ? 12.0/P_NDC_POINT : textstyle.height);
  p_real, adjust, 0.0;
  p_real, ticklen, 0.40*height;
  p_real, laboff, max(0, ticklen) + 0.27*height;
  p_real, thickness, 0.020;
  p_font, font, (is_void(textstyle) ? P_HELVETICA : textstyle.font);
  p_integer, orient, (is_void(textstyle) ? 0 : textstyle.orient);

  /* Parse colors. */
  p_color, color, P_FG;
  p_color, textcolor, (is_void(textstyle) ? color : textstyle.color);
  p_color, linecolor, color;
  p_color, framecolor, linecolor;
  p_color, tickcolor, linecolor;

  /* Parse line types. */
  p_type, linetype, P_SOLID;
  p_type, frametype, linetype;
  p_type, ticktype, linetype;

  /* Parse line widths. */
  p_real, width, 0.0;
  p_real, linewidth, width;
  p_real, framewidth, linewidth;
  p_real, tickwidth, linewidth;

  /* Deal with given labels of levels. */
  if (! is_void(labels)) {
    if (! is_string(labels)) {
      error, "LABELS must be an array of strings";
    }
    n = numberof(labels);
    if (is_void(levels)) {
      error, "LEVELS must be specified with LABELS";
    }
    if (numberof(levels) != n || ident(levels) > Y_DOUBLE) {
      error, "LEVELS must be as many real values as LABELS";
    }
    if (! is_void(nlabs) && ! (is_integer(nlabs) && is_scalar(nlabs) &&
                               nlabs == n)) {
      error, "NLABS must be unspecified or numberof(LABELS) in this case";
    }
    nlabs = n;
  } else if (! is_void(levels)) {
    if (ident(levels) > Y_DOUBLE) {
      error, "bad data type for LEVELS";
    }
    n = numberof(levels);
    if (! is_void(nlabs) && ! (is_integer(nlabs) && is_scalar(nlabs) &&
                               nlabs == n)) {
      error, "NLABS must be unspecified or numberof(LEVELS) in this case";
    }
    nlabs = n;
  } else if (is_void(nlabs)) {
    nlabs = 5;
  } else if (is_integer(nlabs) && is_scalar(nlabs)) {
    nlabs = long(nlabs);
  } else {
    error, "NLABS must be a scalar integer";
  }

  /* Make the color gradient image to display. */
  if (is_void(ncolors)) {
    local red, green, blue;
    palette, red, green, blue, query=1;
    ncolors = numberof(red);
    if (ncolors < 2) {
      ncolors = 240;
    }
    cells = char(indgen(0 : ncolors - 1));
  } else {
    cells = indgen(ncolors);
  }
  if (ncolors > 1 && cmax != cmin) {
    h = (cmax + 0.0 - cmin)/(2*(ncolors - 1));
  }
  c0 = cmin - h;
  c1 = cmax + h;

  linetype = 1; /* "solid" */
  drawline = (is_void(width) || width >= 0);

  if (position == P_RIGHT) {
    x0 = viewport(2) + adjust + 0.022;
    x1 = x0 + thickness;
    y0 = viewport(3);
    y1 = viewport(4);
    cells = cells(-,);
    z0 = y0;
    z1 = y1;
  } else if (position == P_BOTTOM) {
    x0 = viewport(1);
    x1 = viewport(2);
    y0 = viewport(3) - adjust - 0.045;
    y1 = y0 - thickness;
    cells = cells(,-);
    z0 = x0;
    z1 = x1;
  } else {
    error, "invalid position";
  }
  sys = plsys(0);
  pli, cells, x0, y0, x1, y1;
  if (drawline) {
    plg, [y0,y0,y1,y1], [x0,x1,x1,x0], closed=P_TRUE,
      color=framecolor, width=framewidth, type=frametype, marks=0;
  }

  if (nlabs > 0) {
    if (is_void(levels)) {
      ptr = p_labels(c0, c1, nlabs, eps=1e-6,
                     ndig=labeldigits, opt=labeloptions);
      levels = *ptr(1);
      labels = *ptr(2);
      nlabs = numberof(levels);
    } else {
      /* Select levels inside the displayed range. */
      j = where((levels >= min(cmin, cmax))&(levels <= max(cmin, cmax)));
      if ((n = numberof(j)) != nlabs) {
        if (is_array(j)) {
          levels = levels(j);
          if (! is_void(labels)) labels = labels(j);
        }
        nlabs = n; /* may be zero here */
      }
      if (nlabs > 0 && is_void(labels)) {
        // FIXME: use a better method here
        if (is_void(format)) {
          t = max(abs(levels));
          format = (t > 0.1 && t < 1000.0 ? "%.3g" : 3);
        }
        if (is_integer(format)) {
          labels = p_pow10_labels(levels, ndig=format);
        } else {
          labels = swrite(format=format, levels);
        }
      }
    }
  }

  if (nlabs > 0) {
    lz = interp([z0, z1], [c0, c1], levels);
    local lx0, lx1, lx2, ly0, ly1, ly2;
    if (vert) {
      lx0 = array(x1, nlabs);
      lx1 = array(x1 + ticklen, nlabs);
      lx2 = array(x1 + laboff, nlabs);
      eq_nocopy, ly0, lz;
      eq_nocopy, ly1, lz;
      eq_nocopy, ly2, lz;
      justify = "LH";
    } else {
      ly0 = array(y1, nlabs);
      ly1 = array(y1 - ticklen, nlabs);
      ly2 = array(y1 - laboff, nlabs);
      eq_nocopy, lx0, lz;
      eq_nocopy, lx1, lz;
      eq_nocopy, lx2, lz;
      justify = "CT";
    }
    if (ticklen && drawline) {
      pldj, lx0, ly0, lx1, ly1,
        color=tickcolor, width=tickwidth, type=ticktype;
    }
    height_in_points = height/P_NDC_POINT;
    lengths = strlen(labels);
    for (i = 1; i <= nlabs; ++i) {
      if (lengths(i) > 0) {
        _pl_orig_plt, labels(i), lx2(i), ly2(i), tosys=0, color=textcolor,
          font=font, height=height_in_points, opaque=opaque,
          orient=orient, justify=justify;
      }
    }
  }
  plsys, sys;
}

local plp;
func pl_points(y, x, dx=, xlo=, xhi=, dy=, ylo=, yhi=, size=, symbol=, ticks=,
               legend=, type=, width=, fill=,
               color=, fillcolor=, edgecolor=, barcolor=)
/* DOCUMENT pl_points, y, x;
         or pl_points, y, x, dx=sigma_x, dy=sigma_y;

     Plots points (X,Y) with symbols and/or error bars.  X, and Y may have any
     dimensionality, but must have the same number of elements.  If X is nil,
     it defaults to indgen(numberof(Y)).

     Keyword SYMBOL may be used to choose the shape of the symbols (see
     p_symbol), by default an 'x' cross is used.  Keyword SIZE may be used to
     change the size of the symbols and tick marks (SIZE acts as a multiplier,
     default value is 1.0).  If value of keyword FILL is true (non-nil and
     non-zero), symbols are filled.  The default is to draw open symbols.

     Keyword COLOR can be used to specify the symbol color (default is
     foreground color), see `p_color` for allowed color values.  Keywords
     EDGECOLOR and FILLCOLOR can be used to specify different colors for the
     outline and the inside of the symbol.

     Keywords XLO, XHI, YLO, and/or YHI can be used to indicate the bounds of
     the optional error bars (default is to draw no error bars).  Only
     specified bounds get plotted as error bars. If value of keyword TICKS is
     true (non-nil and non-zero), ticks get drawn at the endpoints of the error
     bars.  Alternatively, keywords DX and/or DY can be used to plot error bars
     as segments from XLO=X-DX to XHI=X+DX and/or from YLO=Y-DY to YHI=Y+DY.
     If keyword DX (respectively DY) is used, any value of XLO and XHI
     (respectively YLO and YHI) is ignored.  The error bars and their ticks, if
     any, are drawn with the color specified by keyword BARCOLOR or, if this
     keyword is unspecified, to the same color as the edges of the symbols.

     The other keywords are the same as for pldj: LEGEND, TYPE, WIDTH, COLOR
     (TYPE is only used to draw error bars).


   EXAMPLES:
     pl_points, y, x, symbol='*',  size=3, width=5, edgecolor="orange",
         fillcolor="DarkSlateBlue", fill=1;


   KEYWORDS:
     legend, type, width, color.


   SEE ALSO:
     p_symbol, p_color,
     pldj, plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmk,
     limits, logxy, range, fma, hcp.
 */
{
  /* NDC units for symbols/ticks (one pixel = 0.00125268 NDC at 75 DPI). */
  u0 = 0.0;       // zero
  u1 = 0.005;     // radius in NDC
  if (! is_void(size)) u1 *= size;

  /* Parse color(s). */
  p_color, color, P_FG;
  p_color, edgecolor, color;
  p_color, barcolor, edgecolor;
  if (fill) p_color, fillcolor, color;

  /* Default X values. */
  if (is_void(x)) (x = array(double, dimsof(y)))(*) = indgen(numberof(y));

  /* error bars */
  if (is_void(dx)) {
    err = (! is_void(xlo)) + 2*(! is_void(xhi));
  } else {
    xlo = x - dx;
    xhi = x + dx;
    err = 3;
  }
  if (err) {
    pldj, (is_void(xlo) ? x : xlo), y, (is_void(xhi) ? x : xhi), y,
      type=type, width=width, color=edgecolor;
    if (ticks) {
      xm = [ u0, u0];
      ym = [-u1, u1];
      if      (err == 1) _pl_points,   y,       xlo;
      else if (err == 2) _pl_points,   y,       xhi;
      else               _pl_points, [y, y], [xlo, xhi];
    }
    xhi = xlo = [];
  }
  if (is_void(dy)) {
    err = (! is_void(ylo)) + 2*(! is_void(yhi));
  } else {
    ylo = y - dy;
    yhi = y + dy;
    err = 3;
  }
  if (err) {
    pldj, x, (is_void(ylo) ? y : ylo), x, (is_void(yhi) ? y : yhi),
      type=type, width=width, color=edgecolor;
    if (ticks) {
      xm = [-u1, u1];
      ym = [ u0, u0];
      if      (err == 1) _pl_points,    ylo,       x;
      else if (err == 2) _pl_points,    yhi,       x;
      else               _pl_points, [ylo, yhi], [x, x];
    }
    yhi = ylo = [];
  }

  /* symbols */
  p_symbol, symbol, P_CROSS;
  if (! symbol) {
    return;
  }
  if (symbol == P_POINT) {
    /* point */
    fillable = 0n;
    xm = [0, 0];
    ym = [0, 0];
    fill = 0;
  } else if (symbol == P_PLUS) {
    /* + cross */
    fillable = 0n;
    xm = [-u1, u1, u0, u0, u0, u0];
    ym = [ u0, u0, u0, u1,-u1, u0];
    fill = 0;
  } else if (symbol == P_ASTERISK) {
    /* asterisk (x and +) */
    fillable = 0n;
    u2 = u1*sqrt(0.5);
    xm = [-u1, u1, u0, u0, u0, u0, u2,-u2, u0, u2,-u2, u0];
    ym = [ u0, u0, u0, u1,-u1, u0, u2,-u2, u0,-u2, u2, u0];
    fill = 0;
  } else if (symbol == P_CIRCLE) {
    /* hexagon */
    fillable = 1n;
    u2 = u1*0.5;
    u3 = u1*sqrt(0.75);
    xm = [ u1, u2,-u2,-u1,-u2, u2];
    ym = [ u0, u3, u3, u0,-u3,-u3];
  } else if (symbol == P_CROSS) {
    /* x cross (rotated 45 degrees) */
    fillable = 0n;
    u2 = u1*sqrt(0.5);
    xm = [u2,-u2, u0, u2,-u2, u0];
    ym = [u2,-u2, u0,-u2, u2, u0];
    fill = 0;
  } else if (symbol == P_SQUARE) {
    /* square */
    fillable = 1n;
    u2 = u1*sqrt(0.5);
    xm = [-u2, u2, u2,-u2];
    ym = [ u2, u2,-u2,-u2];
  } else if (symbol == P_DIAMOND) {
    /* diamond */
    fillable = 1n;
    xm = [u1, u0,-u1, u0];
    ym = [u0, u1, u0,-u1];
  } else if (symbol == P_STAR) {
    /* 5 branch star
     *   C18 = cos(18*ONE_DEGREE)
     *   S18 = sin(18*ONE_DEGREE)
     *   C54 = cos(54*ONE_DEGREE)
     *   S54 = sin(54*ONE_DEGREE)
     */
    fillable = 1n;
    u2 = 0.224514*u1; // C54*S18/S54
    u3 = 0.309017*u1; // S18
    u4 = 0.951057*u1; // C18
    u5 = 0.363271*u1; // C18*S18/S54
    u6 = 0.118034*u1; // S18*S18/S54
    u7 = 0.587785*u1; // C54
    u8 = 0.809017*u1; // S54
    u9 = 0.381966*u1; // S18/S54
    xm = [ u0, u2, u4, u5, u7, u0,-u7,-u5,-u4,-u2];
    ym = [ u1, u3, u3,-u6,-u8,-u9,-u8,-u6, u3, u3];
  } else if (P_TRIANGLE_UP <= symbol && symbol <= P_TRIANGLE_RIGHT) {
    /* Draw triangles (the centre of gravity of the triangles are at the point
       locations). */
    fillable = 1n;
    u2 = u1*0.5;
    u3 = u1*0.866025; // sqrt(0.75)
    if (symbol == P_TRIANGLE_UP) {
      xm = [u0, u3,-u3];
      ym = [u1,-u2,-u2];
    } else if (symbol == P_TRIANGLE_DOWN) {
      xm = [ u0, u3,-u3];
      ym = [-u1, u2, u2];
    } else if (symbol == P_TRIANGLE_LEFT) {
      xm = [-u1, u2, u2];
      ym = [ u0, u3,-u3];
    } else  /* P_TRIANGLE_RIGHT */ {
      xm = [ u1,-u2,-u2];
      ym = [ u0, u3,-u3];
    }
  } else {
    /* N-side polygon in unit circle */
    fillable = 1n;
    n = abs(symbol);
    PI = 3.141592653589793238462643383279503;
    a = (2.0*PI/symbol)*indgen(0:n-1);
    xm = u1*cos(a);
    ym = u1*sin(a);
  }
  _pl_points, y, x;
}

plp = pl_points;

func _pl_points(y, x)
/* DOCUMENT _pl_points, x, y;
     Private routine used by pl_points. */
{
  extern xm, ym, edgecolor, fillcolor, fill, legend, width;
  local z;
  n = array(1, 1 + numberof(y));
  m = numberof(ym);
  if (m > 2) {
    if (fill) {
      z = array(fillcolor, numberof(n));
    } else if (fillable) {
      /* Draw inside and ouside edges to emulate 'open' (transparent)
         symbols. */
      m += m;
      grow, xm, xm(1), xm(0:2:-1);
      grow, ym, ym(1), ym(0:2:-1);
    }
  }
  n(1) = m;
  plfp, z, grow(ym,y(*)), grow(xm,x(*)), n,
    legend=legend, edges=1, ewidth=width, ecolor=edgecolor;
}

/*---------------------------------------------------------------------------*/
/* PLOTTING OF SIMPLE SHAPES */

func pl_box(x0, y0, x1, y1, color=, legend=,
            label=, textcolor=, font=, height=, anchor=, margin=,
            linecolor=, type=, width=)
/* DOCUMENT pl_box, x0, y0, x1, y1;
         or pl_box, [x0, x1, y0, y1];

     Draw a the outline of rectangular box with oposite corners (x0,y0) and
     (x1,y1).

     Keywords LINECOLOR, TYPE and WIDTH can be used to specify the attributes
     of the line to draw the box frame.  If LINECOLOR is not specified, the
     value of keyword COLOR is used (which is "black" by default).  If WIDTH is
     ste to a strictly negative value, the frame is not drawn.

     Keyword LABEL can be used to specify a label to write at one of the sides
     of the box.  Keyword ANCHOR indicates where to draw the label if any.  The
     label position for the different anchor values (an integer taken modulo
     12) is indicated in the figure below.  The default anchor is 0 (top-left
     and horizontal).  Keyword MARGIN can be used to specify the distance (in
     the same units as the coordinates of the box corners) the spacing between
     the label and anchor position.  Keywords TEXTCOLOR, FONT and HEIGHT can be
     used to specify the attributes of the drawn text.  If TEXTCOLOR is not
     specified, the value of keyword COLOR is used (which is "black" by
     default).

     +- 0 --- 1 --- 2 -+
     11                3
     |                 |
     |                 |
     10                4
     |                 |
     |                 |
     9                 5
     +- 8 --- 7 --- 6 -+

   SEE ALSO: plg, plt, pl_cbox, p_color, p_type, p_font;
 */
{
  nil = string();
  if (is_void(legend)) legend=nil;
  p_color, color, P_BLACK;
  p_real, width, 4.0;
  if (is_void(y0) && numberof(x0) == 4) {
    y1 = x0(4);
    y0 = x0(3);
    x1 = x0(2);
    x0 = x0(1);
  }
  if (width >= 0) {
    /* Draw box frame. */
    p_color, linecolor, color;
    p_type, type, P_SOLID;
    if (is_void(legend)) legend=nil;
    _pl_orig_plg, [y0, y0, y1, y1], nil, [x0, x1, x1, x0], nil, closed=1n,
      width=width, type=type, color=linecolor, marks=0n, legend=legend;
  }
  if (label && strlen(label)) {
    /* Draw label. */
    p_color, textcolor, color;
    p_real, height, 12.0;
    p_font, font, "helvetica";
    p_real, margin, abs(x1 - x0)*0.07;
    p_integer, anchor, 0;
    anchor %= 12;
    if (anchor < 0) anchor += 12;
    if (anchor == 0) {
      orient = 0;
      justify = "LH";
      x = x0 + margin;
      y = y1;
    } else if (anchor == 1) {
      orient = 0;
      justify = "CH";
      x = (x0 + x1)/2.0;
      y = y1;
    } else if (anchor == 2) {
      orient = 0;
      justify = "RH";
      x = x1 - margin;
      y = y1;
    } else if (anchor == 3) {
      orient = 3;
      justify = "LH";
      x = x1;
      y = y1 - margin;
    } else if (anchor == 4) {
      orient = 3;
      justify = "CH";
      x = x1;
      y = (y0 + y1)/2.0;
    } else if (anchor == 5) {
      orient = 3;
      justify = "RH";
      x = x1;
      y = y0 + margin;
    } else if (anchor == 6) {
      orient = 0;
      justify = "RH";
      x = x1 - margin;
      y = y0;
    } else if (anchor == 7) {
      orient = 0;
      justify = "CH";
      x = (x0 + x1)/2.0;
      y = y0;
    } else if (anchor == 8) {
      orient = 0;
      justify = "LH";
      x = x0 + margin;
      y = y0;
    } else if (anchor == 9) {
      orient = 1;
      justify = "LH";
      x = x0;
      y = y0 + margin;
    } else if (anchor == 10) {
      orient = 1;
      justify = "CH";
      x = x0;
      y = (y0 + y1)/2.0;
    } else {
      orient = 1;
      justify = "RH";
      x = x0;
      y = y1 - margin;
    }
    _pl_orig_plt, " " + label + " ", x, y, tosys=plsys(), opaque=1n,
      orient=orient, justify=justify, font=font, color=textcolor,
      height=p_real(height, P_DEFAULT_HEIGHT)/P_NDC_POINT;
  }
}

func pl_cbox(x0, y0, xsize, ysize, color=, legend=,
             label=, textcolor=, font=, height=, anchor=, margin=,
             linecolor=, type=, width=)
/* DOCUMENT pl_cbox, x0, y0, size;
         or pl_cbox, x0, y0, xsize, ysize;

     Draw a SIZE by SIZE square box or a XSIZE by YSIZE rectangular box
     centered around (X0,Y0).  Keywords COLOR, WIDTH and TYPE can be used and
     have the same meaning as for builtin routine "plg".  If keyword LEGEND is
     not set, an empty legend will be used.

   SEE ALSO plg, pl_box, pl_circle, pl_ellipse,
            p_color, p_type. */
{
  if (is_void(ysize)) ysize = xsize;
  pl_box, x0 - 0.5*xsize, y0 - 0.5*ysize, x0 + 0.5*xsize, y0 + 0.5*ysize,
    color=color, legend=legend, label=label, textcolor=textcolor, font=font,
    height=height, anchor=anchor, margin=margin, linecolor=linecolor,
    type=type, width=width;
}

func pl_fbox(x0, y0, x1, y1, color=, edges=, ecolor=, ewidth=)
/* DOCUMENT pl_fbox, x0, y0, x1, y1;

     Plot filled boxes with opposite corners at coordinates (X0,Y0) and
     (X1,Y1).

     Keyword COLOR can be used to specify the color(s) of the boxes.

     Keywords EDGES, ECOLOR and EWIDTH can be used to specify whether to draw
     the edges of the bars, their color and the width of the line to use.

     A current limitation is that the boxes may have different colors but their
     egdes must have the same attributes (color, width, etc.).

   SEE ALSO: pl_box, pl_cbox, p_color.
 */
{
  n = p_real_vector(x0, 0, "X0");
  p_real_vector, y0, n, "Y0";
  p_real_vector, x1, n, "X1";
  p_real_vector, y1, n, "Y1";
  _pl_fbox;
}

func _pl_fbox /* plot filled rectangles, *PRIVATE* function, all arguments
                 passed externally */
{
  extern x0, y0, x1, y1, color, edges, ecolor, ewidth;

  /* Get colors as RGB triplets. */
  n = numberof(x0);
  color = p_rgb_colors(color, dimsof(x0), P_GRAYB);

  /* Parse attributes for drawing egdes. */
  if (edges) {
    edges = 1n;
    p_color, ecolor, P_FG;
    p_real, ewidth, 1.0;
  } else {
    edges = 0n;
    ecolor = [];
    ewidth = [];
  }

  bx = array(double, 2*n);
  bx(1::2) = x0;
  bx(2::2) = x1;
  bx = bx(,-:1:2);

  by = array(double, 2*n, 2);
  by(,1) = y0(-:1:2,)(*);
  by(,2) = y1(-:1:2,)(*);

  bz = array(char, 3, 2*n - 1, 1);
  bz(,1::2,) = color;

  ireg = array(long, 2*n, 2);
  ireg(2::2,2) = 1;

  plf, bz, by, bx, ireg, edges=edges, ecolor=ecolor, ewidth=ewidth;
}

func pl_circle(x0, y0, r, color=, width=, number=, type=, legend=)
/* DOCUMENT pl_circle, x0, y0, r;

     Draw circle(s) of radius R around (X0,Y0).  Value of keyword NUMBER tells
     how many segments to use to approximate the circle (default 20).  Keywords
     COLOR, WIDTH and TYPE can be used and have the same meaning as for "plg"
     routine (which see). If keyword LEGEND is not set, an empty legend will be
     used.  Arguments may be conformable arrays to draw several circles in one
     call (but keywords must have scalar values if any).

   SEE ALSO plg, pl_box, pl_cbox, pl_ellipse,
            p_color, p_type. */
{
  nil = string();
  if (is_void(legend)) legend = nil;
  if (is_void(number)) number = 20;
  p_color, color, P_FG;
  p_type, type, P_SOLID;
  PI = 3.14159265358979323848;
  t = (2.0*PI/number)*indgen(number);
  cos_t = cos(t);
  sin_t = sin(t);
  if (is_void((dims = dimsof(x0, y0, r)))) {
    error, "non conformable arguments";
  }
  if (dims(1) != 0) {
    /* Draw several circles. */
    zero = array(double, dims);
    n = numberof(zero);
    x0 += zero;
    y0 += zero;
    r  += zero;
    for (i = 1; i <= n; ++i) {
      _pl_orig_plg, y0(i) + r(i)*cos_t, nil, x0(i) + r(i)*sin_t, nil,
        width=width, color=color, type=type, marks=P_FALSE, closed=P_TRUE,
        legend=legend;
    }
  } else {
    /* Draw a single circle. */
    _pl_orig_plg, y0 + r*cos_t, nil, x0 + r*sin_t, nil,
      width=width, color=color, type=type, marks=P_FALSE, closed=P_TRUE,
      legend=legend;
  }
}

func pl_ellipse(x0, y0, a, b, theta, color=, width=, number=, type=, legend=)
/* DOCUMENT pl_ellipse, x0, y0, a, b, theta;

     Draw ellipse(s) centered at (X0,Y0) with semi-axis A and B.  THETA is the
     angle (in degrees counterclockwise) of the axis of semi-length A with
     horizontal axis.  Value of keyword NUMBER tells how many segments to use
     to approximate the circle (default 20).  Keywords COLOR, WIDTH and TYPE
     can be used and have the same meaning as for "plg" routine (which see).
     If keyword LEGEND is not set, an empty legend will be used.  Arguments may
     be conformable arrays to draw several ellipses in one call (but keywords
     must have scalar values if any).

   SEE ALSO plg, pl_box, pl_cbox, pl_circle,
            p_color, p_type. */
{
  nil = string();
  if (is_void(legend)) legend = nil;
  if (is_void(number)) number = 20;
  p_color, color, P_FG;
  p_type, type, P_SOLID;
  PI = 3.14159265358979323848;
  t = (2.0*PI/number)*indgen(number);
  cos_t = cos(t);
  sin_t = sin(t);
  if (is_void((dims = dimsof(x0, y0, a, b, theta)))) {
    error, "non conformable arguments";
  }
  if (dims(1) != 0) {
    /* Draw several ellipses. */
    zero = array(double, dims);
    n = numberof(zero);
    x0 += zero;
    y0 += zero;
    a  += zero;
    b  += zero;
    theta = (PI/180.0)*theta + zero;
    for (i=1 ; i<=n ; ++i) {
      u = a(i)*cos_t;
      v = b(i)*sin_t;
      t = theta(i);
      cs = cos(t);
      sn = sin(t);
      _pl_orig_plg, y0(i) + u*sn + v*cs, nil, x0(i) + u*cs - v*sn, nil,
        width=width, color=color, type=type, marks=P_FALSE,
        closed=P_TRUE, legend=legend;
    }
  } else {
    /* Draw a single ellipse. */
    theta *= (PI/180.0);
    u = a*cos_t;
    v = b*sin_t;
    cs = cos(theta);
    sn = sin(theta);
    _pl_orig_plg, y0 + u*sn + v*cs, nil, x0 + u*cs - v*sn, nil,
      width=width, color=color, type=type, marks=P_FALSE,
      closed=P_TRUE, legend=legend;
  }
}

local pl_hline, pl_vline;
/* DOCUMENT pl_hline, y;
         or pl_hline, y, x0, x1;
         or pl_vline, x;
         or pl_hline, x, y0, y1;

     Plots a horizontal or vertical lines.  If X0 and X1 (resp. Y0 and Y1) are
     not specified, the current viewport limits are used.

   KEYWORDS color, type, width.
   SEE ALSO pldj, limits, p_color, p_type.
*/

func pl_hline(y, x0, x1, color=, width=, type=)
{
  if (! is_void(color)) p_color, color;
  if (! is_void(type)) p_type, type;
  bits = is_void(x0) | (is_void(x1) << 1);
  if (bits != 0) {
    lim = _p_orig_limits();
    if ((bits&1) != 0) x0 = lim(1);
    if ((bits&2) != 0) x1 = lim(2);
  }
  nil = string();
  one = array(1.0, dimsof(y));
  _pl_orig_pldj, one*x0, nil, y, nil, one*x1, nil, y, nil,
    color=color, width=width, type=type;
}

func pl_vline(x, y0, y1, color=, width=, type=)
{
  if (! is_void(color)) p_color, color;
  if (! is_void(type)) p_type, type;
  bits = is_void(y0) | (is_void(y1) << 1);
  if (bits != 0) {
    lim = _p_orig_limits();
    if ((bits&1) != 0) y0 = lim(3);
    if ((bits&2) != 0) y1 = lim(4);
  }
  nil = string();
  one = array(1.0, dimsof(x));
  _pl_orig_pldj, x, nil, one*y0, nil, x, nil, one*y1, nil,
    color=color, width=width, type=type;
}

/*---------------------------------------------------------------------------*/
/* HINTON DIAGRAM */

func pl_hinton(a, x0, y0, x1, y1, cmin=, cmax=,
               bg=, neg=, pos=,
               pad=, debug=)
/* DOCUMENT pl_hinton, a;
         or pl_hinton, a, x0, y0, x1, y1;

     This subroutine plots a Hinton diagram which is a way of visualizing
     numerical values in the matrix/vector A, popular in the neural networks
     and machine learning literature.  Each element of A is represented by a
     square, the area occupied by the square is proportional to the magnitude
     of the value, and the colour (black/white by default) indicates its sign
     (negative/positive).

     Optional arguments X0, Y0, X1 and Y1 can be used to specify the
     coordinates of the centers of the first and last cells.

     Keywords BG, NEG and POS can be used to specify the colors of the
     background, the negative cells and the positive cells (respectively).  See
     p_color for means to specify a color.

     Keyword PAD can be used to specify the minimum amount of padding between
     cells.  PAD is a relative value in the range 0 (no padding) to 1 (full
     padding).  The default value is PAD = 0.05.

     Keywords CMIN and CMAX can be used to specify the minimum and maximum
     values to consider.

   SEE ALSO: plf, p_color.
 */
{
  if (is_void(pad)) {
    pad = 0.05;
  } else if (is_scalar(pad) && identof(pad) <= Y_DOUBLE
             && pad >= 0.0 && pad <= 1.0) {
    pad = double(pad);
  } else {
    error, "value of keyword PAD must be in (0,1)";
  }
  if (identof(a) > Y_DOUBLE || (d = dimsof(a))(1) != 2) {
    error, "expecting a 2-D real array";
  }
  nx = d(2);
  ny = d(3);
  if (is_void(cmin)) cmin = min(a);
  if (is_void(cmax)) cmax = max(a);
  q = max(abs(cmin), abs(cmax));
  if (q == 0.0) {
    return;
  }
  q = (1.0 - pad)/(2.0*sqrt(q));
  t = sqrt(abs(min(max(cmin, a), cmax)));

  /* colors */
  p_color, bg,  P_GRAYC;
  p_color, neg, P_BLACK;
  p_color, pos, P_WHITE;
  rgb = (structof(bg)  == char ||
         structof(neg) == char ||
         structof(pos) == char);
  if (rgb) {
    if (structof(bg) == long) {
      bg = p_indexed_color_as_rgb_triplet(bg);
    } else if (structof(bg) == int) {
      bg = p_packed_color_as_rgb_triplet(bg);
    }
    if (structof(neg) == long) {
      neg = p_indexed_color_as_rgb_triplet(neg);
    } else if (structof(neg) == int) {
      neg = p_packed_color_as_rgb_triplet(neg);
    }
    if (structof(pos) == long) {
      pos = p_indexed_color_as_rgb_triplet(pos);
    } else if (structof(pos) == int) {
      pos = p_packed_color_as_rgb_triplet(pos);
    }
  } else {
    bg = char(bg);
    pos = char(pos);
    neg = char(neg);
  }

  /* Fill the z-cells with the background color (gray) and then with
     foreground color (black/white) depending on the sign. */
  z = array(bg, 2*nx + 1, 2*ny + 1);
  i = where(a > 0.0);
  if (is_array(i)) {
    if (rgb) i = ((--i)%nx)*6 + (i/nx)*(12*nx + 6) + (6*nx + 7);
    else     i = ((--i)%nx)*2 + (i/nx)*(4*nx + 2) + (2*nx + 3);
    z(i) = pos;
  }
  i = where(a < 0.0);
  if (is_array(i)) {
    if (rgb) i = ((--i)%nx)*6 + (i/nx)*(12*nx + 6) + (6*nx + 7);
    else     i = ((--i)%nx)*2 + (i/nx)*(4*nx + 2) + (2*nx + 3);
    z(i) = neg;
  }

  if (is_void(x0)) x0 = 1.0;
  if (is_void(y0)) y0 = 1.0;
  if (is_void(x1)) x1 = x0 + nx - 1.0;
  if (is_void(y1)) y1 = y0 + ny - 1.0;
  xstp = (nx > 1 ? (x1 - x0)/(nx - 1.0) : 1.0);
  ystp = (ny > 1 ? (y1 - y0)/(ny - 1.0) : 1.0);

  xmin = x0 - 0.5*xstp;
  xmax = x1 + 0.5*xstp;
  ymin = y0 - 0.5*ystp;
  ymax = y1 + 0.5*ystp;

  lx = 2*nx + 2;
  ly = 2*ny + 2;

  i0 = 2:lx-2:2;
  i1 = 3:lx-1:2;
  j0 = 2:ly-2:2;
  j1 = 3:ly-1:2;

  s = (xstp*q)*t;
  c = p_span(x0, x1, nx)(,-);
  a = c - s;
  b = c + s;
  x = array(double, lx, ly);
  x(1,) = xmin;
  x(0,) = xmax;
  x(i0, j0) = a;
  x(i0, j1) = a;
  x(i1, j0) = b;
  x(i1, j1) = b;
  x(,1) = x(,2);
  x(,0) = x(,-1);

  s = (ystp*q)*t;
  c = p_span(y0, y1, ny)(-,);
  a = c - s;
  b = c + s;
  y = array(double, lx, ly);
  y(,1) = ymin;
  y(,0) = ymax;
  y(i0, j0) = a;
  y(i0, j1) = b;
  y(i1, j0) = a;
  y(i1, j1) = b;
  y(1,) = y(2,);
  y(0,) = y(-1,);

  if (debug) {
    plf, z, y, x, edges=1n;
  } else {
    plf, z, y, x, edges=0n;
  }
}

/*---------------------------------------------------------------------------*/
/* TITLE, LABELS AND LEGEND BOX */

func pl_title(str1, str2, str3, str4,
              top=, bottom=, left=, right=,
              font=, color=, height=, textstyle=, emph=)
/* DOCUMENT pl_title, title;
         or pl_title, xtitle, ytitle;
         or pl_title, title, xtitle, ytitle;
         or pl_title, top_title, bottom_title, left_title, right_title;

     Plot title(s) and/or label(s) around the current viewport.  With a single
     argument, TITLE is plotted centered above the viewport; with two
     arguments, XTITLE and YTITLE are plotted on the bottom and the left of the
     viewport; otherwise, the different labels are plotted around the viewport
     (an empty string can be used to omit any of the labels).

     Keywords TOP, BOTTOM, LEFT and RIGHT can be used to adjust the position of
     the text on a given side. The value is in NDC units, a positive value
     moves the label away for the viewport while a negative value moves it in
     the opposite direction.

     Keywords FONT, COLOR, and HEIGHT can be used to customize the text style.
     Alternatively, the TEXTSTYLE keyword can be used to specify a
     GpTextAttribs structure with text attributes.  By default, the text
     attributes of the x-axis are used except that, with a single argument, the
     title is plotted in boldface and in a slightly larger size.  As all other
     lengths, text height is given in NDC units (not in points as assumed by
     the `plt` command).

     Keyword EMPH can be set to specify how to emphasis the TITLE text (not
     other labels).  EMPH can be a scalar integer whose bits indicate how to
     emphasis text title: use bold face if the lowest significant bit of EMPH
     is set and/or magnify text size by a factor 1.4 if the second lowest
     significant bit of EMPH is set.  For finer tuning, EMPH can be set with a
     floating point value: the absolute value of EMPH is the magnification
     factor and bold face is used if EMPH is strictly positive.  The default is
     to have no emphasis.


   SEE ALSO: pl_text, plt.
 */
{
  mode = ((is_void(str1) ? 0 : 1) |
          (is_void(str2) ? 0 : 2) |
          (is_void(str3) ? 0 : 4) |
          (is_void(str4) ? 0 : 8));
  if (is_void(textstyle)
      && (is_void(font) || is_void(color) || is_void(height))) {
    textstyle = p_query_text_style(1);
    p_font, font, textstyle.font;
    p_color, color, textstyle.color;
    p_real, height, textstyle.height;
  } else {
    p_font, font;
    p_color, color;
    p_real, height;
  }
  top    = p_real(top,    0.0) + 1.3*height;
  bottom = p_real(bottom, 0.0) + 2.3*height;
  left   = p_real(left,   0.0) + 3.2*height;
  right  = p_real(right,  0.0) + 1.3*height;
  port = _p_orig_viewport();

  /* plt command takes the text height in points */
  textheight = height/P_NDC_POINT;

  /* Emphasis for the title. */
  if (is_void(emph)) emph = 0;
  if (is_scalar(emph)) {
    if (is_integer(emph)) {
      topfont = (((mode == 1 || mode == 7) && (emph&1) != 0)
                  ? (font | P_BOLD) : font);
      toptextheight = (((mode == 1 || mode == 7) && (emph&2) != 0)
                    ? 1.4*textheight : textheight);
    } else if (is_real(emph)) {
      topfont = (((mode == 1 || mode == 7) && emph > 0.0)
                  ? (font | P_BOLD) : font);
      toptextheight = (((mode == 1 || mode == 7) && emph != 0.0)
                    ? abs(emph)*textheight : textheight);
    } else {
      error, "unexpected value for keyword EMPH";
    }
  }

  if (mode == 1) {
    /* draw the plot title */
    _pl_orig_plt, str1, port(zcen:1:2)(1), port(4) + top,
      font=topfont, justify="CB", height=toptextheight, color=color;
  } else if (mode == 3) {
    /* draw the normal x and y axis titles */
    if (str1 && strlen(str1)) {
      _pl_orig_plt, str1, port(zcen:1:2)(1), port(3) - bottom, orient=0,
        font=font, justify="CT", height=textheight, color=color;
    }
    if (str2 && strlen(str2)) {
      _pl_orig_plt, str2, port(1) - left, port(zcen:3:4)(1), orient=1,
        font=font, justify="CB", height=textheight, color=color;
    }
  } else {
    /* draw the surrounding labels */
    if (str1 && strlen(str1)) {
      _pl_orig_plt, str1, port(zcen:1:2)(1), port(4) + top,
        font=topfont, justify="CB", height=toptextheight,
        color=color, orient=0;
    }
    if (str2 && strlen(str2)) {
      _pl_orig_plt, str2, port(zcen:1:2)(1), port(3) - bottom, orient=0,
        font=font, justify="CT", height=textheight, color=color;
    }
    if (str3 && strlen(str3)) {
      _pl_orig_plt, str3, port(1) - left, port(zcen:3:4)(1), orient=1,
        font=font, justify="CB", height=textheight, color=color;
    }
    if (str4 && strlen(str4)) {
      _pl_orig_plt, str4, port(2) + right, port(zcen:3:4)(1), orient=3,
        font=font, justify="CB", height=textheight, color=color;
    }
  }
}

func pl_legend_box(nrows, ncols, viewport=, anchor=,
                   units=, length=, offset=,
                   type=, color=, width=)
/* DOCUMENT lb = pl_legend_box(nrows, ncols);

     Create a new legend bow in the current graphic.  The returned value is an
     array of 3 values which be used by pl_legend_add() to add a line in the
     legend box.

     Keywords WIDTH, TYPE and COLOR can be used to set the line width, type and
     color for the frame of the legend box.  If WIDTH is set to a negative
     value the frame is not drawn.

     Keyword ANCHOR can be used to set the position of the legend box within
     the viewport.  Keyword VP can be used to specify the viewport of the plot
     in NDC coordinates.  By default, the viewport of the current plotting
     system is used.

     The size of the box is NCOLS-by-NROWS muliplied by the value of the UNITS
     keyword ( by default) in NDC units.  The default value of UNITS
     corresponds the height of the text used to label the axis.  There is an
     internal UNITS/2 margin in the legend box.  The number of rows NROWS
     should be integer, but the number of columns NCOLS can be fractional.  The
     items in the legend box are left aligned and the legend box looks like:

                     ncols
         <-------------------------->
             length  offset
           <-------> <---->
         +--------------------------+
         | ====X====        text1   |
         | ----Z----        text2   |
         +--------------------------+

     where the first column is to draw a line (possibly with a symbol).
     Keywords LENGTH and OFFSET can be used to specify the width of the first
     column and the spacing with the text, both in the same units as NCOLS and
     NROWS (by default LENGTH=2 and OFFSET=0.5).


   SEE ALSO: pl_legend_add, p_viewport_ndc.
 */
{
  /* Get the viewport in NDC units. */
  if (is_void(viewport)) viewport = p_viewport_ndc();
  x0 = viewport(1);
  x1 = viewport(2);
  y0 = viewport(3);
  y1 = viewport(4);

  /* get the sizes and units */
  if (is_void(units)) {
    height = p_query_text_style(1).height;
    units = 1.5*height;
  }
  if (is_void(length)) length = 2.0;
  if (is_void(offset)) offset = 0.5;
  ysize = nrows*units;
  xsize = ncols*units;
  xmargin = 0.5*units;
  ymargin = 0.5*units;

  /* position of the box */
  p_anchor, anchor, P_NORTHEAST;
  if (anchor == 0) anchor = P_NORTHEAST;
  a = (anchor & 3);
  if (a == P_LEFT) {
    x0 = x0 + xmargin;
  } else if (a == P_CENTER) {
    x0 = (x0 + x1 - xsize)/2.0;
  } else if (a == P_RIGHT) {
    x0 = x1 - (xsize + xmargin);
  } else {
    error, "illegal value for ANCHOR keyword";
  }
  a = (anchor & 28);
  if (a == P_TOP) {
    y0 = y1 - (ysize + ymargin);
  } else if (a == P_HALF) {
    y0 = (y0 + y1 - ysize)/2.0;
  } else if (a == P_BOTTOM) {
    y0 = y0 + ymargin;
  } else {
    error, "illegal value for ANCHOR keyword";
  }
  x1 = x0 + xsize;
  y1 = y0 + ysize;

  if (is_void(width) || width >= 0) {
    /* draw the legend box */
    nil = string();
    sys = plsys(0);
    _pl_orig_plg, [y0, y0, y1, y1], nil, [x0, x1, x1, x0], nil,
      closed=1n, legend=nil, marks=0n,
      type=p_type(type, P_SOLID),
      color=p_color(color, P_FG), width=width;
    plsys, sys;
  }
  return [x0 + 0.5*units, y1 - units, units, length*units, offset*units];
}

func pl_legend_add(lb, text, textcolor=,
                   font=, height=, width=, type=, color=,
                   linecolor=, linewidth=, linetype=,
                   symbol=, size=, fill=, symbolcolor=,
                   fillcolor=, symbolwidth=, edgecolor=)
/* DOCUMENT pl_legend_add, lb, text;

     Add text TXT as a new line in the legend box LB. LB must have been created
     by pl_legend_box and is updated on return.

     Keywords COLOR, TEXTCOLOR and SCOLOR can be used to specify the line,
     text and symbol colors.

   SEE ALSO plt, pl_legend_box.
 */
{
  lx = lb(1);
  ly = lb(2);
  lu = lb(3);
  ll = lb(4);
  lo = lb(5);
  sys = plsys(0);
  p_color, color, P_FG;
  p_color, textcolor, color;
  p_color, linecolor, color;

  p_real, width, 0.0;
  p_real, linewidth, width;
  p_real, symbolwidth, width;

  p_type, type, P_SOLID;
  p_type, linetype, type;

  p_real, height, P_DEFAULT_HEIGHT;

  if (linewidth >= 0) {
    pldj, lx, ly, lx + ll, ly,
      type = p_type(linetype, type),
      color = p_color(linecolor, color),
      width = linewidth;
  }
  if (! is_void(symbol)) {
    pl_points, ly, lx + 0.5*ll,
      symbol = p_symbol(symbol),
      size = size,
      fill = fill,
      color = p_color(symbolcolor, color),
      edgecolor = edgecolor,
      fillcolor = fillcolor,
      width = p_real(symbolwidth, width);
  }
  _pl_orig_plt, text, lx + ll + lo, ly,
    color = p_color(textcolor, color),
    font = p_font(font, P_HELVETICA),
    height = height/P_NDC_POINT,
    justify="LH";
  plsys, sys;
  lb(2) = ly - lu;
}

P_DEFAULT_COLOR = P_FG;
P_DEFAULT_FONT = P_HELVETICA;
P_DEFAULT_HEIGHT = 12*P_NDC_POINT;
P_DEFAULT_WIDTH = 1.0;
P_DEFAULT_JUSTIFY = p_justify("NN");


func pl_text(text, x, y, tosys=, legend=, hide=, opaque=, orient=,
             color=, font=, height=, justify=)
/* DOCUMENT pl_text, txt, x, y;

     Plots text TXT (a string) at position (X,Y).  This subroutine is identical
     to `plt` except that the value of the HEIGHT keyword is given in points
     (not in NDC) units and that the COLOR, FONT and JUSTIFY keywords use
     `p_color`, `p_font` and `p_justify` functions to parse their value.

   SEE ALSO plt, p_color, p_font, p_justify.
*/
{
  _pl_orig_plt, text, x, y, tosys = tosys, legend = legend, hide = hide,
    opaque = opaque, orient = orient,
    color = p_color(color, P_DEFAULT_COLOR),
    font = p_font(font, P_DEFAULT_FONT),
    height = p_real(height, P_DEFAULT_HEIGHT)/P_NDC_POINT,
    justify = p_justify(justify, P_DEFAULT_JUSTIFY);
}
errs2caller, pl_text;

/*---------------------------------------------------------------------------*/
