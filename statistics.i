/*
 * statistics.i -
 *
 * Density estimation and histogram computation in Yorick.
 *
 * Density estimation is based on "Multivariate density estimation: theory,
 * practice, and visualization," by D. W. Scott (1992, John Wiley & Sons, New
 * York).
 *
 * ----------------------------------------------------------------------------
 *
 * This file is part of YLib (Yorick Library) which is licensed under the MIT
 * "Expat" License:
 *
 * Copyright (C) 2001, 2014-2015: Éric Thiébaut.
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
 * ----------------------------------------------------------------------------
 */

_STATISTICS_VERSION = "$Date$";

func density(s, x, bw=, adjust=, verb=, kernel=, histo=, exact=,
             nsamples=, debug=)
/* DOCUMENT density(s, x)

     This function estimates the density of 1-D data S at positions X.
     Argument S gives the sampled data, argument X gives the position where to
     estimate the density.  The density is computed by convolving the sample
     density by a smoothing kernel with a bandwidth adapted to the sampled
     data.

     Set keyword HISTO true to compute an histogram rather than a (normalized)
     density.

     Keyword KERNEL can be used to specify the smoothing kernel to use.  The
     value of this keyword can be one of:

       "uniform"            "biweight"           "cosine"
       "triangle"           "triweight"          "cubic B-spline"
       "Epanechnikov"       "normal"             "quadratic B-spline"

     Case does not matter and the few first letters of the kernel name can be
     used as far as the abbreviated name is unique.  By default,
     Epanechnikov's kernel is used (which is theoretically the most
     efficient).

     Keyword BW can be used to specify the bandwidth.  If unspecified, the
     bandwidth is computed according to a rule that is optimal for Gaussian
     distributed data. If keyword VERB is true, the value of the bandwidth
     automatically computed is printed.  The theoretical optimal bandwidth is:

        BW = rms(S)/numberof(S)^0.2

     where rms(S) is the standard deviation of the sampled data.

     Keyword ADJUST can be used to tune the bandwidth, the actual bandwidth is
     ADJUST*BW. By default, ADJUST = 1.

     Keyword EXACT can be set true to use a very slow but exact convolution.
     By default, an approximate convolution is computed by means of FFT (fast
     Fourier transform).  For this fast computation, the number of samples can
     be set with keyword NSAMPLES (by default, NSAMPLES=1024) to tune the
     precision.


   EXAMPLE:
     include, "random.i";
     s = 3.2 + 2.1*random_n(200);
     x = span(-10.0, 10.0, 100);
     plg, density(s, x), x;


   REFERENCES
     [1] D. W. Scott, "Multivariate density estimation: theory, practice, and
         visualization," John Wiley & Sons (New York), 1992.


   SEE ALSO: hist.
 */
{
  // FIXME: there should be a special case for integer valued data

  /* Select the kernel (default is "Epanechnikov"). */
  if (is_void(kernel)) {
    k = 3;
  } else {
    k = where(! strfind(kernel, DENSITY_KERNEL_NAMES, case=0)(1,));
    if (numberof(k) == 1) {
      k = k(1);
    } else {
      error, (numberof(k) ? "ambiguous abbreviated kernel name"
              : "no matching kernel name");
    }
  }
  kernel_bandwidth_factor = DENSITY_KERNEL_BANDWIDTHS(k);

  /* Compute bandwidth. */
  ndata = numberof(s);
  if (is_void(bw)) {
    sigma = s(*)(rms);
    bw = sigma/ndata^0.2;
    if (verb) write, format="estimated bandwidth = %g\n", bw;
  }
  if (! is_void(adjust)) {
    bw *= adjust;
  }
  h = kernel_bandwidth_factor*bw;
  q = 1.0/(histo ? h : h*ndata);

  if (exact) {
    /* Compute density by convolving the data samples with the selected
       kernel. */
  slow:
    f = symbol_def("_density_kernel_convolve_"+DENSITY_KERNEL_FUNCTIONS(k));
    pdf = array(double, dimsof(x));
    n = numberof(x);
    for (i = 1; i <= n; ++i) {
      pdf(i) = q*f(h, s, x(i));
    }
    return pdf;
  }

  /* We will need FFT.  Load FFT utility functions. */
  if (! is_func(fft_best_dim)) include, "fft_utils.i", 1;

  /* Compute an histogram HST on a regular grid using linear interpolation of
     data samples. */
  if (is_void(nsamples)) {
    nsamples = 1024;
  } else {
    nsamples = fft_best_dim(nsamples);
  }
  xmin = min(x);
  xmax = max(x);
  if (xmin == xmax) {
    /* Use "slow" computation. */
    goto slow;
  }
  kernel_width = DENSITY_KERNEL_WIDTHS(k);
  smin = xmin - h*kernel_width;
  smax = xmax + h*kernel_width;
  s = s(where((s >= smin)&(s <= smax)));
  if (is_void(s)) return array(double, dimsof(x));
  smid = (smax + smin)/2.0;
  imid = (nsamples + 1)/2.0;
  step = (smax - smin)/(nsamples - 1);
  xs = step*(indgen(nsamples) - imid) + smid; // positions of samples
  u = (1.0/step)*(s - smid) + imid;
  v = floor(u);
  u -= v;
  i = long(v);
  v = [];
  hst = (histogram(i, 1.0 - u, top=nsamples) +
         histogram(i + 1, u, top=nsamples+1)(:-1));

  /* Convolve histogram HST with selected kernel using FFT. */
  ker = symbol_def("_density_kernel_"+DENSITY_KERNEL_FUNCTIONS(k));
  ker = ker((step/h)*fft_indgen(nsamples));
  if (h < 3.0*step) write, format="WARNING - %s\n",
                      "use more samples with keyword NSAMPLES in density() function";
  if (verb) write, format="refinement factor = %g\n", h/step;
  if (is_func(fftw_plan)) {
    forward = fftw_plan(nsamples, 1, real=1);
    backward = fftw_plan(nsamples, -1, real=1);
    ys = fftw(fftw(hst, forward)*fftw(ker, forward), backward);
  } else {
    ws = fft_setup([1,nsamples]);
    ys = double(fft(fft(hst, +1, setup=ws)*fft(ker, +1, setup=ws), -1, setup=ws));
  }
  if (debug) {
    plg, hst, xs, color="green";
    plg, (1.0/(h*nsamples*sum(ker)))*ys, xs, color="red";
    fft_plg, ker, color="blue", scale = step/h;
  }

  /* Finally interpolate sampled density. */
  return (q/nsamples)*interp(ys, xs, x);
}

DENSITY_KERNEL_NAMES = ["uniform",
                        "triangle",
                        "Epanechnikov",
                        "biweight",
                        "triweight",
                        "normal",
                        "cosine",
                        "cubic B-spline",
                        "quadratic B-spline"];

DENSITY_KERNEL_FUNCTIONS = ["uniform",
                            "triangle",
                            "Epanechnikov",
                            "biweight",
                            "triweight",
                            "normal",
                            "cosine",
                            "cubic_B_spline",
                            "quadratic_B_spline"];

DENSITY_KERNEL_BANDWIDTHS = [3.68622 /*                                    uniform */,
                             2.57603 /* = (2^12*pi)^0.1                    triangle */,
                             2.34491 /* = (40*sqrt(pi))^0.2                Epanechnikov */,
                             2.77794 /*                                    biweight */,
                             3.15448 /*                                    triweight */,
                             1.05922 /* = (4.0/3.0)^0.2                    normal */,
                             2.40971 /* = (pi^13/36.0/(pi^2 - 8.0)^4)^0.1  cosine */,
                             1.82764 /* = (1208.0/105.0*sqrt(pi))^0.2      cubic B-spline */,
                             2.10768 /*                                    quadratic B-spline */];

/* Full width of kernels. */
DENSITY_KERNEL_WIDTHS = [ 1.0    /* uniform */,
                          2.0    /* triangle */,
                          2.0    /* Epanechnikov */,
                          2.0    /* biweight */,
                          2.0    /* triweight */,
                         75.2807 /* normal */,
                          2.0    /* cosine */,
                          3.0    /* cubic B-spline */,
                          4.0    /* quadratic B-spline */];

/* for a cubic B-spline, I found empirically:
 *   BW ~ 1.9 * sigma / n^(1/5)
 *
 * theoretical computation yields (see [1] p. 131, Theorem 6.1):
 *   BW ~ 1.82764 * sigma / n^(1/5)
 */


// FIXME: [-1,1]?
func _density_kernel_uniform(t)
{
  t = abs(t);
  return (t < 0.5) + 0.5*(t == 0.5);
}

func _density_kernel_convolve_uniform(h, t, t0)
{
  return double(numberof(where(abs(t - t0) < h/2.0)));
}

func _density_kernel_triangle(t)
{
  return max(1.0 - abs(t), 0.0);
}

func _density_kernel_convolve_triangle(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    return sum(1.0 - (1.0/h)*u(i));
  }
  return 0.0;
}

func _density_kernel_Epanechnikov(t)
{
  return 0.75*max(0.0, 1.0 - t*t);
}

func _density_kernel_convolve_Epanechnikov(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    u = (1.0/h)*u(i);
    return 0.75*sum(1.0 - u*u);
  }
  return 0.0;
}

func _density_kernel_biweight(t)
{
  t = max(0.0, 1.0 - t*t);
  return 0.9375*t*t;
}

func _density_kernel_convolve_biweight(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    u = (1.0/h)*u(i);
    u = 1.0 - u*u;
    return 0.9375*sum(u*u);
  }
  return 0.0;
}

func _density_kernel_triweight(t)
{
  t = max(0.0, 1.0 - t*t);
  return 1.09375*t*t*t;
}

func _density_kernel_convolve_triweight(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    u = (1.0/h)*u(i);
    u = 1.0 - u*u;
    return 1.09375*sum(u*u*u);
  }
  return 0.0;
}

func _density_kernel_normal(t)
{
  return (1.0/sqrt(2*pi))*exp(-0.5*t*t);
}

func _density_kernel_convolve_normal(h, t, t0)
{
  u = t - t0;
  return sum(exp(-(0.5/(h*h))*u*u))/sqrt(2.0*pi);
}

func _density_kernel_cosine(t)
{
  k = array(double, dimsof(t));
  j = where(abs(t) < 1.0);
  if (is_array(j)) {
    k(j) = (pi/4.0)*cos((pi/2.0)*t(j));
  }
  return k;
}

func _density_kernel_convolve_cosine(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    return (pi/4)*sum(cos((pi/(2*h))*u(i)));
  }
  return 0.0;
}

func _density_kernel_quadratic_B_spline(t)
{
  k = array(double, dimsof(t));
  t = abs(t);
  if (is_array((i = where(t < 1.5)))) {
    t = t(i);
    test = (t <= 0.5);
    if (is_array((j = where(test)))) {
      u = t(j);
      k(i(j)) = 0.75 - u*u;
    }
    if (is_array((j = where(! test)))) {
      u = 1.5 - t(j);
      k(i(j)) = 0.5*u*u;
    }
  }
  return k;
}

func _density_kernel_convolve_quadratic_B_spline(h, t, t0)
{
  s = 0.0;
  u = abs(t - t0);
  if (is_array((i = where(u < 1.5*h)))) {
    u = (1.0/h)*u(i);
    test = (u <= 0.5);
    if (is_array((i = where(test)))) {
      v = u(i);
      s += 0.75*numberof(v) - sum(v*v);
    }
    if (is_array((i = where(! test)))) {
      v = 1.5 - u(i);
      s += 0.5*sum(v*v);
    }
  }
  return s;
}

func _density_kernel_cubic_B_spline(t)
{
  k = array(double, dimsof(t));
  t = abs(t);
  if ((is_array((i = where(t < 2.0))))) {
    test = ((t = t(i)) <= 1.0);
    if (is_array((j = where(test)))) {
      u = t(j);
      k(i(j)) = (0.5*u - 1.0)*u*u + (2.0/3.0);
    }
    if (is_array((j = where(! test)))) {
      u = 2.0 - t(j);
       k(i(j)) = (1.0/6.0)*u*u*u;
    }
  }
  return k;
}

func _density_kernel_convolve_cubic_B_spline(h, t, t0)
{
  s = 0.0;
  t = abs(t - t0);
  if ((is_array((j = where(t < 2.0*h))))) {
    q = 1.0/h;
    test = ((t = t(j)) <= h);
    if (is_array((i = where(test)))) {
      u = q*t(i);
      s = sum((0.5*u - 1.0)*u*u + (2.0/3.0));
    }
    if (is_array((i = where(! test)))) {
      u = 2.0 - q*t(i);
      s += (1.0/6.0)*sum(u*u*u);
    }
  }
  return s;
}

func density_test(s, x, adjust=)
{
  if (is_void(s)) {
    s = random_n(300);
  }
  if (is_void(x)) {
    t = (max(s) - min(s))*1E-2;
    x = span(min(s) - t, max(s) + t, 500);
  }
  n = numberof(DENSITY_KERNEL_NAMES);
  colors = ["DeepSkyBlue","DarkOrange","SpringGreen","DarkRed",
            "DarkViolet","MediumBlue","Goldenrod","DarkKhaki",
            "SaddleBrown", "ForestGreen","DarkCyan"];
  for (i = 1; i <= n; ++i) {
    color = colors(i);
    kernel = DENSITY_KERNEL_NAMES(i);
    plg, density(s, x, kernel=kernel, adjust=adjust), x, color=pl_get_color(color);
    write, format="%20s -> %s kernel\n", color, kernel;
  }
}

/*---------------------------------------------------------------------------*/
/* HISTOGRAM */

func hist(v, w, n=, vmin=, vmax=, edg=, interpolate=)
/* DOCUMENT h = hist(v);
         or h = hist(v, w);

     Compute a 1D histogram of values V.  Optional argument W gives the
     weights of the values (W and V must be conformable).  The result is a
     group object with the following members:

       h.pos  = central position of bins;
       h.edg  = edges of the bins (same as keyword EDG);
       h.cnt  = number of occurrences in bins;
       h.frq  = frequency of occurrences in bins, i.e. cnt/sum(cnt).

     If weights W are provided, then the result has additional members:

       h.mean = mean weight in bins;
       h.dev  = standard deviation of weight in bins.

     Keywords N, VMIN and VMAX can be used to specify the number of bins
     and the endpoints of the range of valid data.  The default is to use
     100 bins and the extreme values in V.  Only data within the range of
     validity will be taken into account (notably to compute the
     frequency).  These keywords are ignored if the EDG keyword is
     specified.

     Keyword EDG can be used to specify the edges of the bins.  They must
     be in strict increasing order.  The number of bins is numberof(EDG)-1
     and the range of valid values is EDG(1) <= V < EDG(0).

     If keyword INTERPOLATE is true, simple linear interpolation is used to
     spread the values (and related weiths if any) between the two nearest
     bins.  In that case, the number of occurrences (given by member CNT)
     is fractional; otherwise, the number of occurences is integer.  When
     interpolation is used, the values slightly beyond or above the range
     of valid values may spread in the result.

   SEE ALSO: density, digitize, histogram.
 */
{
  if (identof(v) > Y_DOUBLE) error, "bad data type for V";
  if (! is_void(w)) {
    /* Make sure weight W and values V are conformable. */
    if (identof(w) > Y_DOUBLE) error, "bad data type for W";
    dims = dimsof(w, v);
    if (is_void(dims)) {
      error, "incompatible dimensions";
    }
    z = array(double, dims);
    v = unref(v) + z;
    w = unref(w) + z;
    z = [];
  }

  /* Digitization pass.  The objective is to produce the indexes i and bin
     boundaries EDG following the same rationale as the digitize function.
     For N bins, there are N+1 bin boundaries (EDG) and index i is in the
     range 1:N+2 and such that:

        EDG(i-1) <= V < EDG(i)      for i = 2:N+1
        V <  EDG(1)   = VMIN         if i = 1
        V >= EDG(N+1) = VMAX         if i = N+2
  */
  if (is_void(edg)) {
    /* Generate bin boundaries. */
    if (is_void(n)) {
      n = 100;
    } else if (is_scalar(n) && is_integer(n) && n >= 1) {
      n = long(n);
    } else {
      error, "error bad number of bins";
    }
    vmin = (is_void(vmin) ? min(v) : vmin) + 0.0;
    vmax = (is_void(vmax) ? max(v) : vmax) + 0.0;
    if (vmin >= vmax)  {
      error, "bad VMIN:VMAX range boundaries";
    }
    t = (1.0/n)*indgen(0:n);
    edg = (1.0 - t)*vmin + t*vmax;
    stp = (vmax - vmin)/n;
  } else {
    /* Use given bin boundaries. */
    if (identof(edg) > Y_DOUBLE) error, "bad data type for EDG";
    n = numberof(edg) - 1; // number of bins
    if (n < 1 || ! is_vector(edg)) error, "bad dimension for EDG";
    edg = double(unref(edg)); // convertion and private copy
    stp = edg(dif); // width of bins
    if (min(stp) <= 0.0) {
      error, "expecting strictly increasing bin bounds";
    }
  }
  i = digitize(v, edg);

  /* Count number of occurences in bins. */
  local u0, u1, i0, i1;
  if (interpolate) {
    /* Compute interpolation coefficients. */
    b = grow(edg, 2*edg(0) - edg(-1)); // upper bound
    s = 1.0/stp;
    if (is_scalar(s)) {
      u = (b(i) - v)*s;
    } else {
      s = grow(s(1), s, s(0));
      u = (b(i) - v)*s(i);
      s = [];
    }
    k = where((0.0 <= u)&(u <= 1.0));
    if (numberof(k) < numberof(u)) {
      /* Exclude samples outside interpolation range. */
      if (! is_array(k)) error, "no valid samples";
      u = u(k);
      i = i(k);
      if (! is_void(w)) w = w(k);
    }
    top = n + 3;
    sel = 3:-1;
    eq_nocopy, u0, u;
    eq_nocopy, u1, 1.0 - unref(u);
    eq_nocopy, i0, i;
    eq_nocopy, i1, unref(i) + 1;
    cnt = (histogram(i0, u0, top=top) +
           histogram(i1, u1, top=top))(sel);
  } else {
    top = n + 2;
    sel = 2:-1;
    cnt = histogram(i, top=top)(sel);
  }
  ntot = sum(cnt);
  frq = (ntot > 0 ? (1.0/ntot)*cnt : array(double, numberof(cnt)));
  rv = save(pos = edg(zcen), edg, cnt, frq);
  if (is_void(w)) return rv;

  /* Compute mean and standard deviation.  In the statistical computations,
     the reasonning is that the interpolation coefficients are
     proportionnal to the probability of a datum to belong to a given bin
     (left one with weight U0 or right one with weight U1). */
  if (interpolate) {
    w0 = w(i0);
    w1 = w(i1);
    u0_w0 = unref(u0)*w0;
    u1_w1 = unref(u1)*w1;
    s1 = (histogram(i0, u0_w0, top=top) +
          histogram(i1, u1_w1, top=top))(sel);
    s2 = (histogram(unref(i0), unref(u0_w0)*unref(w0), top=top) +
          histogram(unref(i1), unref(u1_w1)*unref(w1), top=top))(sel);
  } else {
    s1 = histogram(i, w, top=top)(sel);
    s2 = histogram(i, w*w, top=top)(sel);
  }
  if (min(cnt) > 0.0) {
    s = 1.0/cnt;
  } else {
    k = where(cnt);
    s = array(double, n);
    s(i) = 1.0/cnt(i);
  }
  s1 *= s;
  save, rv, mean = s1, dev = sqrt(max(s2*s - s1*s1, 0.0));
  return rv;
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
