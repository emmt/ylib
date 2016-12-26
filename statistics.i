/*
 * statistics.i -
 *
 * Density estimation and histogram computation in Yorick.
 *
 * Density estimation is based on "Multivariate density estimation: theory,
 * practice, and visualization," by D. W. Scott (1992, John Wiley & Sons, New
 * York).
 *
 * ---------------------------------------------------------------------------
 *
 * This file is part of YLib available at <https://github.com/emmt/ylib> and
 * licensed under the MIT "Expat" License.
 *
 * Copyright (C) 2001-2016, Éric Thiébaut.
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

func hist(vals, bins, wgts, average=, interpolate=)
/* DOCUMENT hst = hist(vals, bins);
         or hst = hist(vals, bins, wgts);

     Compute an histogram (an empirical distribution).  VALS gives the values
     of the samples, BINS gives the positions of the histogram bins.
     Optionally, WGTS gives the weights of the samples.  VALS can have any
     dimension list, BINS must be a vector of strictly increasing or strictly
     decreasing valus and, if specified, WGTS must be conformable with VALS.
     If WGTS is not specified, the result is as if the weights were all equal
     to one.

     If keyword AVERAGE is true and the weights WGTS are specified, the
     average value of the weights in each bin is computed instead of the total
     value.  If the weights WGTS are unspecified, the AVERAGE keyword is
     ignored -- otherwise the result would be 0 or 1 everywhere.  For the same
     reason, it makes little sense to set AVERAGE true with uniform weights
     (you have been warned).

     If keyword INTERPOLATE is true, a linear interpolation rule is used to
     distribute the (weighted) counts in the bins and the result has the same
     length as BINS and should be interpreted as having sampled the
     distribution at the positions specified by BINS.  By default, the result
     has one more element than BINS and should be interpreted as the number of
     values between successive bins, the first (resp. last) value, HST(1)
     (resp. HST(0)) being the number of samples beyond the first (resp. last)
     bin BINS(1) (resp. BINS(0)).

     This routine is more general than histo2 as it let you choose non-evenly
     spaced bins.

   SEE ALSO: histogram, digitize, histo2.
 */
{
  i = digitize(vals, bins);
  n = numberof(bins);
  if (interpolate) {
    /* compute interpolation coefficients */
    dims = dimsof(vals);
    w1 = array(double, dims);
    i1 = array(long, dims);
    i2 = array(long, dims);
    lo = (i < 2);
    hi = (i > n);
    j = where(lo);
    if (is_array(j)) {
      /* interpolation coefficients for values beyond first bin */
      w1(j) = 0.0;
      i1(j) = 1;
      i2(j) = 1;
    }
    j = where(hi);
    if (is_array(j)) {
      /* interpolation coefficients for values beyond last bin */
      w1(j) = 1.0;
      i1(j) = n;
      i2(j) = n;
    }
    j = where(!(lo|hi));
    if (is_array(j)) {
       /* interpolation coefficients values between first and last bins (the
          weird division in the expression below is an attempt to do as few
          divisions as possible assuming there are more samples than bins) */
      j2 = i(j);
      j1 = j2 - 1;
      w1(j) = (vals(j) - bins(j1))*(1.0/bins(dif))(j1);
      i1(j) = j1;
      i2(j) = j2;
    }
    w2 = 1.0 - w1;
    if (is_void(wgts)) {
      hst = (histogram(i1, w1, top=n+1) +
             histogram(i2, w2, top=n+1));
    } else {
      hst = (histogram(i1, w1*wgts, top=n+1) +
             histogram(i2, w2*wgts, top=n+1));
      if (average) {
        /* normalize by the sum of the interpolation weights in each bin
           taking care of not dividing by zero */
        nrm = (histogram(i1, w1, top=n+1) +
               histogram(i2, w2, top=n+1));
        msk = (nrm > 0.0);
        hst *= double(msk)/(double(!msk) + nrm);
      }
    }
    return hst(1:n);
  } else {
    if (is_void(wgts)) {
      hst = histogram(i, top=n+1);
    } else {
      hst = histogram(i, wgts, top=n+1);
      if (average) {
        /* normalize by the counts in each bin taking care of not dividing by
           zero */
        nrm = histogram(i, top=n+1);
        msk = (nrm > 0.0);
        hst *= double(msk)/(double(!msk) + nrm);
      }
    }
    return hst;
  }
}

/*---------------------------------------------------------------------------*/
