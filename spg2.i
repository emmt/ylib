/*
 * spg2.i --
 *
 * Implements Spectral Projected Gradient Method (Version 2: "continuous
 * projected gradient direction") to find the local minimizers of a given
 * function with convex constraints, described in:
 *
 * [1] E. G. Birgin, J. M. Martinez, and M. Raydan, "Nonmonotone spectral
 *     projected gradient methods on convex sets", SIAM Journal on Optimization
 *     10, pp. 1196-1211 (2000).
 *
 * [2] E. G. Birgin, J. M. Martinez, and M. Raydan, "SPG: software for
 *     convex-constrained optimization", ACM Transactions on Mathematical
 *     Software (TOMS) 27, pp. 340-349 (2001).
 *
 * ----------------------------------------------------------------------------
 *
 * The MIT License (MIT)
 *
 * Copyright (C) 2014, Éric Thiébaut.
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

_SPG2_VERSION = "$Date$";

SPG2_DBL_MAX = machine_constant("DBL_MAX");

SPG2_WORK_IN_PROGRESS         =  0;
SPG2_CONVERGENCE_WITH_INFNORM =  1;
SPG2_CONVERGENCE_WITH_TWONORM =  2;
SPG2_TOO_MANY_ITERATIONS      = -1;
SPG2_TOO_MANY_EVALUATIONS     = -2;

func spg2_dot(u, v) { return sum(u*v); }
func spg2_twonorm(v) { return sqrt(spg2_dot(v, v)); }
func spg2_infnorm(v) { return max(abs(v)); }

func spg2(fg, prj, x0, m,
          maxit=, maxfc=, eps1=, eps2=, eta=, alt=,
          printer=, verb=)
/* DOCUMENT ws = spg2(fg, prj, x0, m);

     SPG2 implements the Spectral Projected Gradient Method (Version 2:
     "continuous projected gradient direction") to find the local minimizers of
     a given function with convex constraints, described in references [1] and
     [2] below.

     The user must supply the functions FG and PRJ to evaluate the objective
     function and its gradient and to project an arbitrary point onto the
     feasible region.  These functions must be defined as:

        func fg(x, &g) {
          g = gradient_at(x);
          return function_value_at(x);
        }

        func prj(x) {
          return projection_of(x);
        }

     Argument X0 is the initial solution and argument M is the number of
     previous function values to be considered in the nonmonotone line search.
     If M <= 1, then a monotone line search with Armijo-like stopping criterion
     will be used.

     The following keywords are available:
        eps1   - Stopping criterion: ||projected grad||_inf <= eps1.
        eps2   - Stopping criterion: ||projected grad||_2 <= eps2.
        eta    - Scaling parameter for the gradient, the projected gradient is
                 computed as (x - prj(x - eta*g))/eta (with g the gradient at
                 x) instead of x - prj(x - g) which corresponds to the default
                 behavior (same as if eta=1) and is usually used in
                 methodological publications although it does not scale
                 correctly (for instance, if you make a change of variables ro
                 simply multiply the function by some factor).  If option ALT
                 is true, the scaling parameter will only be used to compute
                 the first projected gradient.
        alt    - If set true, use an alternate method to estimate the projected
                 gradient form the search direction (thus saving one projection
                 per iteration).  Note that projected gradient is only used to
                 check the stopping criterion.
        maxit  - Maximum number of iterations.
        maxfc  - Maximum number of function evaluations.
        verb   - If true, print some information at each iteration.
        printer- If specified, a subroutine to print some information at each
                 iteration.  This subroutine will be called with a single
                 argument which is a hash-table WS set with the same members as
                 the result (see below) plus:
                    WS.x - the current solution,
                    WS.f - the function value at x,
                    WS.g - the gradient at x.

     The result WS is a hash-table with the following members:
        WS.xbest  - The approximation to the local minimizer.
        WS.fbest  - The function value at xbest.
        WS.gbest  - The gradient at xbest.
        WS.pginfn - ||projected grad||_inf at the final iteration.
        WS.pgtwon - ||projected grad||_2 at the final iteration.
        WS.iter   - The number of iterations.
        WS.fcnt   - The number of function (and gradient) evaluations.
        WS.pcnt   - The number of projections.
        WS.status - Termination parameter:
          1 = convergence with projected gradient infinite-norm,
          2 = convergence with projected gradient 2-norm,
         -1 = too many iterations,
         -2 = too many function evaluations.


   REFERENCES:

     [1] E. G. Birgin, J. M. Martinez, and M. Raydan, "Nonmonotone spectral
         projected gradient methods on convex sets", SIAM Journal on
         Optimization 10, pp. 1196-1211 (2000).

     [2] E. G. Birgin, J. M. Martinez, and M. Raydan, "SPG: software for
         convex-constrained optimization", ACM Transactions on Mathematical
         Software (TOMS) 27, pp. 340-349 (2001).

   SEE ALSO:
 */
{
  true = 1n;
  false = 0n;
  lmin = 1e-30;
  lmax = 1e+30;
  ftol = 1e-4;
  amin = 0.1;
  amax = 0.9;
  if (is_void(eps1)) eps1 = 1e-6;
  if (is_void(eps2)) eps2 = 1e-6;
  if (is_void(eta)) eta = 1.0;
  if (is_void(maxit)) maxit = -1;
  if (is_void(maxfc)) maxfc = -1;
  improved_method = (alt ? true : false);

  /* Initialization. */
  local sty, sts, x0, f0, g0, x, f, g, d;
  status = SPG2_WORK_IN_PROGRESS;
  iter = 0;
  fcnt = 0;
  pcnt = 0;
  if (m > 1) {
    lastfv = array(-SPG2_DBL_MAX, m);
  } else {
    lastfv = [];
  }

  /* Project initial guess. */
  x = prj(x0);
  ++pcnt;

  /* Evaluate function and gradient. */
  g = true;
  f = fg(x, g);
  ++fcnt;

  /* Initialize best solution and best function value. */
  ws = h_new(xbest = x, fbest = f, gbest = g);

  /* Main loop. */
  while (true) {

    /* Compute continuous projected gradient (and its norms). */
    if (eta == 1.0) {
      pg = x - prj(x - g);
    } else {
      pg = (x - prj(x - eta*g))*(1.0/eta);
    }
    ++pcnt;
    pgtwon = spg2_twonorm(pg);
    pginfn = spg2_infnorm(pg);
    pg = []; // free some memory

    /* Print iteration information */
    if (! is_void(printer)) {
      printer, h_set(ws, status = status, pgtwon = pgtwon, pginfn = pginfn,
                     iter = iter, fcnt = fcnt, pcnt = pcnt,
                     x = x, f = f, g = g);
      h_pop, ws, x=; // release reference
      h_pop, ws, f=; // release reference
      h_pop, ws, g=; // release reference
    }
    if (verb) {
      write,
        format="ITER = %-5d  EVAL = %-5d  PROJ = %-5d  F(%s) =%24.17e  ||PG||oo = %17.10e\n",
        iter, fcnt, pcnt, (f <= ws.fbest ? "+" : "-"), f, pginfn;
    }

    /* Test stopping criteria. */
    if (pginfn <= eps1) {
      /* Gradient infinite-norm stopping criterion satisfied, stop. */
      status = SPG2_CONVERGENCE_WITH_INFNORM;
      break;
    }
    if (pgtwon <= eps2) {
      /* Gradient 2-norm stopping criterion satisfied, stop. */
      status = SPG2_CONVERGENCE_WITH_TWONORM;
      break;
    }
    if (maxit >= 0 && iter >= maxit) {
      /* Maximum number of iterations exceeded, stop. */
      status = SPG2_TOO_MANY_ITERATIONS;
      break;
    }
    if (maxfc >= 0 && fcnt >= maxfc) {
      /* Maximum number of function evaluations exceeded, stop. */
      status = SPG2_TOO_MANY_EVALUATIONS;
      break;
    }

    /* Store function value for the nonmonotone line search and
       find maximum function value since m last calls. */
    if (m > 1) {
      lastfv((iter%m) + 1) = f;
      fmax = max(lastfv);
    } else {
      fmax = f;
    }

    /* Compute spectral steplength. */
    if (iter == 0) {
      /* Initial steplength. */
      lambda = min(lmax, max(lmin, 1.0/pginfn));
    } else {
      s = x - x0;
      y = g - g0;
      sty = spg2_dot(s, y);
      y = []; // free some memory
      if (sty > 0.0) {
        /* Safeguarded Barzilai & Borwein spectral steplength. */
        sts = spg2_dot(s, s);
        lambda = min(lmax, max(lmin, sts/sty));
      } else {
        lambda = lmax;
      }
      s = []; // free some memory
    }

    /* Save current point. */
    eq_nocopy, x0, x;
    eq_nocopy, g0, g;
    f0 = f;

    /* Compute the spectral projected gradient direction and <G,D> */
    x = prj(x0 - lambda*g0);
    ++pcnt;
    d = x - x0;
    delta = spg2_dot(g0, d);

    /* Nonmonotone line search. */
    stp = 1.0; // Step length for first trial.
    while (true) {
      /* Evaluate function and gradient at trial point. */
      g = true;
      f = fg(x, g);
      ++fcnt;

      /* Compare the new function value against the best function value and, if
         smaller, update the best function value and the corresponding best
         point. */
      if (f < ws.fbest) {
        h_set, ws, fbest = f, xbest = x, gbest = g;
      }

      /* Test stopping criteria. */
      if (f <= fmax + stp*ftol*delta) {
        /* Nonmonotone Armijo-like stopping criterion satisfied, stop. */
        break;
      }
      if (maxfc >= 0 && fcnt >= maxfc) {
        /* Maximum number of function evaluations exceeded, stop. */
        status = SPG2_TOO_MANY_EVALUATIONS;
        break;
      }

      /* Safeguarded quadratic interpolation. */
      q = -delta*(stp*stp);
      r = (f - f0 - stp*delta)*2.0;
      if (r > 0.0 && amin*r <= q && q <= amax*stp*r) {
        stp = q/r;
      } else {
        stp /= 2.0;
      }

      /* Compute trial point. */
      x = x0 + stp*d;
    }

    if (status != SPG2_WORK_IN_PROGRESS) {
      /* The number of function evaluations was exceeded inside the line
         search. */
      break;
    }

    /* Do next iteration. */
    ++iter;

  }
  return h_set(ws, status = status, pgtwon = pgtwon, pginfn = pginfn,
               iter = iter, fcnt = fcnt, pcnt = pcnt);
}

/*
 * Local Variables:
 * mode: Yorick
 * tab-width: 8
 * c-basic-offset: 2
 * fill-column: 79
 * coding: utf-8
 * ispell-local-dictionary: "american"
 * End:
 */
