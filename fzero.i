/*
 * fzero.i -
 *
 * Find a local root of an univariate function for Yorick.  The method is based
 * on original Brent's ZEROIN method.
 *
 * ----------------------------------------------------------------------------
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

func fzero(f, a, b, atol=, rtol=, all=)
/* DOCUMENT x = fzero(f, a, b);
         or [x,fx] = fzero(f, a, b, all=1);

     FZERO seeks a local root of a function F(X) in an interval [A,B].

     Argument F is the user-supplied function whose local root is being sought;
     it is called as F(X) to evaluates the function at any X in the interval
     [A,B].  Arguments A and B are the endpoints of the initial search interval
     (in no particular order).  It is assumed that F(A) and F(B) have opposite
     signs.  This is checked, and an error is raised if this is not satisfied.

     FZERO returns a zero X in the given interval [A,B] to within a tolerance:
     RTOL*abs(X) + ATOL.  If keyword ALL is set true, then the retuned value is
     the 2-element vector: [X,F(X)].

     Keywords ATOL and RTOL can be used to specify the absolute and relative
     tolerances for the solution.  The recommended (and default) value for RTOL
     is 4*EPSILON where EPSILON is the relative machine precision defined as
     the smallest representable number such that 1 + EPSILON > 1.


   REFERENCES:

     FZERO has been derived from Richard Brent's FORTRAN77 code ZEROIN
     which itself is a slightly modified translation of the Algol 60
     procedure ZERO given in:

     [1] Richard Brent, "Algorithms for minimization without derivatives,"
         Prentice-Hall, inc. (1973).


   SEE ALSO: fmin.
*/
{
  /* Make sure A and B are double precision values. */
  a += 0.0;
  b += 0.0;

  /* Compute the function value at the endpoints and check the assumptions. */
  fa = f(a);
  if (fa == 0.0) {
    return (all ? [a, fa] : a);
  }
  fb = f(b);
  if (fb == 0.0) {
    return (all ? [b, fb] : b);
  }
  if ((fa > 0.0) == (fb > 0.0)) {
    error, "f(a) and f(b) must have different signs";
  }

  /* Get tolerance parameters. */
  if (is_void(rtol)) {
    rtol =  4.0*FZERO_EPSILON;
  } else {
    rtol += 0.0;
  }
  if (is_void(atol)) {
    atol = FZERO_EPSILON*abs(a - b);
  } else {
    atol += 0.0;
  }

  /* Refine interval until convergence within tolerances. */
  fc = fb; // to trigger bounds update below
  for (;;) {
    if ((fb > 0.0) == (fc > 0.0)) {
      /* Drop point C (make it coincident with point A) and adjust bounds of
         interval. */
      c = a; fc = fa;
      e = d = b - a;
    }

    /* Make sure B is the best point so far. */
    if (abs(fc) < abs(fb)) {
      a = b; fa = fb;
      b = c; fb = fc;
      c = a; fc = fa;
    }

    /* Compute tolerance.  In original Brent's method, the precision and
     * the computed tolerance are given by:
     *    PREC = 4*EPS*abs(X) + 2*T
     *    TOL = 2*EPS*abs(b) + T = PREC/2
     * and we want:
     *    PREC = RTOL*abs(X) + ATOL
     * thus the expression of the tolerance parameter becomes:
     */
    tol = 0.5*(rtol*abs(b) + atol);

    /* Check for convergence. */
    m = (c - b)*0.5;
    if (abs(m) <= tol || fb == 0.0) {
      return (all ? [b, fb] : b);
    }

    /* See if a bisection is forced. */
    if (abs(e) < tol || abs(fa) <= abs(fb)) {
      /* Bisection. */
      d = e = m;
    } else {
      s = fb/fa;
      if (a == c) {
        /* Linear interpolation. */
        p = 2.0*m*s;
        q = 1.0 - s;
      } else {
        /* Inverse quadratic interpolation. */
        q = fa/fc;
        r = fb/fc;
        p = (2.0*m*q*(q - r) - (b - a)*(r - 1.0))*s;
        q = (q - 1.0)*(r - 1.0)*(s - 1.0);
      }
      if (p > 0.0) {
        q = -q;
      } else {
        p = -p;
      }
      two_p = p + p;
      if (two_p < 3.0*m*q - tol*abs(q) && two_p < abs(e*q)) {
        /* Take the interpolation point. */
        e = d;
        d = p/q;
      } else {
        /* Force a bisection. */
        d = e = m;
      }
    }
    a = b; fa = fb;
    if (abs(d) > tol) {
      b += d;
    } else if (m > 0.0) {
      b += tol;
    } else {
      b -= tol;
    }
    fb = f(b);
    if (fb == 0.0) {
      return (all ? [b, fb] : b);
    }
  }
}

local FZERO_EPSILON;
if (is_func(machine_constant)) {
/* DOCUMENT FZERO_EPSILON;
     A global variable set with the machine relative precision.
   SEE ALSO: fzero, machine_constant.
 */
  FZERO_EPSILON = machine_constant("DBL_EPSILON");
} else {
  /* Assume IEEE 64-bit floating point. */
  FZERO_EPSILON = 2.22044604925031308e-16;
}
