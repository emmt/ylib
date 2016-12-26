/*
 * linalg.i -
 *
 * Linear Algebra functions for Yorick.
 *
 * ---------------------------------------------------------------------------
 *
 * This file is part of YLib available at <https://github.com/emmt/ylib> and
 * licensed under the MIT "Expat" License.
 *
 * Copyright (C) 2000, 2014, Éric Thiébaut.
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

func svd_eigen_decomp(A, &Z)
/* DOCUMENT lambda = svd_eigen_decomp(A, Z)

   Computes the spectral decomposition of the symmetric real matrix A by
   means of the singular value decomposition (SVD).  Argument Z is a pure
   output variable used to store a matrix of which the columns are the
   eigen vectors of A and the returned value lambda is an array with the
   corresponding eigen values.

   The spectral decomposition writes:

     A = Z.diag(lambda).Z'

   with diag(lambda) the diagonal matrix with diagonal elements given by
   lambda and Z' the transpose of Z.

   Z is orthogonal, hence: Z'.Z = Z.Z' = I, with I the identity matrix; in
   Yorick notation: Z(,+)*Z(,+) = Z(+,)*Z(+,) = I.

   In Yorick notation, y = A.x is computed as follows:

      y = A(,+)*x(+);
        = Z(,+)*(lambda*(Z(+,)*x(+)))(+);

   SEE ALSO: SVdec.
 */
{
  local U, VT;
  sigma = SVdec(A, U, VT);
  Z = transpose(unref(VT));
  return (unref(U)*Z)(sum, )*sigma;
}

func gram_schmidt_orthonormalization(b, w)
/* DOCUMENT gram_schmidt_orthonormalization, b;
       -or- gram_schmidt_orthonormalization(b)
       -or- gram_schmidt_orthonormalization(b, w)
     Performs Gram-Schmidt orthonormalization of basis functions B.  If B
     is an array of pointers, then the input basis vectors are *B(i) for
     i=1,..,numberof(B); otherwise, the input basis vectors are B(..,i) for
     i=1,..,dimsof(B)(0).  When called as a subroutine, the operation is
     done "in-place". If the weighting array W is given, the
     orthornormalization is done for the scalar product defined as:
       <b1,b2> = sum(w*b1*b2).

   SEE ALSO: SVdec. */
{
  local b_i, b_j, w_b_i;

  /* NOTE: type conversion of array X, e.g. double(X), is a no-operation
     if X already of given type. */
  if (! is_array(b)) error, "expecting array parameter";
  type = double;
  if ((s = structof(b)) == pointer) {
    ptr = 1;
    n = numberof(b);
    if (! am_subroutine()) b = b; /* make a private copy */
    for (i=1 ; i<=n ; ++i) {
      if ((s = structof(*b(i))) == complex) type = complex;
      else if (s != double && s != float && s != long && s != int &&
               s != short && s != char) error, "bad data type";
    }
  } else {
    ptr = 0;
    n = dimsof(b)(0);
    if ((s = structof(b)) == complex) type = complex;
    else if (s != double && s != float && s != long && s != int &&
             s != short && s != char) error, "bad data type";
    if (s != type) {
      if (am_subroutine()) error, "bad data type for in-place conversion";
      b = type(b);
    }
  }
  if (type == complex) error, "complex data type not yet implemented";

  /* Gram-Schmidt Orthonormalization. */
  for (i=1 ; i<=n ; ++i) {
    /* get i-th basis vector */
    if (ptr) eq_nocopy, b_i, type(*b(i));
    else     b_i = b(.., i);

    /* make the i-th vector othogonal to previous ones */
    if (i > 1) {
      if (is_void(w)) eq_nocopy, w_b_i, b_i;
      else w_b_i = w*b_i;
      if (ptr) eq_nocopy, b_j, type(*b(1));
      else     b_j = b(.., 1);
      s = sum(w_b_i*b_j)*b_j;
      for (j=2 ; j<i ; ++j) {
        if (ptr) eq_nocopy, b_j, type(*b(j));
        else     b_j = b(.., j);
        s += sum(w_b_i*b_j)*b_j;
      }
      b_i -= s;
    }

    /* normalize i-th vector */
    s = (is_void(w) ? sum(b_i*b_i) : sum(w*b_i*b_i));
    if (ptr) {
      b(i) = &((s > 0.0 ? (1.0/sqrt(s))*b_i : array(double, dimsof(b_i))));
    } else {
      b(..,i) = (s > 0.0 ? (1.0/sqrt(s))*b_i : array(double, dimsof(b_i)));
    }
  }
  return b;
}

func trace(a)
/* DOCUMENT trace(a)
     Returns the trace of matrix A.

   SEE ALSO: diag. */
{
  if (! is_array(a) || (dims = dimsof(a))(1) != 2)
    error, "expecting a 2D array";
  m = dims(2);
  n = dims(3);
  return a(sum:1:m*min(m,n):m+1);
}

func diag(a)
/* DOCUMENT diag(a)
     Returns the diagonal of matrix A (if A is a 2D array) or a square
     diagonal matrix with diagonal elements equal to those of vector A (if
     A is a 1D array).

   SEE ALSO: trace, unit. */
{
  if (is_array(a)) {
    if ((dims = dimsof(a))(1) == 1) {
      m = dims(2);
      (mx = array(structof(a), m, m))(1:m*m:m+1) = a;
      return mx;
    } else if (dims(1) == 2) {
      m = dims(2);
      n = dims(3);
      return a(1:m*min(m,n):m+1);
    }
  }
  error, "expecting a 1D or 2D array";
}

func dot(x, y) { return sum(x*y); }
/* DOCUMENT dot(x, y)
    Returns the dot product x'.y = sum(x*y). */

local nrm2;
func euclidean_norm(x)
/* DOCUMENT euclidean_norm(x)
            nrm2(x)
    Returns the Euclidian norm of x: ||x||_2 = sqrt(sum(x*x)),
    taking care of overflows. */
{
  if (! (s = max(-min(x), max(x)))) return 0.0;
  x *= (1.0/s);
  return s*sqrt(sum(x*x));
}
nrm2 = euclidean_norm;

func pm(a, ndigits=, file=, format=)
/* DOCUMENT pm, a;
     Prints matrix A.  Keyword FILE can be set to the name/stream of output
     file; the default is to use standard output.  If keyword NDIGITS is
     set, floating-point values get printed with that number of significant
     digits; alternatively, keyword FORMAT may be set with the format for
     each element of A -- for complex numbers, the real and imaginary parts
     use the same format.

   SEE ALSO: write. */
{
  if (! is_array(a) || (dims = dimsof(a))(1) != 2) {
    error, "expecting a 2-dimensional array";
  }
  m = dims(2);
  n = dims(3);
  s = structof(a);
  if (s == complex) {
    if (is_void(format)) {
      if (is_void(ndigits)) ndigits = 5;
      format = swrite(format="%%.%dg+%%.%dgi", ndigits, ndigits);
    } else {
      format += "+" + format + "i";
    }
    a = swrite(format=format, double(a), a.im);
  } else if (s == pointer) {
    a = swrite(a);
  } else {
    if (is_void(format)) {
      if (s == double) {
        if (is_void(ndigits)) ndigits = 5;
        format = swrite(format="%%.%dg", ndigits);
      } else if (s == float) {
        if (is_void(ndigits)) ndigits = 3;
        format = swrite(format="%%.%dg", ndigits);
      } else if (s == long || s == int || s == short) {
        format = "%d";
      } else if (s == char) {
        format = "0x%02x";
      } else if (s == string) {
        /* should escape quotes in input strings */
        format = "\"%s\"";
      } else {
        error, "bad data type";
      }
    }
    a = swrite(format=format, a);
  }
  if (structof(file) == string) file = open(file, "w");
  cols = swrite(format="(,%d)",indgen(n));
  rows = swrite(format="(%d,)",indgen(m));
  fmt0 = swrite(format="%%%ds", max(strlen(rows)));
  fmt1 = swrite(format=" %%-%ds", max(max(strlen(cols)), max(strlen(a))));
  fmt2 = " %s\n";
  write, file, format=fmt0, "";
  for (j=1 ; j<n ; ++j) write, file, format=fmt1, cols(j);
  write, file, format=fmt2, cols(n);
  for (i=1 ; i<=m ; ++i) {
    write, file, format=fmt0, rows(i);
    for (j=1 ; j<n ; ++j) write, file, format=fmt1, a(i,j);
    write, file, format=fmt2, a(i,n);
  }
}

func interpolation_matrix(m, n)
{
  u = array(1.0, m);
  ut = array(1.0, 1, m);
  v = array(1.0, n);
  return (1.0/n)*(kronecker_product(diag(v), ut)(,+)*
                  kronecker_product(diag(u), v)(+,));
}

func kronecker_product(a,b)
{
  if (! is_array(a) || (a_rank = (a_dims = dimsof(a))(1)) > 2 ||
      ! is_array(b) || (b_rank = (b_dims = dimsof(b))(1)) > 2) {
    error, "A and B must be at most 2-D arrays";
  }
  a_nrows = (a_rank >= 1 ? a_dims(2) : 1);
  a_ncols = (a_rank >= 2 ? a_dims(3) : 1);
  b_nrows = (b_rank >= 1 ? b_dims(2) : 1);
  b_ncols = (b_rank >= 2 ? b_dims(3) : 1);
  c_nrows = a_nrows*b_nrows;
  c_ncols = a_ncols*b_ncols;
  type = structof(a(1)+b(1));
  if (type != double && type != complex) {
    type = double;
  }
  c = array(type, c_nrows, c_ncols);
  for (j = 1; j <= a_ncols; ++j) {
    l = (j - 1)*b_ncols;
    for (i = 1; i <= a_nrows; ++i) {
      k = (i - 1)*b_nrows;
      c(k+1:k+b_nrows, l+1:l+b_ncols) = a(i,j)*b;
    }
  }
  return c;
}

/*---------------------------------------------------------------------------*/
/* Linear fit */

func linreg(x,y,w)
/* DOCUMENT [a,b] = linreg(x,y);
         or [a,b] = linreg(x,y,w);

     Performs linear regression.  Returns a and b such that a + b*x is the
     closest straight line to the given points (x, y), i.e., such that the
     squared error between y and a + b*x is minimized.

     If weights w are provided, the result of weighted least-squares linear
     regression is returned.

   SEE ALSO:
 */
{
  dims = dimsof(x, y);
  if (is_void(w)) {
    a1 = avg(x*x);
    a2 = avg(x);
    a3 = 1.0;
    b1 = avg(x*y);
    b2 = avg(y);
  } else {
    if (min(w) < 0) error, "bad weights";
    wx = w*x;
    a1 = avg(wx*x);
    a2 = avg(wx);
    a3 = avg(w);
    b1 = avg(wx*y);
    b2 = avg(w*y);
  }
  a = max(abs(a1), abs(a2), abs(a3));
  if (a > 0.0) {
    r = 1.0/a;
    a1 *= r;
    a2 *= r;
    a3 *= r;
    d = a1*a3 - a2*a2;
    if (d > 0.0) {
      r /= d;
      b1 *= r;
      b2 *= r;
      return [a3*b1 - a2*b2, a1*b2 - a2*b1];
    }
  }
  error, "singular system";
}

/*---------------------------------------------------------------------------*/
/* Faddeev-Leverrier method */

/*
 * Faddeev-Leverrier Algorithm:
 *
 *   alpha_{n} = 1
 *   for k = 1, n; do
 *     if k = 1 then
 *       D_{1} = I  (identity)
 *       B_{1} = A
 *     else
 *       D_{k} = B_{k-1} - p_{k}*I
 *       B_{k} = A.D_{k}
 *     end
 *     p_{k} = trace(B_{k})/k
 *     alpha_{n - k} = -p_{k}
 *   end
 *
 * Roots of polynomial: alpha_0 + alpha_1*lambda + ... + alpha_n*lambda^n
 * are the eigenvalues of A.
 *
 * The matrix: D_{n}/p_{n} is the inverse of A.
 *
 * Any column of the matrix: sum_{k=1}^{n} lambda^(n-k) D_{k} is the eigenvector
 * corresponding to eigenvalue lambda.
 *
 * Note: If ker(A) has dimension n - r  (r is the rank of A and A is n*n),
 *       then the n - r first corfficients alpha are equal to zero.  Since
 *       they are computed the first...
 */
func faddeev_leverrier(a, full=, transp=)
/* DOCUMENT [&c, &ainv] = faddeev_leverrier(a)

     Implement Faddeev-Leverrier method for eigenvalues/eigenvector
     decomposition of a matrix.

     Vector C contains the coefficients of the polynomial of which the
     roots are the eigenvalues of A.

     Matrix AINV is the inverse of A.

     Slightly faster for large matrices with TRANSP=1.

   SEE ALSO: SVdec
*/
{
  if (((s = structof(a)) != double && s != float && s != complex &&
       s != char && s != short && s != int) ||
      (dims = dimsof(a))(1) != 2 || (n = dims(2)) != dims(3)) {
    error, "expecting a N × N numerical array";
  }
  if (s != double && s != complex) {
    a = double(a);
    s = double;
  }
  trace = sum : 1 : n*n : n + 1;
  ident = array(s, n, n);
  ident(1 : n*n : n + 1) = 1;
  c = array(s, n + 1);
  c(n + 1) = 1;
  b = a;
  if (transp) {
    ap = transpose(a);
    for (k = 1; ; ++k) {
      p = b(trace)/k;
      c(n + 1 - k) = -p;
      if (k >= n) {
        break;
      }
      d = b - p*ident;
      b = ap(+,)*d(+,);
    }
  } else {
    for (k = 1; ; ++k) {
      p = b(trace)/k;
      c(n + 1 - k) = -p;
      if (k >= n) {
        break;
      }
      d = b - p*ident;
      b = a(,+)*d(+,);
    }
  }
  ainv = (1.0/p)*d;
  if (! full) {
    return [&c, &ainv];
  }


  /* Z(,i) is i-th eigen vector */

  if (! is_func(zroots)) include, "zroots.i", 1;
  l = zroots(c);
  //z = array(s, n, n); // FIXME:
  z = (l^(n - 1))*ident(,avg)(-,);
  b = a;
  for (k = 1; ; ++k) {
    p = b(trace)/k;
    if (k >= n) {
      z += d(,avg)(-,);
      break;
    }
    z += (l^(n - k))*d(,avg)(-,);
    d = b - p*ident;
    b = a(,+)*d(+,);
  }
  return [&c, &ainv, &l, &z];
}

/*---------------------------------------------------------------------------*/
/* Cholesky Decomposition */

func cholesky(a, raw)
/* DOCUMENT cholesky(a)
       -or- cholesky(a, 0/1)

     Given  a  symmetric  positive  definite  matrix  A,  returns  a  lower
     triangular  matrix C  which is  the Cholesky  decomposition of  A such
     that:

        A = C(,+)*C(,+);        (i.e., A is equal to C times C transpose)

     If  optional  second argument  is  true  (non-nil  and non-zero),  the
     scratch values in  the upper triangular part of  C are left unchanged;
     otherwise (the  default behavior), the  upper triangular part of  C is
     filled with zeros.
*/
{
  if (! is_array(a) || structof(a) == complex ||
      (dims = dimsof(a))(1) != 2 || (n = dims(2)) != dims(3))
    error, "expecting a N × N non-complex array";
  a = double(a);

  if ((s = a(1,1)) <= 0.0) error, "the matrix is not positive definite";
  a(1,1) = sqrt(s);
  for (j=2 ; j<=n ; ++j) {
    a(1,j) = (t = a(1,j)/a(1,1));
    s = t*t;
    for (k=2 ; k<j ; ++k) {
      rng = 1:k-1;
      a(k,j) = (t = (a(k,j) - sum(a(rng,k)*a(rng,j)))/a(k,k));
      s += t*t;
    }
    s = a(j,j) - s;
    if (s <= 0.0) error, "the matrix is not positive definite";
    a(j,j) = sqrt(s);
  }
#if 0 /* slower code (in Yorick) but less obscure */
  for (j=1 ; j<=n ; ++j) {
    s = 0.0;
    for (k=1 ; k<j ; ++k) {
      if (k == 1) {
        t = a(1,j) / a(1,1);
      } else {
        rng = 1:k-1;
        t = (a(k,j) - sum(a(rng,k)*a(rng,j))) / a(k,k);
      }
      a(k,j) = t;
      s += t*t;
    }
    s = a(j,j) - s;
    if (s <= 0.0) error, "the matrix is not positive definite";
    a(j,j) = sqrt(s);
  }
#endif
  if (! raw) {
    for (k=1 ; k<n ; ++k) a(k+1, 1:k)=0;
  }
  return transpose(a);
}

/*---------------------------------------------------------------------------*/
/* Singular Value Decomposition */

local sv_intro;
/* DOCUMENT sv_intro - introduction to SVD Yorick package

   Notes about matrix multiplication in Yorick:

     A.B  = A(,+)*B(+,)
     A'.B = A(+,)*B(+,)  // transpose of A times B
     A.B' = A(,+)*B(,+)  // A times transpose of B

     diag(s).A  = diag(s)(,+)*A(+,)
                = s*A = A*s

     diag(s).A' = diag(s)(,+)*A(,+)
                = transpose(s(-,)*A) = transpose(A*s(-,))

     A.diag(s)  = A(,+)*diag(s)(+,)
                = A*s(-,) = s(-,)*A

     A'.diag(s) = A(+,)*diag(s)(+,) = A(+,)*diag(s)(,+)
                = transpose(A*s) = transpose(s*A)

   Singular Value Decomposition:

     A = U.diag(SIGMA).V' = U.diag(SIGMA).VT
       = (U*SIGMA(-,))(,+)*VT(+,)
       = U(,+)*(SIGMA*VT)(+,)

   where:

     SIGMA = SVdec(A, U, VT)

   Columns of U and V form an orthonormal basis (i.e.  U and V are
   column-orthonormal):

     U'.U = V'.V = I

   in Yorick notation:

     U(+,)*U(+,) = V(+,)*V(+,) = VT(,+)*VT(,+) = unit(N)

   Note (to be verified): if U and/or V are square, they are also
   row-orthonormal:

     U'.U = U.U' = I   (if U is square)
     V'.V = V.V' = I   (if V is square)

   Generalized-inverse of A:

     AP = V.diag(1/SIGMA).U' = VT'.diag(1/SIGMA).U'
       = VT(+,)*((1.0/SIGMA)(-,)*U)(,+)
       = ((1.0/SIGMA)*VT)(+,)*U(,+)

   Solving a linear problem: A.x ~ b with SVD (taking care of index ordering
   for faster matrix multiplication):

     X = V.diag(W).U'.B
       = (W*VT)(+,)*U(,+))(,+)*B(+,..)
       = (W*VT)(+,)*(U(+,)*B(+,..))(+,..)  // sum over 1st indices: faster

   where W is an approximation of 1/SIGMA.

   SEE ALSO: sv_dcmp, sv_solve_trunc, sv_solve_wiener, SVdec, SVsolve.
 */

func sv_dcmp(a, full)
/* DOCUMENT sv_dcmp(a)
       -or- sv_dcmp(a, full)
     Computes the singular value decomposition of matrix A.  The result
     is an array of pointers:

         [&SIGMA, &U, &VT]

     where SIGMA is the vector of  singular values of A, and the columns of
     U  and  the  rows of  VT  are  the  left  and right  singular  vectors
     (respectively).  Using matrix notation:

         A = U.diag(SIGMA).V' = U.diag(SIGMA).VT

     and using Yorick notation:

         A = (U*SIGMA(-,))(,+)*VT(+,)
           = U(,+)*(SIGMA*VT)(+,)

     FULL has the same meaning as keyword FULL in SVdec (to see).

   SEE ALSO: sv_dcmp, sv_solve_trunc, sv_solve_wiener, SVdec. */
{
  local u, vt;
  sigma = SVdec(a, u, vt, full=full);
  return [&sigma, &u, &vt];
}

func _sv_get_dcmp(a, full)
/* DOCUMENT local sigma, u, vt; _sv_get_dcmp(a);
       -or- local sigma, u, vt; _sv_get_dcmp(a, full)
     Private routine used to extract Singular Value Decomposition of A into
     external symbols: SIGMA, U and VT.

   SEE ALSO: sv_dcmp, sv_solve_trunc, sv_solve_wiener. */
{
  extern sigma, u, vt;
  if (structof(a) == pointer) {
    eq_nocopy, sigma, *a(1);
    eq_nocopy, u,     *a(2);
    eq_nocopy, vt,    *a(3);
  } else {
    sigma = SVdec(a, u, vt, full=full);
  }
}

func sv_solve_trunc(a, b, eps, full=)
/* DOCUMENT sv_solve_trunc(a, b, eps)
     Solve linear problem A.x = b by truncated singular value method.  A is
     either a matrix or the  singular value value decomposition as returned
     by sv_dcmp  (to see).  B  is the right  hand side vector (or  array to
     solve for  several right  hand sides  at a time).   EPS (in  the range
     [0,1])  is the  minimum  relative  singular value  to  keep, i.e.  all
     singular values  less than EPS*max(SIGMA) get discarded  (SIGMA is the
     vector of singular values of A).  The result is:

         (W*VT)(+,)*(U(+,)*B(+,..))(+,..)

     where  SIGMA,  U and  VT  are the  components  of  the singular  value
     decomposition of A and W is:

         W(i) = 1/SIGMA(i)   if SIGMA(i) > EPS*max(SIGMA);
                0            otherwise.

   SEE ALSO: sv_dcmp, sv_intro, sv_solve_wiener. */
{
  local sigma, u, vt;
  _sv_get_dcmp, a, full;
  if (is_void(eps)) eps = 1e-1;
  w = double(sigma > eps*sigma(1))/(sigma + !sigma);
  return (w*vt)(+,)*(u(+,)*b(+,..))(+,..);
}

func sv_solve_wiener(a, b, eps, full=)
/* DOCUMENT sv_solve_wiener(a, b, eps)
     Solve  linear problem  A.x =  b by  Wiener filtering  of  the singular
     values.   A   is  either  a   matrix  or  the  singular   value  value
     decomposition as  returned by sv_dcmp (to  see).  B is  the right hand
     side  vector (or  array to  solve for  several right  hand sides  at a
     time).  EPS (in the range [0,1]) is the relative singular value filter
     level.  The result is:

         (W*VT)(+,)*(U(+,)*B(+,..))(+,..)

     where  SIGMA,  U and  VT  are the  components  of  the singular  value
     decomposition of A and W is:

         W = SIGMA/(SIGMA^2 + (EPS*max(SIGMA))^2)

   SEE ALSO: sv_dcmp, sv_intro, sv_solve_trunc. */
{
  local sigma, u, vt;
  _sv_get_dcmp, a, full;
  if (is_void(eps)) eps = 1e-1;
  w = sigma/(sigma*sigma + (eps*sigma(1))^2);
  return (w*vt)(+,)*(u(+,)*b(+,..))(+,..);
}

func pseudo_inverse(a, rcond=, lsq=, debug=)
/* DOCUMENT pseudo_inverse(a)

     This function computes the pseudo-inverse of the matrix A.  Argument A
     must be a M-by-N real or complex array.  Singular value decomposition is
     used to handle numerically singular cases.  If A is square and
     non-singular, LUsolve(A) is faster than computing the pseudo-inverse
     of A.

     If keyword LSQ (for "Least SQuares") is true, the result is the
     pseudo-inverse of A'.A (the matrix in the left hand side of the normal
     equations).

     Keyword RCOND can be set with the allowed condition number; RCOND =
     1E-9, by default.


   SEE ALSO: SVdec, LUsolve.
 */
{
  if (is_void(rcond)) rcond = 1E-9;
  dims = dimsof(a);
  m = dims(2);
  n = dims(3);
  cmplx = is_complex(a);

  /* Perform the Singular Value decomposition of m-by-n matrix A:
     A = U1.S1.V1' where U1 is m-by-r, S1 is non-singular r-by-r diagonal
     and V1 is n-by-r with r = rank(A). */
  local u, vt;
  if (cmplx) {
    /* Complex matrix A is the same as the real matrix
       [[Re(A),-Im(A)],[Im(A),Re(A)]] */
    temp = array(double, 2*m, 2*n);
    r1 = 1:-1:2;
    r2 = 2: 0:2;
    temp(r1, r1) = temp(r2, r2) = double(a);
    temp(r1, r2) = -(temp(r2, r1) = a.im);
    eq_nocopy, a, temp;
  }
  s = SVdec(a, u, vt);
  a = [];
  if (lsq) u = [];
  j = where(s > rcond*max(s));
  if (numberof(j) != numberof(s)) {
    /* Keep only non-zero singular values. */
    if (! is_array(j)) error, "matrix is numerically equal to zero";
    s = s(j);
    if (! lsq) u = u(,j);
    vt = vt(j,);
  }

  /* Compute pseudo inverse. */
  z = (1.0/s)*unref(vt);
  if (lsq) {
    /* Pseudo inverse of A'.A = V1.S1^{-2}.V1'. */
    z = z(+,)*z(+,);
  } else {
    z = z(+,)*u(,+);
  }
  if (cmplx) {
    z11 = z(r1,r1);
    z12 = z(r1,r2);
    z21 = z(r2,r1);
    z22 = z(r2,r2);
    if (debug) {
      write, "Real part of matrix:";
      pm,  z11, format="%12.3e";
      pm,  z22, format="%12.3e";
      write, "Imaginary part of matrix:";
      pm, -z12, format="%12.3e";
      pm,  z21, format="%12.3e";
    }
    /* FIXME: Improve numerical precision by taking the average -- normally
       the real part of the result is Z11 = Z22, and the imaginary part
       is Z21 = -Z12. */
    return 0.5*(z11 + z22) + 1i*(0.5*(z21 - z12));
  }
  return z;
}

/*---------------------------------------------------------------------------*/
/* UTILITIES */

func make_matrix(op, x, extra)
/* DOCUMENT make_matrix(op, x)
 *     -or- make_matrix(op, x, extra)
 *
 *   Build real matrix cooresponding to a linear operator OP which is
 *   a function like object which takes 1 or 2 arguments: Y = OP(X) or
 *   Y = OP(X, EXTRA) where X gives the data type and dimension list
 *   of the input space for OP and where EXTRA is any additionnal
 *   parameters required by OP (the type and contents of EXTRA has no
 *   incidence on make_matrix, it is just passed as the second
 *   argument of OP).  The result is a M-by-N real matrix with
 *   M = numberof(Y) and N = numberof(X).
 *
 * SEE ALSO: mvmult.
 */
{
  if (structof(x) == complex) {
    error, "complex input vector not supported";
  }
  x = array(double, dimsof(x));
  n = numberof(x);
  simple = is_void(extra);
  for (j = 1; j <= n; ++j) {
    x(j) = 1.0;
    y = (simple ? op(x) : op(x, extra));
    x(j) = 0.0;
    if (j == 1) {
      if (structof(y) == complex) {
        error, "complex input vector not supported";
      }
      m = numberof(y);
      a = array(double, m, n);
    }
    a(,j) = y(*);
  }
  return a;
}
