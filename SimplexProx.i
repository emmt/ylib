/*
 * SimplexProx.i --
 *
 * This file is part of YLib (Yorick Library) which is licensed under the MIT
 * "Expat" License:
 *
 * Copyright (C) 2014, Ferréol Soulez.
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

func Simplex_prox(x)
/* DOCUMENT p = Simplex_prox(x);

   Compute the projection on the simplex such that:
   p = arg min_p || x - p ||^2_2  s.t.  x \ge 0 and  sum_k x_k = 1

   SEE ALSO:
 */
{
  y = x(*);
  heapsort,y;
  N = numberof(y);
  n = N+1;
  cs = 0;
  ind=0;
  do{
    mu_prev=mu;
    n--;
    ind++;
    cs += y(n);
    mu = (1. - cs)/ind;

  } while(((y(n)+mu)>0)&(n!=0));
    return max(x + mu_prev,0);

}
