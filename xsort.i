/*
 * xsort.i --
 *
 * Extends Yorick sort methods.
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

func xsort_uniq(x, ..)
/* DOCUMENT index = xsort_uniq(x1, x2, ...);

     Returns a list of Yorick indices such that `x1(index)`, `x2(index)`, etc.,
     are unique N-uplets and sorted in ascending order (in the same sense as
     `msort` or `xsort`).

     The arguments may be numbers or strings (e.g., `x1` could be an integer
     while `x2` was a string, and `x3` was a real).  The `xi` must all be
     conformable, and each dimension of `x1` must be as large as the
     corresponding dimension of any other argument.  If an argument, say `x2`,
     does not have the same dimensions as `x1`, they have to be expanded
     accordingly before using the index on them, say `(x2 + array(structof(x2),
     dimsof(x1)))(index)`.

   SEE ALSO: msort, xsort.
 */
{
  local index, uniq; /* shared variables with _xsort_classify */
  classify = _xsort_classify; /* shortcut */

  /* Process first argument. */
  rank = classify(x);
  max_rank = (number = numberof(x)) - 1;
  if (max(rank) != max_rank) {
    /* Process remaining arguments. */
    norm = 1.0/(max_rank + 1.0);
    if (1.0 + norm == 1.0) {
      error, swrite(format="%d is too large an array", number);
    }
    n = more_args();
    while (n--) {
      rank += classify(next_arg())*norm;    /* adjust rank for next key */
      rank = classify(rank);                /* renormalize adjusted rank */
      if (numberof(rank) != number) {
        error, "X1 must be the largest in every dimensions";
      }
      if (max(rank) == max_rank) {
        break;
      }
    }
  }
  return index(where(grow(1n, uniq)));
}

func xsort(x, .., all=)
/* DOCUMENT index = xsort(x1, x2, x3, ...);

     Returns an index list which sorts the array `x1` into increasing order.
     Where `x1` values are equal, the list will sort `x2` into increasing
     order.  Where both `x1` and `x2` are equal, `x3` will be in increasing
     order, and so on.  Finally, where all of the keys are equal, the returned
     list will leave the order unchanged from the input keys.

     The arguments may be numbers or strings (e.g., `x1` could be an integer
     while `x2` was a string, and `x3` was a real).  The arguments must all be
     conformable, and each dimension of `x1` must be as large as the
     corresponding dimension of any other argument.

     Hence, `xsort(x)` will return the same list as `sort(x)` or `heapsort(x)`,
     except where the values of `x` are equal, in which case `xsort` leaves the
     order unchanged, while `sort` non-deterministically permutes equal
     elements.  This feature may cost a factor of two in speed, so don't use it
     unless you really need it.  In general, `xsort` will call `heapsort` up to
     twice per input argument.

     If keyword `all` is true, then `[&index,&rank,&uniq]` is returned where
     `rank` is the "rank" of `(x1,x2,...)` as computed by `xsort_rank` and
     `uniq` is a boolean array set true where two consecutive sorted elements
     are different.

   SEE ALSO: sort, heapsort, msort, xsort_rank.
 */
{
  local index, uniq; /* shared variables with _xsort_classify */
  classify = _xsort_classify; /* shortcut */

  /* Process first argument. */
  rank = classify(x);
  max_rank = (number = numberof(x)) - 1;
  if (max(rank) == max_rank) {
    return (all ? [&index, &rank, &uniq] : index);
  }
  norm = 1.0/(max_rank + 1.0);
  if (1.0 + norm == 1.0) {
    error, swrite(format="%d is too large an array", number);
  }

  /* Process remaining arguments. */
  n = more_args();
  while (n--) {
    rank += classify(next_arg())*norm;    /* adjust rank for next key */
    rank = classify(rank);                /* renormalize adjusted rank */
    if (numberof(rank) != number) {
      error, "first argument must be the largest in every dimensions";
    }
    if (max(rank) == max_rank) {
      return (all ? [&index, &rank, &uniq] : index);
    }
  }

  /* Use indgen as final key guaranteed to break up any remaining equal
     values. */
  rank += indgen(0:max_rank)*norm;
  if (all) {
    rank = classify(rank);
    return [&index, &rank, &uniq];
  } else {
    return heapsort(rank);
  }
}

func xsort_rank(x, &index, &uniq)
/* DOCUMENT rank = xsort_rank(x);
         or rank = xsort_rank(x, index);
         or rank = xsort_rank(x, index, uniq);

     Returns a list of longs the same size and shape as `x`, whose values are
     the "rank" of the corresponding element of `x` among all the elements of
     `x` -- the smallest element has rank 0 and the largest has the largest
     rank, which is equal to one less than the number of distinct values in the
     array `x`.

     If `index` is present, it is set to the order list returned by
     `sort(x(*))` if `x` is an array of strings and by `heapsort(x)` otherwise.

     If `uniq` is present, it is a boolean array of lenght `numberof(x) - 1`,
     set true where two consecutive sorted elements are different.

     This function behaves like `msort_rank` but has slight improvements (like
     avoiding copying strings three times and using `heapsort` for numbers).

   SEE ALSO: msort, sort, heapsort, xsort.
 */
{
  return _xsort_classify(x);
}

func _xsort_classify(x)
{
  extern index, uniq; /* shared variables with _xsort_classify */
  rank = array(long, dimsof(x));
  if (numberof(x) < 2) {
    index = 1 + rank;
    uniq = [];
  } else {
    void = use_origins(0); /* cancel any politics for origins of arrays */
    if (is_string(x)) {
      /* For strings, use sort (not heapsort) and avoid copying string 3 times
         (only twice). */
      index = sort(x(*));
      uniq = (x(index(1:-1)) != x(index(2:0)));
    } else {
      index = heapsort(x);
      x = x(index);
      uniq = (x(1:-1) != x(2:0));
    }
    rank(index) = uniq(cum);
  }
  return rank;
}
