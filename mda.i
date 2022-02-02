/*
 * mda.i --
 *
 * Yorick support for MDA (Multi-Dimensional Arrays) file format.
 *
 * ---------------------------------------------------------------------------
 *
 * This file is part of YLib available at <https://github.com/emmt/ylib> and
 * licensed under the MIT "Expat" License.
 *
 * Copyright (C) 2000-2014, Éric Thiébaut.
 *
 * ----------------------------------------------------------------------------
 */

func mda_save(data, file, &offset, byteorder=)
/* DOCUMENT mda_save, data, file;
         or mda_save, data, file, offset;
     This subroutine saves array DATA into FILE using MDA (Multi-Dimensional
     Arrays) format.  FILE must be the name of a file or a binary stream open
     for writing.  If optional argument OFFSET is provided, the writing starts
     at the given byte offset (0 if OFFSET is unset) and the variable offset
     is updated on output.  Thus the caller can keep track of the offset when
     writing several arrays in the same file.

     Keyword BYTEORDER can be set with "little_endian" or "big_endian" to
     specify the byte order to use (the defaut is little endian).

   SEE ALSO: mda_recv, set_primitives.
 */
{
  type = structof(data);
  cmplx = is_complex(data);
  if (type == char) {
    type = ('\xFF' < '\x00' ? 1 : 2);
    elsize = 1;
  } else if (type == short) {
    type = 3;
    elsize = 2;
  } else if (type == int) {
    type = 5;
    elsize = 4;
  } else if (type == long) {
    type = 7;
    elsize = 8;
  } else if (type == float) {
    type = 9;
    elsize = 4;
  } else if (type == double) {
    type = 10;
    elsize = 8;
  } else if (type == complex) {
    type = 12;
    elsize = 16;
  } else {
    error, "unsupported data type";
  }
  if (is_string(file) && is_scalar(file)) {
    file = open(file, "wb");
  } else if (typeof(file) != "stream") {
    error, "FILE must be a file name or a binary stream handle";
  }
  if (is_void(offset)) offset = 0;
  if (is_void(byteorder) || byteorder == "little_endian") {
    encoding = _MDA_LITTLE_ENDIAN;
  } else if (byteorder == "big_endian") {
    encoding = _MDA_BIG_ENDIAN;
  } else {
    error, "bad value for keyword BYTEORDER";
  }
  dims = dimsof(data);
  ndims = dims(1);
  header = int(dims);
  header(1) = 0x4D444100 | (type << 4) | ndims;
  set_primitives, file, encoding;
  if (cmplx) {
    /* make stream aware of the definition of a complex */
    save, file, complex;
  }
  _write, file, offset, header;
  offset +=  4*(ndims + 1);
  _write, file, offset, data;
  offset += elsize*numberof(data);
}

func mda_recv(file, &offset, justcheck=)
/* DOCUMENT mda_recv(file);
         or mda_recv(file, offset);
     This subroutine recovers an array from FILE using MDA (Multi-Dimensional
     Arrays) format.  FILE must be the name of a file or a binary stream open
     for writing.  If optional argument OFFSET is provided, the reading starts
     at the given byte offset (0 if OFFSET is unset) and the offset variable
     is updated on output.  Thus the caller can keep track of the offset when
     reading several arrays in the same file.

   SEE ALSO: mda_save, set_primitives.
 */
{
  if (is_string(file) && is_scalar(file)) {
    file = open(file, "rb");
  } else if (typeof(file) != "stream") {
    error, "FILE must be a file name or a binary stream handle";
  }
  /* Load the first 4 bytes, to figure out the format and the byte order. */
  if (is_void(offset)) offset = 0;
  data = array(char, 4);
  if (_read(file, offset, data) != sizeof(data)) {
    error, "short MDA file/stream";
  }
  if (data(1) == 0x4D && data(2) == 0x44 && data(3) == 0x41) {
    /* Input is big endian. */
    encoding = _MDA_BIG_ENDIAN;
    type_and_ndims = (data(4) & 0xFF);
  } else if (data(4) == 0x4D && data(3) == 0x44 && data(2) == 0x41) {
    /* Input is little endian. */
    encoding = _MDA_LITTLE_ENDIAN;
    type_and_ndims = (data(1) & 0xFF);
  } else {
    error, "invalid MDA identifier";
  }
  type = ((type_and_ndims >> 4) & 0xF);
  ndims = (type_and_ndims & 0xF);
  if (type == 1 || type == 2) {
    type = char;
    elsize = 1;
    cmplx = 0n;
  } else if (type == 3 || type == 4) {
    type = short;
    elsize = 2;
    cmplx = 0n;
  } else if (type == 5 || type == 6) {
    type = int;
    elsize = 4;
    cmplx = 0n;
  } else if (type == 7 || type == 8) {
    type = long;
    elsize = 8;
    cmplx = 0n;
  } else if (type == 9 || type == 11) {
    cmplx = (type == 11);
    type = float;
    elsize = 4;
  } else if (type == 10 || type == 12) {
    cmplx = (type == 12);
    type = double;
    elsize = 8;
  } else {
    error, "invalid MDA type identifier";
  }
  set_primitives, file, encoding;
  if (ndims == 0) {
    dims = (cmplx ? [1,2] : [0]);
  } else {
    dims = array(int, ndims);
    _read, file, offset + 4, dims;
    if (cmplx) {
      dims = grow(ndims+1, 2, dims);
    } else {
      dims = grow(ndims, dims);
    }
  }
  if (justcheck) {
    if (am_subroutine()) {
      write, format = "byte order = \"%s\", type = %s, dimensions =",
        (encoding(3) < 0 ? "little" : "big"),
        nameof(type);
      write, dims;
    } else {
      return [&type, &dims, &encoding];
    }
  }
  offset +=  4*(ndims + 1);
  data = array(type, dims);
  _read, file, offset, data;
  offset += elsize*numberof(data);
  if (cmplx) {
    local z;
    dims = dims(2:);
    dims(1) = ndims;
    reshape, z, &double(data), complex, dims;
    return z;
  }
  return data;
}

/* Primitive types for big and little endian byte order.  Apart for the byte
   ordering and the 64-bit integer for the "long" type, these formats closely
   follows IEEE standard and XDR (eXternal Data Representation). */
/*                               size   alignment   byte order */
_MDA_BIG_ENDIAN = [/*   char */   1,         1,         1,
                   /*  short */   2,         2,         1,
                   /*    int */   4,         4,         1,
                   /*   long */   8,         8,         1,
                   /*  float */   4,         4,         1,
                   /* double */   8,         8,         1,
                   /*           sign  exp   exp   man   man   man   exp
                    *           addr  addr  len   addr  len   norm  bias
                    *  float */   0,    1,    8,    9,   23,    0,  0x7f,
                   /* double */   0,    1,   11,   12,   52,    0,  0x3ff];
_MDA_LITTLE_ENDIAN = _MDA_BIG_ENDIAN;
_MDA_LITTLE_ENDIAN(3:18:3) = -1;
