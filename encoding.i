/*
 * encoding.i --
 *
 * Deal with character encodings.
 *
 * ---------------------------------------------------------------------------
 *
 * This file is part of YLib available at <https://github.com/emmt/ylib> and
 * licensed under the MIT "Expat" License.
 *
 * Copyright (C) 2009-2017, Éric Thiébaut.
 *
 * ---------------------------------------------------------------------------
 */

/*---------------------------------------------------------------------------*/
/* LATIN-1 (ISO 8859-1) */

/*
 * Notes:
 *   - "wide" characters are 32-bit integers which can store all possible
 *     character codes.
 *
 *   - More compact representations are possible which store the characters as
 *     bytes (possibly multiple bytes for a single character) and assuming
 *     various encoding (ASCII, UTF-8 or Latin-1).  These compact
 *     representations can be stored in Yorick strings or array of char.
 *
 *   - For any representation, the value zero represent an end of string.
 *
 *   - Latin-1 (ISO 8859-1) characters have values in the range [0,255]
 *     whatever the representation.  Strings or arrays of char are treated as
 *     arrays of unsigned 8-bit integers and "wide" character representation is
 *     just a conversion of the unsigned 8-bit integers into 3-bit integers.
 */

func decode_latin1(text)
/* DOCUMENT decode_latin1(text);

     Convert TEXT into wide characters assuming TEXT is encoded in Latin-1 (ISO
     8859-1).  Input TEXT can be an array of strings or an array of chars, the
     output is a vector of 32-bit integers.

     This operation simply performs type conversion taking care of returning
     values in the range [0,255].

   SEE ALSO: decode_utf8, encode_utf8, encode_ascii.
*/
{
  if ((type = structof(text)) == string) {
    chars = int(strchar(text));
  } else if (type == char) {
    chars = int(text(*));
  } else {
    error, "bad data type";
  }
  return (min(chars) >= 0 ? chars : (chars & 0xFFn));
}
errs2caller, decode_latin1;

func encode_latin1(chars, text=)
/* DOCUMENT encode_latin1(chars);

     Convert wide characters CHARS into bytes using Latin-1 (ISO 8859-1)
     encoding.  Input CHARS must be an array of 32-bit integers.  If keyword
     TEXT is true, an array of strings is returned; otherwise, an array of
     bytes is returned.

   SEE ALSO: decode_latin1, encode_utf8, encode_ascii.
*/
{
  if (structof(chars) != int) {
    error, "expecting 32-bit integer(s)";
  }
  if (min(chars) < 0n || max(chars) > 255n) {
    error, "invalid Latin-1 character(s)";
  }
  bytes = char(chars);
  return (text ? strchar(bytes) : bytes);
}
errs2caller, encode_latin1;

/* Constants for Latin1 (ISO 8859-1) characters. */
C_NO_BREAK_SPACE            = 0x00A0n;
C_INVERTED_EXCLAMATION_MARK = 0x00A1n;
C_CENT_SIGN                 = 0x00A2n;
C_POUND_SIGN                = 0x00A3n;
C_CURRENCY_SIGN             = 0x00A4n;
C_YEN_SIGN                  = 0x00A5n;
C_BROKEN_BAR                = 0x00A6n;
C_SECTION_SIGN              = 0x00A7n;
C_DIAERESIS                 = 0x00A8n;
C_COPYRIGHT_SIGN            = 0x00A9n;
C_FEMININE_ORDINAL          = 0x00AAn;
C_LEFT_GUILLEMET            = 0x00ABn;
C_NOT_SIGN                  = 0x00ACn;
C_SOFT_HYPHEN               = 0x00ADn;
C_REGISTERED_SIGN           = 0x00AEn;
C_MACRON                    = 0x00AFn;
C_DEGREE_SIGN               = 0x00B0n;
C_PLUS_MINUS_SIGN           = 0x00B1n;
C_SUPERSCRIPT_TWO           = 0x00B2n;
C_SUPERSCRIPT_THREE         = 0x00B3n;
C_ACUTE_ACCENT              = 0x00B4n;
C_MICRO_SIGN                = 0x00B5n;
C_PILCROW_SIGN              = 0x00B6n;
C_MIDDLE_DOT                = 0x00B7n;
C_CEDILLA                   = 0x00B8n;
C_SUPERSCRIPT_ONE           = 0x00B9n;
C_MASCULINE_ORDINAL         = 0x00BAn;
C_RIGHT_GUILLEMET           = 0x00BBn;
C_ONE_QUARTER               = 0x00BCn;
C_ONE_HALF                  = 0x00BDn;
C_THREE_QUARTERS            = 0x00BEn;
C_INVERTED_QUESTION_MARK    = 0x00BFn;
C_CAPITAL_A_GRAVE           = 0x00C0n;
C_CAPITAL_A_ACUTE           = 0x00C1n;
C_CAPITAL_A_CIRCUMFLEX      = 0x00C2n;
C_CAPITAL_A_TILDE           = 0x00C3n;
C_CAPITAL_A_DIAERESIS       = 0x00C4n;
C_CAPITAL_A_RING            = 0x00C5n;
C_CAPITAL_AE                = 0x00C6n;
C_CAPITAL_C_CEDILLA         = 0x00C7n;
C_CAPITAL_E_GRAVE           = 0x00C8n;
C_CAPITAL_E_ACUTE           = 0x00C9n;
C_CAPITAL_E_CIRCUMFLEX      = 0x00CAn;
C_CAPITAL_E_DIAERESIS       = 0x00CBn;
C_CAPITAL_I_GRAVE           = 0x00CCn;
C_CAPITAL_I_ACUTE           = 0x00CDn;
C_CAPITAL_I_CIRCUMFLEX      = 0x00CEn;
C_CAPITAL_I_DIAERESIS       = 0x00CFn;
C_CAPITAL_ETH               = 0x00D0n;
C_CAPITAL_N_TILDE           = 0x00D1n;
C_CAPITAL_O_GRAVE           = 0x00D2n;
C_CAPITAL_O_ACUTE           = 0x00D3n;
C_CAPITAL_O_CIRCUMFLEX      = 0x00D4n;
C_CAPITAL_O_TILDE           = 0x00D5n;
C_CAPITAL_O_DIAERESIS       = 0x00D6n;
C_MULTIPLICATION_SIGN       = 0x00D7n;
C_CAPITAL_O_STROKE          = 0x00D8n;
C_CAPITAL_U_GRAVE           = 0x00D9n;
C_CAPITAL_U_ACUTE           = 0x00DAn;
C_CAPITAL_U_CIRCUMFLEX      = 0x00DBn;
C_CAPITAL_U_DIAERESIS       = 0x00DCn;
C_CAPITAL_Y_ACUTE           = 0x00DDn;
C_CAPITAL_THORN             = 0x00DEn;
C_SMALL_SHARP_S             = 0x00DFn;
C_SMALL_A_GRAVE             = 0x00E0n;
C_SMALL_A_ACUTE             = 0x00E1n;
C_SMALL_A_CIRCUMFLEX        = 0x00E2n;
C_SMALL_A_TILDE             = 0x00E3n;
C_SMALL_A_DIAERESIS         = 0x00E4n;
C_SMALL_A_RING              = 0x00E5n;
C_SMALL_AE                  = 0x00E6n;
C_SMALL_C_CEDILLA           = 0x00E7n;
C_SMALL_E_GRAVE             = 0x00E8n;
C_SMALL_E_ACUTE             = 0x00E9n;
C_SMALL_E_CIRCUMFLEX        = 0x00EAn;
C_SMALL_E_DIAERESIS         = 0x00EBn;
C_SMALL_I_GRAVE             = 0x00ECn;
C_SMALL_I_ACUTE             = 0x00EDn;
C_SMALL_I_CIRCUMFLEX        = 0x00EEn;
C_SMALL_I_DIAERESIS         = 0x00EFn;
C_SMALL_ETH                 = 0x00F0n;
C_SMALL_N_TILDE             = 0x00F1n;
C_SMALL_O_GRAVE             = 0x00F2n;
C_SMALL_O_ACUTE             = 0x00F3n;
C_SMALL_O_CIRCUMFLEX        = 0x00F4n;
C_SMALL_O_TILDE             = 0x00F5n;
C_SMALL_O_DIAERESIS         = 0x00F6n;
C_DIVISION_SIGN             = 0x00F7n;
C_SMALL_O_STROKE            = 0x00F8n;
C_SMALL_U_GRAVE             = 0x00F9n;
C_SMALL_U_ACUTE             = 0x00FAn;
C_SMALL_U_CIRCUMFLEX        = 0x00FBn;
C_SMALL_U_DIAERESIS         = 0x00FCn;
C_SMALL_Y_ACUTE             = 0x00FDn;
C_SMALL_THORN               = 0x00FEn;
C_SMALL_Y_DIAERESIS         = 0x00FFn;

/*---------------------------------------------------------------------------*/
/* ASCII ENCODING */

func decode_ascii(text)
/* DOCUMENT decode_ascii(text);

     Convert TEXT into wide characters assuming TEXT is encoded in ASCII.
     Input TEXT can be an array of strings or an array of chars, the output is
     a vector of 32-bit integers.

   SEE ALSO: encode_ascii, decode_utf8, decode_latin1.
*/
{
  chars = decode_latin1(text);
  if (max(chars) >= 128n) {
    error, "invalid ASCII code";
  }
  return chars;
}

func encode_ascii(chars, text=)
/* DOCUMENT encode_ascii(chars);

     Convert wide characters CHARS into bytes using ASCII encoding.  Input
     CHARS must be an array of 32-bit integers.  If keyword TEXT is true, an
     array of strings is returned; otherwise, an array of bytes is returned.

   SEE ALSO: decode_ascii, encode_utf8, encode_latin1.
*/
{
  if (structof(chars) != int) {
    error, "expecting 32-bit integer(s)";
  }
  if (min(chars) < 0n || max(chars) > 127n) {
    error, "invalid ASCII character(s)";
  }
  bytes = char(chars);
  return (text ? strchar(bytes) : bytes);
}

/*---------------------------------------------------------------------------*/
/* UTF-8 ENCODING */

/*
 *    0 : 0000    4 : 0100    8 : 1000    C : 1100
 *    1 : 0001    5 : 0101    9 : 1001    D : 1101
 *    2 : 0010    6 : 0110    A : 1010    E : 1110
 *    3 : 0011    7 : 0111    B : 1011    F : 1111
 */

func decode_utf8(text)
/* DOCUMENT decode_utf8(text);

     Convert TEXT into wide characters assuming TEXT is encoded in UTF-8.
     Input TEXT can be an array of strings or an array of chars, the output is
     a vector of 32-bit integers.


   SEE ALSO: decode_utf8, decode_latin1.
*/
{
  /* Convert input as a vector of bytes, stored as 32-bit integers. */
  chars = decode_latin1(text);

  /* Find indices of bytes which needs to be converted. */
  if (max(chars) < 0x80n) {
    /* Quick return. */
    return chars;
  }
  index = where(chars >= 0x80n);
  other = chars(index);
  number = numberof(chars);

  /* Build a boolean mask true where the result will be stored and flase
     otherwise, that is where input bytes start by bits 10... */
  select = array(1n, number);
  j = where((other & 0xC0n) == 0x80n); // bytes starting with bits 10...
  if (is_array(j)) {
    select(index(j)) = 0n;
  }

  /* Deal with 2-byte codes. */
  j = where((other & 0xE0n) == 0xC0n); // bytes starting with bits 110...
  if (is_array(j)) {
    j1 = (j0 = index(j)) + 1;
    if (j1(0) > number) {
      error, "unfinished UTF-8 character";
    }
    if (anyof(select(j1))) {
      error, "invalid UTF-8 sequence";
    }
    chars(j0) = ((chars(j0) & 0x1Fn) << 6) | (chars(j1) & 0x3Fn);
  }

  /* Deal with 3-byte codes. */
  j = where((other & 0xF0n) == 0xE0n); // bytes starting with bits 1110...
  if (is_array(j)) {
    j2 = (j1 = (j0 = index(j)) + 1) + 1;
    if (j2(0) > number) {
      error, "unfinished UTF-8 character";
    }
    if (anyof(select(j1)) || anyof(select(j2))) {
      error, "invalid UTF-8 sequence";
    }
    chars(j0) = (((chars(j0) & 0x0Fn) << 12) |
                 ((chars(j1) & 0x3Fn) <<  6) | (chars(j2) & 0x3Fn));
  }

  /* Deal with 4-byte codes. */
  j = where((other & 0xF8n) == 0xF0n); // bytes starting with bits 11110...
  if (is_array(j)) {
    j3 = (j2 = (j1 = (j0 = index(j)) + 1) + 1) + 1;
    if (j3(0) > number) {
      error, "unfinished UTF-8 character";
    }
    if (anyof(select(j1)) || anyof(select(j2)) || anyof(select(j3))) {
      error, "invalid UTF-8 sequence";
    }
    chars(j0) = (((chars(j0) & 0x07n) << 18) | ((chars(j1) & 0x3Fn) << 12) |
                 ((chars(j2) & 0x3Fn) <<  6) |  (chars(j3) & 0x3Fn));
  }

  /* Return converted values. */
  return chars(where(select));
}

func encode_utf8(chars, text=)
/* DOCUMENT encode_utf8(chars);

     Convert wide characters CHARS into bytes using UTF-8 encoding.  Input
     CHARS must be an array of 32-bit integers.  If keyword TEXT is true, an
     array of strings is returned; otherwise, an array of bytes is returned.

   SEE ALSO: decode_utf8, encode_latin1, encode_ascii.
*/
{
  if (structof(chars) != int) {
    error, "expecting 32-bit integer(s)";
  }
  if (min(chars) < 0n || max(chars) >= 0x110000n ) {
    error, "invalid UTF-8 character(s)";
  }
  if (max(chars) < 0x80n) {
    /* Quick conversion. */
    bytes = char(chars);
  } else {
    /* Workspace. */
    bytes = array(char, 4*numberof(chars));

    /* Deal with ASCII characters. */
    ascii = (chars < 0x80n);
    j = where(ascii);
    if (is_array(j)) {
      bytes(4*j - 3) = chars(j);
      j = [];
    }

    /* Prepare for others. */
    index = where(! ascii);
    ascii = [];
    other = chars(index);
    index = 4*index - 3;
    cat2 = (other < 0x800n);
    cat4 = (other >= 0x10000n);
    cat3 = (! (cat2 | cat4));
    j = where(cat2);
    if (is_array(j)) {
      b = other(j);
      j = index(j);
      bytes(j)   = 0xC0n | ((b >>  6) & 0x1Fn);
      bytes(j+1) = 0x80n | ( b        & 0x3Fn);
      b = j = [];
    }
    cat2 = [];
    j = where(cat3);
    if (is_array(j)) {
      b = other(j);
      j = index(j);
      bytes(j)   = 0xE0n | ((b >> 12) & 0x0Fn);
      bytes(j+1) = 0x80n | ((b >>  6) & 0x3Fn);
      bytes(j+2) = 0x80n | ( b        & 0x3Fn);
      b = j = [];
    }
    cat3 = [];
    j = where(cat4);
    if (is_array(j)) {
      b = other(j);
      j = index(j);
      bytes(j)   = 0xF0n | ((b >> 18) & 0x07n);
      bytes(j+1) = 0x80n | ((b >> 12) & 0x3Fn);
      bytes(j+2) = 0x80n | ((b >>  6) & 0x3Fn);
      bytes(j+3) = 0x80n | ( b        & 0x3Fn);
      b = j = [];
    }
    cat4 = [];
    select = (bytes != '\0');
    select(1::4) = 1n;
    bytes = bytes(where(select));
  }
  return (text ? strchar(bytes) : bytes);
}

func encoding_tests
{
  last = 0x10FFFFn;
  if (0) {
    number = 1000*1000*10;
    chars = int(random(number)*(last + 1));
  } else {
    chars = int(indgen(0:last));
    number = numberof(chars);
  }

  write, format="%s: ", "Encoding UTF-8";
  tic;
  bytes = encode_utf8(chars);
  toc, number;

  write, format="%s: ", "Decoding UTF-8";
  tic;
  c = decode_utf8(bytes);
  toc, number;

  write, format="max. diff. = %d\n", max(abs(c - chars));
}
