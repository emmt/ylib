# Changes in YLib

This page describes the most important changes in `YLib`. The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic
Versioning](https://semver.org/spec).

## Unreleased

### Added

* Add `tic` and `toc` to time execution of code.
* Add routines `encode_$enc` and `decode_$enc` in `encodings.i` to deal with character
  encodings. Encoding `$enc` is one of `latin1`, `ascii`, or `utf8`.
* Add extended sorting routines `xsort`, `xsort_uniq`, `xsort_rank`.
* New messaging functions `inform`, `warn`, and `throw` whose behavior is controlled by
  `styled_messages`.
* New functions `scalar_double`, `scalar_int`, etc. to retrieve a scala argument of given
  type.
* New functions `p_write_style` and `p_read_style`.
* New function `absdirname` to get the absolute directory of a file.
* New function `is_nan` and `ieee_generate` to deal with IEEE special values.
* Add rules for installation by [EasyYorick](https://github.com/emmt/EasyYorick).
* Add routines to calculate an Hashed Message Authentication digest (HMAC).
* Move Yeti string functions `strtrimleft` and `strtrimright` in `utils.i`

### Changed

* A simple hyphen is considered as a normal argument.
* Improved label algorithm.
* Make some functions raise errors at caller level.
* Change unroll offsets in `img_convolve`.
* Assume function `heapsort` is auto-loaded from Yeti.
* Change map behavior for `[]` argument.

### Fixed

* Do not force installation of hacked plot commands.
* Fix `pl_img`.
* Fix code for saving integer data in `mda_save`.
* Add `ylib-start.i` with auto-load rules.
* Fix missing `options.i`, `fzero.i`, and `statistics.i` in `Makefile.in`.
* Use OXY object instead of temporary file and fix paths in `rgb_build_database`. Fix path
  of databases.
* Function `window_geometry` is now part of Yorick (was Yeti).
* Fix printing of version.

## February 19, 2024
* Add functions `test_init`, `test_assert`, `test_eval`, and `test_summary`
  for testing Yorick code.

## February 2, 2022
* Fix includes/requires.
* Move Yeti string functions `strtrimleft` and `strtrimright` in `utils.i`.

## May 15, 2020
* Add routines to calculate an Hashed Message Authentication digest (HMAC) as
  described in RFC 2104 (https://www.rfc-editor.org/rfc/rfc2104.txt).

## December 3, 2016
* Detection of duplicate command line options.
* List of values allowed in command line options.

## December 1, 2016
* Package for parsing command line options.
