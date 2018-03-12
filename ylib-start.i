autoload, "statistics.i", density, hist;

autoload, "fft_utils.i", abs2, fft_best_dim, fft_indgen, fft_dist, fft_freqlist;
autoload, "fft_utils.i", fft_smooth, fft_convolve, fft_gaussian_psf, fft_gaussian_mtf;
//autoload, "fft_utils.i", fft_get_ndims, fft_symmetric_index, fft_centroid, reverse_dims;
autoload, "fft_utils.i", fft_recenter, fft_recenter_at_max;
//autoload, "fft_utils.i", fft_roll_1d, fft_roll_2d;
//autoload, "fft_utils.i", fft_shift_phasor, fft_unphasor, fft_fine_shift;
//autoload, "fft_utils.i", fft_interp_real, fft_interp_complex, fft_interp;
autoload, "fft_utils.i", fft_plh, fft_plg, fft_pli, fft_plc, fft_plfc;
//autoload, "fft_utils.i", fft_of_two_real_arrays;
//autoload, "fft_utils.i", fft_paste;

autoload, "fmin.i", fmin, fmin_global, fmin_range;
autoload, "fzero.i", fzero;

autoload, "img.i", img_dims, img_plot, img_cbar;
autoload, "img.i", img_resize, img_thumbnail, img_interpolate;
autoload, "img.i", img_extract_parallelogram_as_rectangle;
autoload, "img.i", img_max, img_fft_centered_at_max, img_convolve, img_pad, img_paste, img_crop;
autoload, "img.i", img_to_gray, img_flatten, img_photometry;
//autoload, "img.i", img_flt_max, img_flt_flac;
autoload, "img.i", img_get_file_type, img_read, img_write, img_tmpnam;

autoload, "linalg.i", svd_eigen_decomp, gram_schmidt_orthonormalization;
autoload, "linalg.i", trace, diag, dot, euclidean_norm;
autoload, "linalg.i", pm;
//autoload, "linalg.i", interpolation_matrix, kronecker_product, faddeev_leverrier;
//autoload, "linalg.i", cholesky;
//autoload, "linalg.i", sv_dcmp, sv_solve_trunc, sv_solve_wiener;
//autoload, "linalg.i", pseudo_inverse;
//autoload, "linalg.i", make_matrix;

autoload, "mda.i", mda_save, mda_recv;

autoload, "options.i", opt_init, opt_parse, opt_usage, opt_error;

//autoload, "plot.i", pl_arrow, pl_fc, pl_img, pl_cbar, color_bar;
//autoload, "plot.i", pla, pls_mesh, pls, plfg;
autoload, "plot.i", plh, plp;
autoload, "plot.i", plhline, plvline;
autoload, "plot.i", pl_get_color, pl_get_palette, pl_get_font, pl_get_symbol, pl_get_axis_flags, pl_set_ndigits;
//autoload, "plot.i", pl3t, pl3dj, pl3s;
autoload, "plot.i", xwindow;
//autoload, "plot.i", xbtn_plot, xbtn_which;
autoload, "plot.i", xmouse_point, xmouse_box, xmouse_line, xmouse_length, xmouse, xmouse_demo;
autoload, "plot.i", pl_box, pl_cbox, pl_circle, pl_ellipse, pl_hinton;
autoload, "plot.i", ps2png, ps2jpeg, win2png, win2jpeg;
autoload, "plot.i", pl_span, pl_map, win_copy_lim;
//autoload, "plot.i", pl_database_index_to_rgb, pl_database_index_to_packed, pl_rgb_to_packed, pl_packed_to_rgb;

autoload, "utils.i", eval, map, lambda, cast, collate, swap_bytes;
autoload, "utils.i", undersample, resample, spline_zoom, rescale, smooth;
//autoload, "utils.i", ndims_of, width_of, height_of, depth_of;
//autoload, "utils.i", is_integer_scalar, is_string_scalar, is_absolute_path;
autoload, "utils.i", scalar_double, scalar_int, scalar_long, scalar_string,
  vector_double, is_nan, is_inf, ieee_generate;
autoload, "utils.i", pwd, glob;
//autoload, "utils.i", dirname, basename;
autoload, "utils.i", fulldirname;
autoload, "utils.i", locate, load, tempfile, filesize, structname;
autoload, "utils.i", strip_file_extension, expand_file_name, get_file_name, protect_file_name, read_ascii, load_text, dump_text, guess_compression, xopen, raw_read;
autoload, "utils.i", pw_get_user, pw_get_uid, pw_get_gid, pw_get_name, pw_get_home, pw_get_shell;
autoload, "utils.i", pdb_list, pdb_restore_all;
autoload, "utils.i", timer_start, timer_elapsed;
autoload, "utils.i", moments, stat;
autoload, "utils.i", ansi_term, printf, inform, warn, throw, styled_messages;
//autoload, "utils.i", open_url;
//autoload, "utils.i", strlower, strupper, strcut, strjoin, strchr, strrchr;

autoload, "xsort.i", xsort, xsort_rank, xsort_uniq;
