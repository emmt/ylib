YLib
====

YLib (Yorick Library) is a collection of useful interpreted functions
for Yorick.

Contents
--------
* img.i - routines for manipulation image.
* utils.i - various utilities.
* plot.i - plotting utilities.


Installation
------------

1. You simply clone the contents of the repository:

    git clone https://github.com/emmt/ylib.git

   which creates a directory `ylib` with the Git repository.

2. You may update the time stamps of the file (so that you can refer
to them in case of problems).  To that end, in the `ylib` directory,
execute the following command:

    ./tools/code_dater *.i

3. If you want to keep the synchronization with the Git repository, it
is probably a good idea to create symbolic links in your `~/Yorick`
directory toward all the `*.i` files in the `ylib` directory.  It is
also a good idea to follows the instructions in the "Developpers"
section.


Instructions for Developers
---------------------------

Checking out and comitting involve a number of filters.  The filters are
in the `tools` directory.  To activate the filters you have to edit the
file `.git/config' and add/modify the following lines:

    [filter "code-filter"]
            clean = ./tools/code_cleaner
            smudge = ./tools/code_dater
    [filter "text-filter"]
            clean = ./tools/code_cleaner
            smudge = ./tools/code_dater
    [filter "yorick-makefile-filter"]
            clean = ./tools/ymk_cleaner
            smudge = cat

