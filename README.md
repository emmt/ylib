# YLib

YLib (Yorick Library) is a collection of useful interpreted functions for
[Yorick](http://github.com/LLNL/yorick/).


## Contents

Amon others:
* `img.i` - routines for manipulation image.
* `utils.i` - various utilities.
* `plot.i` - plotting utilities.


## Installation

### Installation with EasyYorick

Installation of YLib by [EasyYorick](https://github.com/emmt/EasyYorick) is
fully supported.  Assuming you have installed EasyYorick, you just have to
execute:

```sh
ypkg install yorick ylib
```

which should install Yorick (if not yet installed) and YLib.

To upgrade to the last master version:

```sh
ypkg upgrade ylib
```


### Manual installation

1. You must have [Yorick](http://github.com/LLNL/yorick/) installed on your machine.

2. Unpack the [software code](https://github.com/emmt/ylib/archive/master.zip)
   somewhere or clone the Git repository:

   ```sh
   git clone https://github.com/emmt/ylib.git ylib
   ```

   if you want/prefer to use HTTPS, or:

   ```sh
   git clone git@github.com:emmt/ylib.git ylib
   ```

   if you want/prefer to use SSH.  Any of these commands creates a local GIT
   repository named `ylib`.


3. Configure for compilation.  There are two possibilities:

   - For an **in-place build**, go to the source directory, say `$SRC_DIR`, of
     the plug-in code and run the configuration script:

     ```sh
     cd $SRC_DIR
     ./configure
     ```

     To see the configuration options, type:

     ```sh
     ./configure --help
     ```

   - For an **out-of-place build**, create a dedicated build directory, say
     `$BUILD_DIR`, go to the build directory and run the configuration script:

     ```sh
     mkdir -p $BUILD_DIR
     cd $BUILD_DIR
     $SRC_DIR/configure
     ```

     where `$SRC_DIR` is the path to the source directory of the plug-in code.
     To see the configuration options, type:

     ```sh
     $SRC_DIR/configure --help
     ```

4. Build YLib:

   ```sh
   make clean
   make
   ```

5. Install YLib (you must have write access granted to Yorick directories):

   ```sh
   make install
   ```
