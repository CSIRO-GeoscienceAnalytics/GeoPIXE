# Image Fortran routines

The fortran code in the file `image_lib.f` provides
Image routines for DLL or shared lib to serve IDL with
matrix routines for DA image accumulation and image processing
and fiddly bit twiddling for certain input data devices.

For details of how to call a Fortran function from within IDL, see
[this](https://www.nv5geospatialsoftware.com/docs/FORTRANExamples.html) page.

Three platforms are supported: Linux, Windows and MacOS.

Two bit sizes are suppoprted: 32bit (except MacOS) and 64bit.

The code was originally built using version 4.5 of gcc (https://gcc.gnu.org)
on Linux and MacOS (Intel) and with [MinGW-w64](https://www.mingw-w64.org/)
on Windows.

However, when adding the MacOS M1/M2/M3 support, using MacPorts, it was
discovered that gcc version 4.5 could not be installed (it was flagged
as a failing build) - so the latest version was used instead, version 15.

It was subsequently discovered on the Mac platform that gfortran from
gcc15 was generating bogus code (references to \_lroundf_ with no
corresponding implementation in any of the libraries - shared or static).

Rather than try to fix this, which might take a lot of effort, the "flang"
ccompiler from LLVM was tried - version 22 from Macports. This appeared to
generate good code, without needing the "kind" manipulations of `iand()`
and `ior()` required by gfortran. However, it did not support 32bit code
so 32bit support on Macos was removed.

The same compiler was then also used for building Linux 64bit code.
MingW has an LLVM version but it was fairly new and a suitable source of
debian packages could not be found.

This table summarises the current support:

| O/S     | Arch  | Word Size | Built on | with Compiler    |
| ------- | ----- | --------- | -------- | ---------------- |
| Linux   | Intel | 32bit     | Linux    | gfortran         |
| Linux   | Intel | 64bit     | Linux    | flang            |
| Windows | Intel | 32bit     | Linux    | gfortran (MingW) |
| Windows | Intel | 64bit     | Linux    | gfortran (MingW) |
| MacOS   | Intel | 64bit     | MacOS    | flang            |
| MacOS   | Arm   | 64bit     | MacOS    | flang            |

## Linux

Currently built on Ubuntu 22.04 using gcc11.

I installed these packages:
* gfortran-11
then ran `make all` (builds both linux and windows libs)

## Windows

The windows libs can be built on Linux via the MinGW packages.

I installed these packages:
* mingw-w64
* gfortran-mingw-w64
then ran `make all` (builds both linux and windows libs)

## MacOS

On MacOS there is an extra bit size (really a different architecture): arm64.
In addition, 32bit is no longer supported.

All supported bit sizes and architectures are bundled into a single `.dylib`
file and this is searched for first by the geopixe loading code.

### Building with GCC15 (aka GFORTRAN15)

Building with gfortran from gcc15 initially had a lot of errors but command
line options to downgrade these to warnings were added. For details of gfortran
command line options see
[this](https://gcc.gnu.org/onlinedocs/gcc-15.1.0/gfortran/Option-Summary.html).

One error remained and it was caused by a new restriction that required that
the two integer arguments passed to the iand() and ior() functions must have
the same "kind" (whatever that is). The solution was provided in
[this](https://stackoverflow.com/questions/59756525/iand-with-different-kind-parameters-using-new-gfortran-version)
stack overflow article.

However, it was discovered later that there were linking errors caused by the
compiler generating references to functions that did not exist in any of the
libraries provided by gcc15 or MacOS.

### Building with LLVM FLANG

The flang compiler from LLVM was then tried and that seemed to run much more
smoothly, with faster compile times, and no requirement to do the "kind"
manipulations for `iand()` and `ior()`.

flang did not support 32bit on MacOS though, so 32bit was dropped. Both
Intel and Arm 64bit architectures seemed to build well with flang though.
