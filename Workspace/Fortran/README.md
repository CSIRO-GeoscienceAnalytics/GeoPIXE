# Image Fortran routines

The fortran code in the file `image_lib.f` provides
Image routines for DLL or shared lib to serve IDL with
matrix routines for DA image accumulation and image processing
and fiddly bit twiddling for certain input data devices.

For details of how to call a Fortran function from within IDL, see
[this](https://www.nv5geospatialsoftware.com/docs/FORTRANExamples.html) page.

Three platforms are supported: Linux, Windows and MacOS.

Two bit sizes are suppoprted (on Intel platforms): 32bit and 64bit.

The code was originally built using version 4.5 of gcc (https://gcc.gnu.org)
on Linux and MacOS (Intel) and with [MinGW-w64](https://www.mingw-w64.org/)
on Windows.

However, when adding the MacOS M1/M2/M3 support, using MacPorts, it was
discovered that gcc version 4.5 could not be installed (it was flagged
as a failing build) - so the latest version was used instead, version 15.

## Linux

TBA ...

## Windows

TBA ...

## MacOS

On MacOS there is a third bit size (which is really a different architecture): arm64.

Theoretically, all three bit sizes should be bundled into a single `.dylib` file for all
possible MacOS architectures (and this has been built), but this is not searched
for in the main GeoPIXE code (yet?).

Building with gcc15 initially had a lot of errors but command line options to
downgrade these to warnings were added. For details of gfortran command line
options see
[this](https://gcc.gnu.org/onlinedocs/gcc-15.1.0/gfortran/Option-Summary.html).

One error remained and it was caused by a new restriction that required that
the two integer arguments passed to the iand() and ior() functions must have
the same "kind" (whatever that is). The solution was provided in
[this](https://stackoverflow.com/questions/59756525/iand-with-different-kind-parameters-using-new-gfortran-version)
stack overflow article.
