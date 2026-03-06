OS := $(shell uname -s)
NF := $(findstring n,$(MAKEFLAGS))
trace = $(if $(NF),,@echo "$(1)")

# trailing slash is important
DESTDIR = ../geopixe/
INSTDIR = /opt/geopixe/latest

# fortran flags common to all platforms ...
#FFLAGS = -O0 -cpp -ffree-form -fno-range-check
FFLAGS = -O3 -cpp -ffree-form -ffree-line-length-512 -fno-range-check

# Linux
LGF = gfortran-14
LGF32 = $(LGF) -D__32BIT__ -m32
LGF64 = $(LGF) -D__64BIT__ -m64
LGFFLAGS = -std=legacy -fallow-invalid-boz -fPIC -fsecond-underscore $(FFLAGS)
#LLD32 = $(LGF) -m32
#LLD64 = $(LGF) -m64
#LLD32 = ld --oformat elf32-i386
#LLD64 = ld --oformat elf64-x86-64
LLD32 = ld -m elf_i386
LLD64 = ld -m elf_x86_64
LLDFLAGS = -shared
LLDLIBS32 =
LLDLIBS64 =

# Windows
WGF32 = i686-w64-mingw32-gfortran -D__32BIT__ -m32
WGF64 = x86_64-w64-mingw32-gfortran -D__64BIT__ -m64
WGFFLAGS = -std=legacy -fallow-invalid-boz -mdll $(FFLAGS)
WLD32 = i686-w64-mingw32-gfortran -m32
WLD64 = x86_64-w64-mingw32-gfortran -m64
WLDFLAGS = -shared $(FFLAGS)
WLDLIBS32 =
WLDLIBS64 =

# Darwin
DGF = gfortran-mp-15
DGF32 = $(DGF) -D__32BIT__ -m32
DGF64 = $(DGF) -D__64BIT__ -m64
DGFA64 = $(DGF) -D__64BIT__ -m64
DGFFLAGS = -std=legacy -fallow-invalid-boz -fPIC $(FFLAGS)
DLD32 = $(DGF32)
DLD64 = $(DGF64)
DLDA64 = $(DGF64)
DLDFLAGS = -bundle -flat_namespace -undefined suppress $(FFLAGS)
DLDLIBS32 = -static-libgfortran
DLDLIBS64 = -static-libgfortran
DLDLIBSA64 = -static-libgfortran

# these source files ...
SRCS = image_lib.f # ...
# produce this library (without suffix) ...
LIB = image_lib

# define intermediate file and output file suffixes
# intermediate files are cleaned
#
# there are three platforms (Linux, Windows and Darwin)
# and two word-sizes (32 and 64 bit)
word-sizes = 32bit 64bit
ifeq ($(OS),Darwin)
# and on MacOS also M1/M2/M3/... aka arm64
word-sizes += arm64
int-suffs = do
out-suffs = dylib
else
int-suffs = lo wo
out-suffs = so dll
endif

# all possible intermediate files (aka object files)
OBJS = $(foreach s,$(int-suffs),$(foreach w,$(word-sizes),$(SRCS:.f=.$w.$s)))
.SECONDARY: $(OBJS)
# all possible output files (aka library files)
LIBS = $(foreach s,$(out-suffs),$(foreach w,$(word-sizes),$(DESTDIR)$(LIB).$w.$s))
ifeq ($(OS),Darwin)
# and on MacOS we also build a container dylib containing all possible MacOS dylibs
LIBS += $(foreach s,$(out-suffs),$(DESTDIR)$(LIB).$s)
endif
.PRECIOUS: $(LIBS)

default:
	$(call trace,Build one or more of the following targets:)
	$(if $(NF),,@for l in $(LIBS); do echo "	$$l"; done)

all: $(LIBS)

install: all
	$(call trace,Installing into $(INSTDIR) ...)
	@cp $(LIBS) $(INSTDIR)

clean:
	$(call trace,Cleaning intermediate files ...)
	@rm -f $(OBJS)

# rules for building intermediate files

%.32bit.lo: %.f
	$(call trace,[Linux 32bit] Compiling $^ ...)
	@$(LGF32) $(LGFFLAGS) -c -o $@ $^

%.64bit.lo: %.f
	$(call trace,[Linux 64bit] Compiling $^ ...)
	@$(LGF64) $(LGFFLAGS) -c -o $@ $^

%.32bit.wo: %.f
	$(call trace,[Windows 32bit] Compiling $^ ...)
	@$(WGF32) $(WGFFLAGS) -c -o $@ $^

%.64bit.wo: %.f
	$(call trace,[Windows 64bit] Compiling $^ ...)
	@$(WGF64) $(WGFFLAGS) -c -o $@ $^

%.32bit.do: %.f
	$(call trace,[Darwin 32bit] Compiling $^ ...)
	@$(DGF32) $(DGFFLAGS) -c -o $@ $^

%.64bit.do: %.f
	$(call trace,[Darwin 64bit] Compiling $^ ...)
	@$(DGF64) $(DGFFLAGS) -c -o $@ $^

%.arm64.do: %.f
	$(call trace,[Darwin arm64] Compiling $^ ...)
	@$(DGFA64) $(DGFFLAGS) -c -o $@ $^

# rules for building output files

$(DESTDIR)$(LIB).32bit.so: $(SRCS:.f=.32bit.lo)
	$(call trace,[Linux 32bit] Linking $@ ...)
	@$(LLD32) $(LLDFLAGS) -o $@ $^ $(LLDLIBS32)

$(DESTDIR)$(LIB).64bit.so: $(SRCS:.f=.64bit.lo)
	$(call trace,[Linux 64bit] Linking $@ ...)
	@$(LLD64) $(LLDFLAGS) -o $@ $^ $(LLDLIBS64)

$(DESTDIR)$(LIB).32bit.dll: $(SRCS:.f=.32bit.wo)
	$(call trace,[Windows 32bit] Linking $@ ...)
	@$(WLD32) $(WLDFLAGS) -o $@ $^ $(WLDLIBS32)

$(DESTDIR)$(LIB).64bit.dll: $(SRCS:.f=.64bit.wo)
	$(call trace,[Windows 64bit] Linking $@ ...)
	@$(WLD64) $(WLDFLAGS) -o $@ $^ $(WLDLIBS64)

$(DESTDIR)$(LIB).32bit.dylib: $(SRCS:.f=.32bit.do)
	$(call trace,[Darwin 32bit] Linking $@ ...)
	@$(DLD32) $(DLDFLAGS) -o $@ $^ $(DLDLIBS32)

$(DESTDIR)$(LIB).64bit.dylib: $(SRCS:.f=.64bit.do)
	$(call trace,[Darwin 64bit] Linking $@ ...)
	@$(DLD64) $(DLDFLAGS) -o $@ $^ $(DLDLIBS64)

$(DESTDIR)$(LIB).arm64.dylib: $(SRCS:.f=.arm64.do)
	$(call trace,[Darwin arm64] Linking $@ ...)
	@$(DLDA64) $(DLDFLAGS) -o $@ $^ $(DLDLIBSA64)

$(DESTDIR)$(LIB).dylib: $(foreach w,$(word-sizes),$(DESTDIR)$(LIB).$w.dylib)
	$(call trace,[Darwin both] Linking $@ ...)
	@lipo -create -output $@ $^
