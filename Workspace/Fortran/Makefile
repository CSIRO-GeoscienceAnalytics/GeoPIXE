OS := $(shell uname -s)
MACH := $(shell uname -m)
NF := $(findstring n,$(MAKEFLAGS))
trace = $(if $(NF),,@echo "$(1)")

# trailing slash is important
DESTDIR = ../geopixe/
INSTDIR = /opt/geopixe/latest

# fortran flags common to all platforms ...
#FFLAGS = -O0 -cpp -ffree-form -fno-range-check
#FFLAGS = -O3 -cpp -ffree-form -ffree-line-length-512 -fno-range-check
FFLAGS = -O3 -cpp -ffree-form

# gfortran specific flags
GFORTRAN_FLAGS = -D__GFORTRAN__ -std=legacy -fallow-invalid-boz -fsecond-underscore -ffree-line-length-512 -fno-range-check

# flang specific flags
FLANG_FLAGS = -D__FLANG__

# Linux
LGF32 = gfortran $(GFORTRAN_FLAGS) -D__32BIT__ -m32
LGF64 = flang-22 $(FLANG_FLAGS) -D__64BIT__ -m64
LGFFLAGS = -fPIC $(FFLAGS)
LLD32 = ld -m elf_i386
LLD64 = ld -m elf_x86_64
LLDFLAGS = -shared
LLDLIBS32 =
LLDLIBS64 =

# Windows
WGF32 = i686-w64-mingw32-gfortran $(GFORTRAN_FLAGS) -D__32BIT__ -m32
WGF64 = x86_64-w64-mingw32-gfortran $(GFORTRAN_FLAGS) -D__GFORTRAN__ -D__64BIT__ -m64
WGFFLAGS = -mdll $(FFLAGS)
WLD32 = i686-w64-mingw32-gfortran -m32 $(GFORTRAN_FLAGS)
WLD64 = x86_64-w64-mingw32-gfortran -m64 $(GFORTRAN_FLAGS)
WLDFLAGS = -shared $(FFLAGS)
WLDLIBS32 =
WLDLIBS64 =

# Darwin
DGF = flang-mp-22 $(FLANG_FLAGS)
DGF64 = $(DGF) -D__64BIT__
DGFA64 = $(DGF) -D__64BIT__
DGFFLAGS = -fPIC $(FFLAGS)
DLD64 = $(DGF64)
DLDA64 = $(DGFA64)
DLDFLAGS = $(FFLAGS) -shared
DLDLIBS64 =
DLDLIBSA64 =

# these source files ...
SRCS = image_lib.f # ...
# produce this library (without suffix) ...
LIB = image_lib

# define intermediate file and output file suffixes
# intermediate files are cleaned
#
# there are three platforms (Linux, Windows and Darwin)
# and two word-sizes (32 and 64 bit)
ifeq ($(OS),Darwin)
ifeq ($(MACH),arm64)
word-sizes = arm64
else
word-sizes = 64bit
endif
int-suffs = do
out-suffs = dylib
else
word-sizes = 32bit 64bit
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

.PHONY: help
help:
	$(call trace,Build one or more of the following targets:)
	$(if $(NF),,@for l in $(LIBS); do echo "	$$l"; done)

all: $(LIBS)

install: all
	$(call trace,Installing into $(INSTDIR) ...)
	@cp $(LIBS) $(INSTDIR)

clean:
	$(call trace,Cleaning intermediate and log files ...)
	@rm -f $(OBJS) *.errs

fullclean: clean
	$(call trace,Cleaning target files ...)
	@rm -f $(LIBS)

# rules for building intermediate files

%.32bit.lo: %.f
	$(call trace,[Linux 32bit] Compiling $^ ...)
	@$(LGF32) $(LGFFLAGS) -c -o $@ $^ >$@.errs 2>&1

%.64bit.lo: %.f
	$(call trace,[Linux 64bit] Compiling $^ ...)
	@$(LGF64) $(LGFFLAGS) -c -o $@ $^ >$@.errs 2>&1

%.32bit.wo: %.f
	$(call trace,[Windows 32bit] Compiling $^ ...)
	@$(WGF32) $(WGFFLAGS) -c -o $@ $^ >$@.errs 2>&1

%.64bit.wo: %.f
	$(call trace,[Windows 64bit] Compiling $^ ...)
	@$(WGF64) $(WGFFLAGS) -c -o $@ $^ >$@.errs 2>&1

%.64bit.do: %.f
	$(call trace,[Darwin 64bit] Compiling $^ ...)
	@$(DGF64) $(DGFFLAGS) -c -o $@ $^ >$@.errs 2>&1

%.arm64.do: %.f
	$(call trace,[Darwin arm64] Compiling $^ ...)
	@$(DGFA64) $(DGFFLAGS) -c -o $@ $^ >$@.errs 2>&1

# rules for building output files

$(DESTDIR)$(LIB).32bit.so: $(SRCS:.f=.32bit.lo)
	$(call trace,[Linux 32bit] Linking $@ ...)
	@$(LLD32) $(LLDFLAGS) -o $@ $^ $(LLDLIBS32) >$(notdir $@).errs 2>&1

$(DESTDIR)$(LIB).64bit.so: $(SRCS:.f=.64bit.lo)
	$(call trace,[Linux 64bit] Linking $@ ...)
	@$(LLD64) $(LLDFLAGS) -o $@ $^ $(LLDLIBS64) >$(notdir $@).errs 2>&1

$(DESTDIR)$(LIB).32bit.dll: $(SRCS:.f=.32bit.wo)
	$(call trace,[Windows 32bit] Linking $@ ...)
	@$(WLD32) $(WLDFLAGS) -o $@ $^ $(WLDLIBS32) >$(notdir $@).errs 2>&1

$(DESTDIR)$(LIB).64bit.dll: $(SRCS:.f=.64bit.wo)
	$(call trace,[Windows 64bit] Linking $@ ...)
	@$(WLD64) $(WLDFLAGS) -o $@ $^ $(WLDLIBS64) >$(notdir $@).errs 2>&1

$(DESTDIR)$(LIB).64bit.dylib: $(SRCS:.f=.64bit.do)
	$(call trace,[Darwin 64bit] Linking $@ ...)
	@$(DLD64) $(DLDFLAGS) -o $@ $^ $(DLDLIBS64) >$(notdir $@).errs 2>&1

$(DESTDIR)$(LIB).arm64.dylib: $(SRCS:.f=.arm64.do)
	$(call trace,[Darwin arm64] Linking $@ ...)
	@$(DLDA64) $(DLDFLAGS) -o $@ $^ $(DLDLIBSA64) >$(notdir $@).errs 2>&1

$(DESTDIR)$(LIB).dylib: $(foreach w,$(word-sizes),$(DESTDIR)$(LIB).$w.dylib)
	$(call trace,[Darwin both] Linking $@ ...)
	@lipo -create -output $@ $^ >$(notdir $@).errs 2>&1
