OS := $(shell uname -s)
NF := $(findstring n,$(MAKEFLAGS))
trace = $(if $(NF),,@echo "$(1)")

INSTDIR = /opt/geopixe/latest

#GFORTRAN=g77
#FFLAGS=-fno-pedantic -Wno-globals -ff90 -ffree-form

# fortran flags common to all platforms ...
FFLAGS = -O3 -cpp -ffree-form -fno-range-check

#FFLAGS = -O0 -cpp -ffree-form -fno-range-check

# Linux
LR = /opt/gcc-4.5.3
LT = x86_64-unknown-linux-gnu
LGF = $(LR)/bin/$(LT)-gfortran
LGF32 = $(LGF) -D__32BIT__ -m32
LGF64 = $(LGF) -D__64BIT__ -m64
LGFFLAGS = -fPIC -fsecond-underscore $(FFLAGS)
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
WR = /opt/mingw-w64
WT = x86_64-w64-mingw32
WGF = $(WR)/bin/$(WT)-gfortran
WGF32 = $(WGF) -D__32BIT__ -m32
WGF64 = $(WGF) -D__64BIT__ -m64
WGFFLAGS = -mdll $(FFLAGS)
WLD32 = $(WGF) -m32
WLD64 = $(WGF) -m64
WLDFLAGS = -shared $(FFLAGS)

# Darwin
DGF = gfortran-mp-15
DGF32 = $(DGF) -D__32BIT__ -m32
DGF64 = $(DGF) -D__64BIT__ -m64
DGFFLAGS = -std=legacy -fallow-invalid-boz -fPIC $(FFLAGS)
DLD32 = $(DGF32)
DLD64 = $(DGF64)
DLDFLAGS = -bundle -flat_namespace -undefined suppress $(FFLAGS)

# these source files ...
SRCS = image_lib.f # ...
# produce this library (without suffix) ...
LIB = image_lib

# there are three platforms (Linux, Windows and Darwin)
# and two word-sizes (32 and 64 bit)
word-sizes = 32bit 64bit
ifeq ($(OS),Darwin)
word-sizes += arm64
int-suffs = do
out-suffs = dylib
else
int-suffs = lo wo
out-suffs = so dll
endif

OBJS = $(foreach s,$(int-suffs),$(foreach w,$(word-sizes),$(SRCS:.f=.$w.$s)))
LIBS = $(foreach s,$(out-suffs),$(foreach w,$(word-sizes),$(LIB).$w.$s))
ifeq ($(OS),Darwin)
LIBS += $(foreach s,$(out-suffs),$(LIB).$s)
endif

all: $(LIBS)

install: all
	$(call trace,Installing into $(INSTDIR) ...)
	@cp $(LIBS) $(INSTDIR)

clean:
	rm -f $(LIBS) $(OBJS)

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
	@$(DGF64) $(DGFFLAGS) -c -o $@ $^

$(LIB).32bit.so: $(SRCS:.f=.32bit.lo)
	$(call trace,[Linux 32bit] Linking $@ ...)
	@$(LLD32) $(LLDFLAGS) -o $@ $^ $(LLDLIBS32)

$(LIB).64bit.so: $(SRCS:.f=.64bit.lo)
	$(call trace,[Linux 64bit] Linking $@ ...)
	@$(LLD64) $(LLDFLAGS) -o $@ $^ $(LLDLIBS64)

$(LIB).32bit.dll: $(SRCS:.f=.32bit.wo)
	$(call trace,[Windows 32bit] Linking $@ ...)
	@$(WLD32) $(WLDFLAGS) -o $@ $^ $(WLDLIBS32)

$(LIB).64bit.dll: $(SRCS:.f=.64bit.wo)
	$(call trace,[Windows 64bit] Linking $@ ...)
	@$(WLD64) $(WLDFLAGS) -o $@ $^ $(WLDLIBS64)

$(LIB).32bit.dylib: $(SRCS:.f=.32bit.do)
	$(call trace,[Darwin 32bit] Linking $@ ...)
	@$(DLD32) $(DLDFLAGS) -o $@ $^ $(DLDLIBS32)

$(LIB).64bit.dylib: $(SRCS:.f=.64bit.do)
	$(call trace,[Darwin 64bit] Linking $@ ...)
	@$(DLD64) $(DLDFLAGS) -o $@ $^ $(DLDLIBS64)

$(LIB).dylib: $(foreach w,$(word-sizes),$(LIB).$w.dylib)
	$(call trace,[Darwin both] Linking $@ ...)
	@lipo -create -output $@ $^

$(LIB).arm64.dylib: $(SRCS:.f=.arm64.do)
	$(call trace,[Darwin arm64] Linking $@ ...)
	@$(DLD64) $(DLDFLAGS) -o $@ $^ $(DLDLIBS64)
