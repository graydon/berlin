TOPDIR=../..
OS_ARCH='Linux'

DIR_CPPFLAGS = -D_MIT_POSIX_THREADS -D_REENTRANT -D__OSVERSION__=2 -D__OMNIORB2__ -D__x86__
DIR_CXXFLAGS = -g -pthread
DIR_INCLUDES =  -I.  
DIR_LDFLAGS  =  -L. -L$(TOPDIR)/lib \

# naming is a special case, cause omni includes a definition of it too
ALL_SUBTARGETS = Registrar.idl
IDLSRCS = $(ALL_SUBTARGETS)
CXXSRCS = RegistrarSK.cc RegistrarDynSK.cc

SO_TARGET = libregistrar.so
SO_OBJS_libregistrar.so = RegistrarSK.o RegistrarDynSK.o
SO_MAJOR_libregistrar.so := 0
SO_MINOR_libregistrar.so := 0
SO_PATCH_libregistrar.so := 1

include $(TOPDIR)/config/rules.mk

all::
	$(CP) Registrar.hh $(TOPDIR)/include/support/registrar/
