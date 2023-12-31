TOPDIR=../..
OS_ARCH='Linux'

DIR_CPPFLAGS = -D_MIT_POSIX_THREADS -D_REENTRANT -D__OSVERSION__=2 -D__OMNIORB2__ -D__x86__
DIR_CXXFLAGS = -g -pthread
DIR_INCLUDES =  -I.  
DIR_LDFLAGS  =  -L. -L$(TOPDIR)/lib -L../libregistrar/\
		-lomniORB2 -lomnithread -lpthread -ltcpwrapGK -lomniLC \
		-lregistrar

# naming is a special case, cause omni includes a definition of it too
CXXSRCS = Db.cc Dcache.cc Registrar_impl.cc Test1.cc \
		Test2.cc Test3.cc Test4.cc main.cc

EX_TARGET = Registrar RegTest1 RegTest2 RegTest3 RegTest4
EX_OBJS_Registrar =  Db.o Dcache.o Registrar_impl.o main.o
EX_OBJS_RegTest1 =  Test1.o
EX_OBJS_RegTest2 =  Test2.o
EX_OBJS_RegTest3 =  Test3.o
EX_OBJS_RegTest4 =  Test4.o

include $(TOPDIR)/config/rules.mk

