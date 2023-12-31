TOPDIR=../../
OS_ARCH='Linux'

DIR_CXXFLAGS = -rdynamic -O2
DIR_CPPFLAGS = -D_MIT_POSIX_THREADS -D_REENTRANT
DIR_INCLUDES = -I./ 

IDLSRCS = NotMyChildException.idl NoSuchAttributeException.idl \
          NoSuchNodeException.idl domDefs.idl Node.idl \
          NodeIterator.idl Text.idl Attribute.idl Element.idl \
          Document.idl \
          Comment.idl ProcessingInstruction.idl AttributeList.idl

PLUG_TARGET = libDOMcorba.so
PLUG_OBJS_libDOMcorba.so = $(IDLSRCS:.idl=SK.o)

include $(TOPDIR)/config/rules.mk
