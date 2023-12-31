#ifndef _Prague_config_hh
#define _Prague_config_hh

@TOP@
/* Define if you have the dlfcn family of functions for dynamic loading  */
#undef HAVE_DLFCN

/* Define if you have the AIX family of functions for dynamic loading */
#undef HAVE_DLAIX

/* Define if you have the dlclose function.  */
#undef HAVE_DLCLOSE

/* Define if you have the dlopen function.  */
#undef HAVE_DLOPEN

/* Define if you have the dlsym function.  */
#undef HAVE_DLSYM

/* Define if you have the shl_load function.  */
#undef HAVE_SHL_LOAD

/* Define if you have the <dl.h> header file.  */
#undef HAVE_DL_H

/* Define if you have the <dlfcn.h> header file.  */
#undef HAVE_DLFCN_H

/* Define if you have the dl library (-ldl).  */
#undef HAVE_LIBDL

/* Define if you have the socklen_t type */
#undef HAVE_SOCKLEN_T

/* Define if you have inet sockets */
#undef HAVE_INET_SOCKETS

/* Define if you have unix sockets */
#undef HAVE_UNIX_SOCKETS

/* Define if you have the inet_aton function. */
#undef HAVE_INET_ATON

/* Define if you have the zlib library. */
#undef HAVE_ZLIB

/* some threading macros. */
#undef _REENTRANT
#undef UsePthread
#undef _THREAD_SAFE

@BOTTOM@

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t;
#endif

#ifndef HAVE_INET_ATON
#define inet_aton(cp, addr) (((*(unsigned long int *)(addr)) = inet_addr(cp)) != -1)
#endif

/* Define if you have the strsignal function.  */
#undef HAVE_STRSIGNAL
/* Whether strsignal must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_STRSIGNAL

#ifndef HAVE_STRSIGNAL
#include <signal.h>
inline const char *strsignal(int signo) { return _sys_siglist[signo];}
#elif NEED_DECLARATION_STRSIGNAL
extern "C" const char *strsignal(int);
#endif

/* Define if you want tracer support */
#undef TRACER

#endif
