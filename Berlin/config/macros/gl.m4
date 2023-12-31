dnl
dnl This source file is a part of the Berlin Project.
dnl Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
dnl http://www.berlin-consortium.org
dnl
dnl This library is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU Library General Public
dnl License as published by the Free Software Foundation; either
dnl version 2 of the License, or (at your option) any later version.
dnl
dnl This library is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Library General Public License for more details.
dnl
dnl You should have received a copy of the GNU Library General Public
dnl License along with this library; if not, write to the
dnl Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
dnl MA 02139, USA.

dnl
dnl BERLIN_GL_HOOK(script-if-gl-found, version, failflag)
dnl
dnl If failflag is "failure", it aborts if GL is not found.
dnl `version' is OpenGL version, not Mesa version.
dnl

AC_DEFUN([BERLIN_GL_HOOK],[

	AC_LANG_SAVE
	AC_LANG_C

	AC_ARG_WITH(gl-prefix,
		[  --with-gl-prefix=PFX    Prefix for OpenGL],[
		gl_prefix="$withval"])
	AC_ARG_WITH(gl-eprefix,
		[  --with-gl-eprefix=PFX   Exec prefix for OpenGL],[
		gl_eprefix="$withval"],[
		gl_eprefix="$gl_prefix"])

	dnl Check for OpenGL includes
	if test x$gl_prefix != x ; then
		GL_INCLUDES=-I$gl_prefix/include
	fi
	save_CPPFLAGS="$CPPFLAGS"
	CPPFLAGS="$GL_INCLUDES $CPPFLAGS"
	AC_CHECK_HEADER(GL/gl.h,,no_gl=yes)
	CPPFLAGS="$save_CPPFLAGS"

	dnl Check for OpenGL libs
	if test x$no_gl = x ; then

		if test x$gl_eprefix != x ; then
			GL_LIBS=-L$gl_eprefix/lib
		fi
		save_LDFLAGS="$LDFLAGS"
		LDFLAGS="$GL_LIBS $LDFLAGS"
		AC_CHECK_LIB(GL, glFinish, :, no_gl=yes)
		LDFLAGS="$save_LDFLAGS"
	fi

	if test x$no_gl = x ; then

		dnl Version check
		gl_major=[`echo $2 | \
			sed 's/\([0-9\]*\)\.\([0-9]*\)/\1/'`]
		gl_minor=[`echo $2 | \
			sed 's/\([0-9]*\)\.\([0-9]*\)/\2/'`]

		save_CPPFLAGS="$CPPFLAGS"
		CPPFLAGS="$GL_INCLUDES $CPPFLAGS"
		AC_MSG_CHECKING(if OpenGL version >= $2 is supported)
		symbol=GL_VERSION_${gl_major}_${gl_minor}
		AC_EGREP_CPP(yes,[
#include <GL/gl.h>
#if $symbol == 1
yes
#endif
			],:,no_gl=yes)
		if test x$no_gl = x ; then
			AC_MSG_RESULT(yes)
		else
			AC_MSG_RESULT(no)
		fi
		
		CPPFLAGS="$save_CPPFLAGS"
	fi

	if test x$no_gl != x ; then
		
		dnl Abort or warn?
		if test x$3 = xfailure ; then
			AC_MSG_ERROR(OpenGL library was not found!)
		else
			AC_MSG_WARN(OpenGL library was not found!)
		fi
	else
		GL_LIBS="$GL_LIBS -lGL -lGLU"
		ifelse($1,,:,$1)
	fi

	AC_SUBST(GL_INCLUDES)
	AC_SUBST(GL_LIBS)

	AC_LANG_RESTORE
])

AC_DEFUN([BERLIN_GL_CHECK], [
	BERLIN_GL_HOOK([],1.2,failure)
])

AC_DEFUN([BERLIN_GGIMESA_CHECK], [

	BERLIN_GGI_CHECK
	BERLIN_GL_CHECK

	save_CPPFLAGS="$CPPFLAGS"
	CPPFLAGS="$GGI_INCLUDES $GL_INCLUDES $CPPFLAGS"
	AC_CHECK_HEADER(GL/ggimesa.h,,AC_MSG_ERROR(GGIMesa header not found!))
	CPPFLAGS="$save_CPPFLAGS"
	
	GL_INCLUDES="$GGI_INCLUDES $GL_INCLUDES"
	GL_LIBS="$GGI_LIBS $GL_LIBS"
])