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
dnl BERLIN_GGI_HOOK(script-if-ggi-found, version, failflag)
dnl
dnl If failflag is "failure", it aborts if GGI is not found.
dnl Version checking is not done at this moment.
dnl

AC_DEFUN([BERLIN_GGI_HOOK],[

	AC_LANG_SAVE
	AC_LANG_C

	AC_ARG_WITH(ggi-prefix,
		[  --with-ggi-prefix=PFX   Prefix for GGI],[
		ggi_prefix="$withval"])
	AC_ARG_WITH(ggi-eprefix,
		[  --with-ggi-eprefix=PFX  Exec prefix for GGI],[
		ggi_eprefix="$withval"],[
		ggi_eprefix=$ggi_prefix])

	dnl Check for GGI includes
	if test x$ggi_prefix != x ; then
		GGI_INCLUDES=-I$ggi_prefix/include
	fi
	save_CPPFLAGS="$CPPFLAGS"
	CPPFLAGS="$GGI_INCLUDES $CPPFLAGS"
	AC_CHECK_HEADER(ggi/ggi.h,:,no_ggi=yes)
	CPPFLAGS="$save_CPPFLAGS"

	dnl Check for GGI libs
	if test x$no_ggi = x ; then

		if test x$ggi_eprefix != x ; then
			GGI_LIBS=-L$ggi_eprefix/lib
		fi

		save_LDFLAGS="$LDFLAGS"
		LDFLAGS="$GGI_LIBS $LDFLAGS"
		AC_CHECK_LIB(ggi, ggiInit, :, no_ggi=yes)
		LDFLAGS="$save_LDFLAGS"
	fi

	if test x$no_ggi != x ; then

		dnl Abort or warn?
		if test x$3 = xfailure ; then
			AC_MSG_ERROR(GGI library was not found!)
		else
			AC_MSG_WARN(GGI library was not found!)
		fi
	else
		GGI_LIBS="$GGI_LIBS -lggi"
		ifelse($1,,:,$1)
	fi

	AC_SUBST(GGI_INCLUDES)
	AC_SUBST(GGI_LIBS)

	AC_LANG_RESTORE
])

AC_DEFUN([BERLIN_GGI_CHECK], [
	BERLIN_GGI_HOOK([],2.0,failure)
])
