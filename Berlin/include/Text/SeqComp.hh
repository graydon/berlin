#ifndef _SeqComp_hh
#define _SeqComp_hh
//
// $Id: SeqComp.hh,v 1.1 1999/06/06 05:01:59 gray Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//

// this is a helper template you can use if you want to compare 2 of omniorb's sequence templates.
// it might need a little tweakin' to work with other ORBs.

#include <omniORB2/CORBA.h>
#include <omniORB2/seqtemplates.h>

template <class T> inline bool SeqComp(const _CORBA_Sequence<T> &a, const _CORBA_Sequence<T> &b) {
    unsigned long len1 = a.length();
    unsigned long len2 = b.length();
    unsigned long len = len1 > len2 ? len2 : len1;	
    for (unsigned long i = 0; i < len; i++) {
	if (a[i] != b[i]) return a[i] > b[i];
    }
    return false;
}

#endif
