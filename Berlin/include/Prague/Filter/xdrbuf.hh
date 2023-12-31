/*$Id: xdrbuf.hh,v 1.2 1999/04/27 20:11:10 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on binio from Dietmar Kuehl:
 *
 * Copyright (C) 1996 Dietmar Kuehl
 * Universitaet Konstanz, Lehrstuhl fuer praktische Informatik I
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */
#ifndef _xdrbuf_h
#define _xdrbuf_h

#include <streambuf.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>

namespace Prague
{

extern void xdrbuf_create(XDR *xdrs, streambuf *sb, xdr_op op);
extern void xdrbuf_reseat(XDR *xdrs, streambuf *sb);

};

#endif /* xdrbuf_h */
