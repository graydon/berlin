/*$Id: Warsaw.hh,v 1.1 1999/10/27 15:50:32 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
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
#ifndef _Warsaw_hh
#define _Warsaw_hh

#include <Graphic.hh>
#include <Region.hh>
#include <iostream>

/*
 * here I start to put some utilities which are based only on top of
 * Warsaw types, so this stuff should be distributed with the server
 * *and* the client side... -stefan
 */

ostream &operator << (ostream &, const Graphic::Requirement &);
ostream &operator << (ostream &, const Graphic::Requisition &);
ostream &operator << (ostream &, const Region::Allotment &);
ostream &operator << (ostream &, Region_ptr);

#endif /* _Warsaw_hh */
