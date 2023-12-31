/*$Id: IO.hh,v 1.5 2001/04/18 06:07:26 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Warsaw_IO_hh
#define _Warsaw_IO_hh

#include <Warsaw/config.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/Transform.hh>
#include <iostream>

inline std::ostream &operator << (std::ostream &os, const Warsaw::Vertex &v) { return os << '(' << v.x << ',' << v.y << ',' << v.z << ')';}
std::ostream &operator << (std::ostream &, const Warsaw::Color &);
std::ostream &operator << (std::ostream &, const Warsaw::Graphic::Requirement &);
std::ostream &operator << (std::ostream &, const Warsaw::Graphic::Requisition &);
std::ostream &operator << (std::ostream &, const Warsaw::Region::Allotment &);
std::ostream &operator << (std::ostream &, Warsaw::Region_ptr);
std::ostream &operator << (std::ostream &, const Warsaw::Transform::Matrix &);
std::ostream &operator << (std::ostream &, Warsaw::Transform_ptr);

#endif
