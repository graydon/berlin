/*$Id: GGIKitImpl.cc,v 1.2 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "GGI/GGIKitImpl.hh"
#include "GGI/VisualImpl.hh"

using namespace Warsaw;

GGIKitImpl::GGIKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
GGIKitImpl::~GGIKitImpl() {}

GGI::Visual_ptr GGIKitImpl::create_visual(Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{
  VisualImpl *visual = new VisualImpl(w, h);
  activate(visual);
  return visual->_this();
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "GGIKitImpl"};
  return new KitFactoryImpl<GGIKitImpl>("IDL:GGI/GGIKit:1.0", properties, 1);
} 
