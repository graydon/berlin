/*$Id: TransformatorImpl.cc,v 1.4 1999/10/26 20:51:11 gray Exp $
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

#include "Warsaw/config.hh"
#include "Warsaw/Region.hh"
#include "Warsaw/Traversal.hh"
#include "Figure/TransformatorImpl.hh"

TransformatorImpl::TransformatorImpl()
{
  transform = new TransformImpl;
  transform->_obj_is_ready(_boa());
}

TransformatorImpl::~TransformatorImpl()
{
  transform->_dispose();
}

void TransformatorImpl::transformation(Transform_ptr t)
{
  transform->copy(t);
  needResize();
}

Transform_ptr TransformatorImpl::transformation()
{
  return transform->_this();
}

void TransformatorImpl::request(Requisition &requisition)
{
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  child->request(requisition);
  GraphicImpl::transformRequest(requisition, Transform_var(transform->_this()));
}

void TransformatorImpl::traverse(Traversal_ptr traversal)
{
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  traversal->traverseChild(child, 0, Region::_nil(), Transform_var(transform->_this()));
}

void TransformatorImpl::allocate(Tag, const Allocation::Info &info)
{
  info.transformation->premultiply(Transform_var(transform->_this()));
}
