/*$Id: ImageImpl.hh,v 1.6 2000/09/19 21:11:04 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
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

#ifndef _ImageImpl_hh
#define _ImageImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Image.hh>
#include <Warsaw/Raster.hh>
#include <Berlin/GraphicImpl.hh>
#include <Berlin/ViewImpl.hh>
#include <Berlin/MonoGraphic.hh>
#include <Berlin/RefCountVar.hh>

class ImageImpl : public virtual POA_Warsaw::Image,
		  public virtual ViewImpl,
		  public GraphicImpl
{
public:
  ImageImpl(Warsaw::Raster_ptr);
  ~ImageImpl();
  
  virtual Warsaw::Raster_ptr data() { return Warsaw::Raster::_duplicate(raster);}
  virtual void data(Warsaw::Raster_ptr r) { raster = r;}

  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void draw(Warsaw::DrawTraversal_ptr); 
  virtual void update(const CORBA::Any &);
protected:
  virtual void activate_composite();
private:
  RefCount_var<Warsaw::Raster> raster;
  Warsaw::Coord width, height;
};

class Texture : public MonoGraphic
{
public:
  Texture(Warsaw::Raster_ptr);
  ~Texture();
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);
private:
  RefCount_var<Warsaw::Raster> raster;
};

#endif
