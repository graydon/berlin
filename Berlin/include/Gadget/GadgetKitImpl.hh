/*$Id: GadgetKitImpl.hh,v 1.6 2000/08/31 18:52:32 stefan Exp $
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
#ifndef _GadgetKitImpl_hh
#define _GadgetKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/GadgetKit.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/FigureKit.hh>
#include <Berlin/KitImpl.hh>
#include <vector>

class GraphicImpl;

class GadgetKitImpl : public virtual POA_Warsaw::GadgetKit,
		      public KitImpl
{
 public:
  GadgetKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~GadgetKitImpl();
  virtual void bind(Warsaw::ServerContext_ptr);
  virtual Warsaw::Graphic_ptr rgb(Warsaw::Graphic_ptr, Warsaw::BoundedValue_ptr, Warsaw::BoundedValue_ptr, Warsaw::BoundedValue_ptr);
  virtual Warsaw::Graphic_ptr alpha(Warsaw::Graphic_ptr, Warsaw::BoundedValue_ptr);
  virtual Warsaw::Graphic_ptr lighting(Warsaw::Graphic_ptr, Warsaw::BoundedValue_ptr, Warsaw::BoundedValue_ptr, Warsaw::BoundedValue_ptr);
  virtual Warsaw::Graphic_ptr rotator(Warsaw::Graphic_ptr, Warsaw::BoundedValue_ptr, Warsaw::Axis);
  virtual Warsaw::Graphic_ptr zoomer(Warsaw::Graphic_ptr, Warsaw::BoundedValue_ptr);
 private:
  Warsaw::CommandKit_var command;
  Warsaw::FigureKit_var figure;
};

#endif
