/*$Id: VisualImpl.hh,v 1.3 2001/04/18 06:07:26 stefan Exp $
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
#ifndef _VisualImpl_hh
#define _VisualImpl_hh

#include <Prague/Sys/Thread.hh>
#include <Warsaw/config.hh>
#include <Warsaw/GGIKit.hh>
#include <Berlin/ControllerImpl.hh>
#include <Berlin/Console.hh>

class VisualImpl : public virtual POA_GGI::Visual,
                   public ControllerImpl
{
public:
  VisualImpl(Warsaw::PixelCoord, Warsaw::PixelCoord);
  virtual ~VisualImpl();
  virtual char *name();
  virtual char *mode();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void extension(const Warsaw::Allocation::Info &info, Warsaw::Region_ptr region);
  virtual CORBA::Boolean handle_positional(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
  virtual CORBA::Boolean handle_non_positional(const Warsaw::Input::Event &);
private:
  void forward_event(const ggi_event &);
  Warsaw::PixelCoord   _width;
  Warsaw::PixelCoord   _height;
  Warsaw::Drawable_var _drawable;
  int                  _shm;
  GGIDrawable         *_ggi;
  std::string          _name;
  std::string          _mode;
};

#endif
