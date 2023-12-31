/*$Id: CanvasImpl.cc,v 1.3 2001/03/03 00:09:54 tobias Exp $
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

#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/SHM.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/DrawingKit.hh>
#include <Berlin/Console.hh>
#include <Berlin/Logger.hh>
#include "Tool/CanvasImpl.hh"
#include <sys/ipc.h>
#include <strstream.h>

#if !defined(CONSOLE_GGI)
#warning "CanvasImpl is currently only available for the GGI console"
#else

using namespace Prague;
using namespace Warsaw;

CanvasImpl::CanvasImpl(PixelCoord w, PixelCoord h)
  : _width(w), _height(h)
{
  Trace trace("CanvasImpl::CanvasImpl");
  Console::Drawable::PixelFormat format = Console::drawable()->pixel_format();
  /*
   * the drawable plus some memory for the event queue
   */
  size_t size = w * h * format.size + 64*1024;
  _shm = SHM::allocate(size);
  ostrstream oss;
  oss << "display-memory:-input:shmid:" << _shm << ends;
  const char *name = oss.str();
  GGIDrawable *ggi = 0; 
  try { ggi = new GGIDrawable(name, w, h, 3);}
  catch (...)
    {
      cerr << "Error : can't open shm GGIDrawable" << endl;
      throw;
    }
  Logger::log(Logger::drawing) << "open ggi display with name :'" << name << '\'' << endl;
  delete [] name;
  Console::Drawable *drawable = Console::create_drawable(ggi);
  _drawable = Console::activate_drawable(drawable);
}

CanvasImpl::~CanvasImpl()
{
  Trace trace("CanvasImpl::~CanvasImpl");
  SHM::deallocate(_shm);
}

CORBA::Long CanvasImpl::shm_id()
{
  return _shm;
}

Warsaw::Canvas::PixelFormat CanvasImpl::pixel_format()
{
  return _drawable->pixel_format();
}

Warsaw::Canvas::BufferFormat CanvasImpl::buffer_format()
{
  return _drawable->buffer_format();
}

void CanvasImpl::lock()
{
  _mutex.lock();
}

void CanvasImpl::unlock()
{
  _mutex.unlock();
}

void CanvasImpl::request(Warsaw::Graphic::Requisition &requisition)
{
  Trace trace("CanvasImpl::request");
  requisition.x.defined = true;
  requisition.x.natural = requisition.x.maximum = requisition.x.minimum = _width * 10;// / _drawable->resolution(xaxis);
  requisition.x.align = 0.;
  requisition.y.defined = true;
  requisition.y.natural = requisition.y.maximum = requisition.y.minimum = _height * 10;// / _drawable->resolution(yaxis);
  requisition.y.align = 0.;
  requisition.z.defined = false;
}

void CanvasImpl::draw(DrawTraversal_ptr traversal)
{
  Trace trace("CanvasImpl::draw");
  DrawingKit_var drawing = traversal->drawing();
  drawing->copy_drawable(_drawable, 0, 0, _width, _height);
}

#endif
