/*$Id: CanvasImpl.hh,v 1.2 2001/03/03 00:09:54 tobias Exp $
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
#ifndef _CanvasImpl_hh
#define _CanvasImpl_hh

#include <Prague/Sys/Thread.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Canvas.hh>
#include <Berlin/GraphicImpl.hh>
#include <Berlin/Console.hh>

class CanvasImpl : public virtual POA_Warsaw::Canvas,
                   public GraphicImpl
{
public:
#ifdef CONSOLE_GGI
    CanvasImpl(Warsaw::PixelCoord, Warsaw::PixelCoord);
    virtual ~CanvasImpl();
    virtual CORBA::Long shm_id();
    virtual Warsaw::Canvas::PixelFormat pixel_format();
    virtual Warsaw::Canvas::BufferFormat buffer_format();
    virtual void lock();
    virtual void unlock();
    
    virtual void request(Warsaw::Graphic::Requisition &);
    virtual void draw(Warsaw::DrawTraversal_ptr);
private:
    Warsaw::PixelCoord   _width;
    Warsaw::PixelCoord   _height;
    Warsaw::Drawable_var _drawable;
    Prague::Mutex        _mutex;
    CORBA::Long          _shm;
#else
    CanvasImpl(Warsaw::PixelCoord, Warsaw::PixelCoord) {}
    virtual ~CanvasImpl() {}
    virtual CORBA::Long shm_id() {}
    virtual Warsaw::Canvas::PixelFormat pixel_format() {}
    virtual Warsaw::Canvas::BufferFormat buffer_format() {}
    virtual void lock() {}
    virtual void unlock() {}

    virtual void request(Warsaw::Graphic::Requisition &) {}
    virtual void draw(Warsaw::DrawTraversal_ptr) {}
#endif
};

#endif
