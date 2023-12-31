/*$Id: ToolKitImpl.hh,v 1.11 2000/11/14 21:25:23 stefan Exp $
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
#ifndef _ToolKitImpl_hh
#define _ToolKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/State.hh>
#include <Berlin/KitImpl.hh>
#include <vector>

class GraphicImpl;

class ToolKitImpl : public virtual POA_Warsaw::ToolKit,
		    public KitImpl
{
 public:
  ToolKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~ToolKitImpl();

  Warsaw::Graphic_ptr      debugger(Warsaw::Graphic_ptr, const char *);
  Warsaw::DrawingState_ptr decorator(Warsaw::Graphic_ptr);
  Warsaw::Graphic_ptr      rgb(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  Warsaw::Graphic_ptr      alpha(Warsaw::Graphic_ptr, Warsaw::Coord);
  Warsaw::Graphic_ptr      lighting(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);

  Warsaw::Graphic_ptr      frame(Warsaw::Graphic_ptr, Warsaw::Coord, const Warsaw::ToolKit::FrameSpec &, CORBA::Boolean);
  Warsaw::Graphic_ptr      dynamic(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Telltale::Mask, const Warsaw::ToolKit::FrameSpec &,
			   const Warsaw::ToolKit::FrameSpec &, CORBA::Boolean, Warsaw::Telltale_ptr);
  Warsaw::Graphic_ptr      framed_triangle(Warsaw::Graphic_ptr, Warsaw::Coord, const Warsaw::ToolKit::FrameSpec &, CORBA::Boolean,
					   Warsaw::ToolKit::Direction d);
  Warsaw::Graphic_ptr      dynamic_triangle(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Telltale::Mask, const Warsaw::ToolKit::FrameSpec &,
					    const Warsaw::ToolKit::FrameSpec &, CORBA::Boolean, Warsaw::ToolKit::Direction d,
					    Warsaw::Telltale_ptr);
  Warsaw::Graphic_ptr      framed_diamond(Warsaw::Graphic_ptr, Warsaw::Coord, const Warsaw::ToolKit::FrameSpec &, CORBA::Boolean);
  Warsaw::Graphic_ptr      dynamic_diamond(Warsaw::Graphic_ptr, Warsaw::Coord, Warsaw::Telltale::Mask, const Warsaw::ToolKit::FrameSpec &,
				           const Warsaw::ToolKit::FrameSpec &, CORBA::Boolean, Warsaw::Telltale_ptr);
//   Graphic_ptr      filler(Graphic_ptr, const Color &);
//   Graphic_ptr      indicator(Graphic_ptr, const Color &, Telltale_ptr);
  Warsaw::Trigger_ptr      button(Warsaw::Graphic_ptr, Warsaw::Command_ptr);
  Warsaw::Controller_ptr   dragger(Warsaw::Graphic_ptr, Warsaw::Command_ptr);
  Warsaw::Controller_ptr   stepper(Warsaw::Graphic_ptr, Warsaw::Command_ptr);
  Warsaw::Controller_ptr   text_input(Warsaw::Graphic_ptr, Warsaw::TextBuffer_ptr);
  Warsaw::Controller_ptr   terminal(Warsaw::Graphic_ptr, Warsaw::StreamBuffer_ptr);
  Warsaw::Controller_ptr   group(Warsaw::Graphic_ptr);
  Warsaw::Controller_ptr   toggle(Warsaw::Graphic_ptr);
  Warsaw::Canvas_ptr       create_canvas(Warsaw::PixelCoord, Warsaw::PixelCoord);
};

#endif
