/*$Id: TerminalView.hh,v 1.7 2001/04/18 06:07:26 stefan Exp $
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
#ifndef _TerminalView_hh
#define _TerminalView_hh

#include <Warsaw/config.hh>
#include <Warsaw/View.hh>
#include <Warsaw/TextKit.hh>
#include <Berlin/ViewImpl.hh>
#include "Text/Composition.hh"
#include <map>
#include <vector>

class TerminalView : public virtual ViewImpl,
		     public Composition
{
  typedef std::vector<Composition *> lines_t;
 public:
  TerminalView(Warsaw::StreamBuffer_ptr, Warsaw::TextKit_ptr, Warsaw::DrawingKit_ptr, Compositor *, Compositor *);
  virtual ~TerminalView();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void need_resize();
  virtual void update(const CORBA::Any &);
 protected:
  void begin();
  void end();
  Warsaw::StreamBuffer_ptr _stream;
  Warsaw::TextKit_var      _kit;
  Warsaw::DrawingKit_var   _canonicalDK;
  Compositor              *_compositor;
  lines_t                  _lines;
  bool                     _locked;
};

#endif
