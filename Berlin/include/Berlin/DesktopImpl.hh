/*$Id: DesktopImpl.hh,v 1.9 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _DesktopImpl_hh
#define _DesktopImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Desktop.hh>
#include <Berlin/ControllerImpl.hh>
#include <Berlin/RefCountVar.hh>

class WindowImpl;

class DesktopImpl : public virtual POA_Warsaw::Desktop,
                    public ControllerImpl
{
public:
  DesktopImpl(Layout::Stage_ptr);
  virtual ~DesktopImpl();
  virtual void body(Warsaw::Graphic_ptr);
  virtual Warsaw::Graphic_ptr body();
  virtual Warsaw::Region_ptr bbox();
  virtual CORBA::Long layers();
  virtual Layout::StageHandle_ptr layer(Layout::Stage::Index);
  virtual void begin();
  virtual void end();
  virtual Layout::StageHandle_ptr insert(Warsaw::Graphic_ptr, const Warsaw::Vertex &, const Warsaw::Vertex &, Layout::Stage::Index);
protected:
  virtual void key_press(const Warsaw::Input::Event &); 
private:
  RefCount_var<Layout::Stage> _stage;
};

#endif 
