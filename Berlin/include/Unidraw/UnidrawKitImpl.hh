/*$Id: UnidrawKitImpl.hh,v 1.4 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _UnidrawKitImpl_hh
#define _UnidrawKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/FigureKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/WidgetKit.hh>
#include <Warsaw/UnidrawKit.hh>
#include <Berlin/KitImpl.hh>
#include <Berlin/RefCountVar.hh>
#include <vector>

class UnidrawKitImpl : public virtual POA_Unidraw::UnidrawKit,
		       public KitImpl
{
public:
  UnidrawKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~UnidrawKitImpl();
  virtual void bind(Warsaw::ServerContext_ptr);
  virtual Unidraw::Tool_ptr select_tool();
  virtual Unidraw::Editor_ptr create_editor();
  virtual Unidraw::View_ptr   create_view(Warsaw::Graphic_ptr g, Unidraw::Model_ptr);
  Warsaw::FigureKit_ptr figures();
  Warsaw::ToolKit_ptr tools();
  Warsaw::WidgetKit_ptr widgets();
private:
  RefCount_var<Warsaw::FigureKit> _figures;
  RefCount_var<Warsaw::ToolKit>   _tools;
  RefCount_var<Warsaw::WidgetKit> _widgets;
};

#endif
