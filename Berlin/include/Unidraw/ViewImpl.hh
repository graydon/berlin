/*$Id: ViewImpl.hh,v 1.1 2000/12/21 21:06:48 stefan Exp $
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
#ifndef _UViewImpl_hh
#define _UViewImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/UnidrawKit.hh>
#include <Berlin/ControllerImpl.hh>

class UViewImpl : public virtual POA_Unidraw::View,
		  public ControllerImpl
{
public:
  UViewImpl(Unidraw::Model_ptr);
  virtual ~UViewImpl();
  virtual Unidraw::Model_ptr subject();
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);
  virtual CORBA::Boolean handle_positional(Warsaw::PickTraversal_ptr, const Warsaw::Input::Event &);
private:
  Unidraw::Model_var _model;
};

#endif
