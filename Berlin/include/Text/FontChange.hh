#ifndef _FontChange_hh
#define _FontChange_hh
//
// $Id: FontChange.hh,v 1.2 1999/12/22 16:53:39 stefan Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//

#include "Warsaw/config.hh"
#include "Warsaw/Text.hh"
#include "Berlin/GraphicImpl.hh"

class FontChange : public virtual GraphicImpl {
public:
//   FontChange(const Text::FontDescriptor & f, const Style::Spec &sty);	    
  virtual void draw(DrawTraversal_ptr dt);
  virtual void request(Requisition &);

protected:
  const Text::FontDescriptor myFontDescriptor;
//   const Style::Spec myStyle;
};


#endif

