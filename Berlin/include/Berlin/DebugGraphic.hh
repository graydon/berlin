/*$Id: DebugGraphic.hh,v 1.10 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _DebugGraphic_hh
#define _DebugGraphic_hh

#include "Berlin/MonoGraphic.hh"
#include <Warsaw/Region.hh>
#include <iostream>
#include <string>

class DebugGraphic : public MonoGraphic
{
public:
  enum Flags {none = 0x0, requests = 0x1, draws = 0x2, picks = 0x4, traversals = 0x7};
  DebugGraphic(std::ostream &, const std::string &, unsigned int f = traversals);
  virtual ~DebugGraphic();

  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);
  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);
private:
  void         heading(const char *);
  std::ostream &_os;
  std::string   _message;
  unsigned int  _flags;
};

#endif 
