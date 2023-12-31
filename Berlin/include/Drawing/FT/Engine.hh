/*$Id: Engine.hh,v 1.2 1999/12/28 22:47:06 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _FT_Engine_hh
#define _FT_Engine_hh

#include <freetype/freetype.h>
#include <exception>

namespace FT
{

class Engine : public TT_Engine
{
  struct Cleaner { ~Cleaner() { delete Engine::engine;}};
  friend class Cleaner;
public:
  static Engine *instance() { if (!engine) engine = new Engine; return engine;}
private:
  Engine() { if (TT_Init_FreeType(this)) throw exception();}
  Engine(const Engine &);
  ~Engine() { TT_Done_FreeType(*this);}
  static Engine *engine;
  static Cleaner cleaner;
};

};

#endif /* _FT_Engine_hh */
