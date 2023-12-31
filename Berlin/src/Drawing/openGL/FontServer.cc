/*$Id: FontServer.cc,v 1.1 2000/03/20 22:25:35 stefan Exp $
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

#include <Drawing/openGL/FontServer.hh>
#include <Drawing/openGL/GLUnifont.hh>
#include <Drawing/openGL/GLPixmapFont.hh>

using namespace Prague;

namespace GL
{

FontServer::FontServer()
{
  _size = 16;
  _weight = 100;
  _font = new GLUnifont;
  fonts[key(16, 100, Unicode::toCORBA(Unicode::String("GNU Unifont")))] = _font;
//   char *env = getenv("BERLIN_ROOT");
//   if (!env)
//     {
//       cerr << "Please set environment variable BERLIN_ROOT first" << endl;
//       exit(-1);
//     }
//   string ttf = string(env) + "/etc/Fonts/helv.ttf";
//   FT::Face *face = new FT::Face(ttf.c_str());
//   faces[Unicode::toCORBA(Unicode::String("Helvetica"))] = face;
//   _font = new GLPixmapFont(*face, 10);
//   fonts[key(16, 100, Unicode::toCORBA(Unicode::String("Helvetica")))] = _font;
}

FontServer::~FontServer()
{
  for (fmap_t::iterator i = fonts.begin(); i != fonts.end(); i++) delete (*i).second;
}

void FontServer::lookup(unsigned long, unsigned long, const Unistring &)
{
}

};
