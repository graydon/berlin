/*$Id: FontServer.hh,v 1.1 2000/03/20 22:25:05 stefan Exp $
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
#ifndef _GL_FontServer_hh
#define _GL_FontServer_hh

#include <Drawing/openGL/GLFont.hh>
#include <Drawing/FT/Face.hh>
#include <map>

namespace GL
{

inline bool operator == (const Unistring &us1, const Unistring &us2)
{
  if (us1.length() != us2.length()) return false;
  for (const Unichar *uc1 = us1.get_buffer(), *uc2 = us2.get_buffer(); uc1 != us1.get_buffer() + us1.length(); uc1++, uc2++)
    if (*uc1 != *uc2) return false;
  return true;
}

inline bool operator < (const Unistring &us1, const Unistring &us2)
{
  if (us1.length() < us2.length()) return true;
  if (us1.length() > us2.length()) return false;
  for (const Unichar *uc1 = us1.get_buffer(), *uc2 = us2.get_buffer(); uc1 != us1.get_buffer() + us1.length(); uc1++, uc2++)
    if (*uc1 < *uc2) return true;
    else if (*uc1 > *uc2) return false;
  return false;
}

struct my_less
{
  bool operator ()(const Unistring &us1, const Unistring &us2) const { return us1 < us2;}
};

class FontServer
{
  struct key
  {
    key(unsigned long s, unsigned long w, const Unistring &f) : size(s), weight(w), family(f) {}
    bool operator == (const key &k) const { return size == k.size && weight == k.weight && family == k.family;}
    bool operator < (const key &k) const { return size < k.size && weight < k.weight && family < k.family;}
    unsigned long size;
    unsigned long weight;
    Unistring family;
  };
  typedef map<key, GLFont *> fmap_t;
  typedef map<Unistring, FT::Face *, my_less> faces_t;
public:
  FontServer();
  ~FontServer();
  unsigned long size() { return _size;}
  void size(unsigned long s) { lookup(s, _weight, _family);}
  unsigned long weight() { return _weight;}
  void weight(unsigned long w) { lookup(_size, w, _family);}
  const Unistring &family() { return _family;}
  void family(const Unistring &f) { lookup(_size, _weight, f);}
  const Unistring &subfamily() { return _subfamily;}
  void subfamily(const Unistring &) {}
  const Unistring &fullname() { return _fullname;}
  void fullname(const Unistring &) {}
  const Unistring &style() { return _style;}
  void style(const Unistring &) {}
  
  void drawText(const Unistring & text) { _font->drawText(text);}
  void allocateText(const Unistring & s, Graphic::Requisition & req) { _font->allocateText(s, req);}
private:
  void lookup(unsigned long, unsigned long, const Unistring &);
  unsigned long _size;
  unsigned long _weight;
  Unistring _family;
  Unistring _subfamily;
  Unistring _fullname;
  Unistring _style;
  GLFont   *_font;
  fmap_t    fonts;
  faces_t   faces;
};

};

#endif /* _GL_FontServer_hh */
