/*$Id: Color.hh,v 1.4 1999/10/19 21:07:51 gray Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _Color_hh
#define _Color_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <iostream>

inline Color brightness(const Color &c1, double adjust)
{
  Color c2;
  if (adjust >= 0)
    {
      c2.red   = c1.red + (1 - c1.red) * adjust;
      c2.green = c1.green + (1 - c1.green) * adjust;
      c2.blue  = c1.blue + (1 - c1.blue) * adjust;
    }
  else
    {
      c2.red   = c1.red * (1 + adjust);
      c2.green = c1.green * (1 + adjust);
      c2.blue  = c1.blue * (1 + adjust);
    }
  c2.alpha = c1.alpha;
  return c2;
};

inline void CMYtoRGB(Coord cyan, Coord magenta, Coord yellow, Color &color)
{
  color.red = 1. - cyan;
  color.green = 1. - magenta;
  color.blue = 1. - yellow;
}

inline void HSVtoRGB(Coord hue, Coord saturation, Coord value, Color &color)
{
//   unsigned short i, f;
//   unsigned short p, q, t;
//   unsigned short r = 0, g = 0, b = 0;
//   s = (s * 0xff) / 100;
//   v = (v * 0xff) / 100;
//   if (h == 360) h = 0;
//   if (s == 0) { h = 0; r = g = b = v;}
//   i = h / 60;
//   f = h % 60;
//   p = v * (0xff - s) / 0xff;
//   q = v * (0xff - s * f / 60) / 0xff;
//   t = v * (0xff - s * (60 - f) / 60) / 0xff;
//   switch (i) 
//     {
//     case 0: r = v, g = t, b = p; break;
//     case 1: r = q, g = v, b = p; break;
//     case 2: r = p, g = v, b = t; break;
//     case 3: r = p, g = q, b = v; break;
//     case 4: r = t, g = p, b = v; break;
//     case 5: r = v, g = p, b = q; break;
//     }
//   setRGB(r, g, b);
}

inline ostream &operator << (ostream &os, const Color &c)
{
  if (c.alpha != 1.) os << '(' << c.red << ',' << c.green << ',' << c.blue << ';' << c.alpha << ')';
  else os << '(' << c.red << ',' << c.green << ',' << c.blue << ')';
  return os;
}

#endif /* _Color_hh */
