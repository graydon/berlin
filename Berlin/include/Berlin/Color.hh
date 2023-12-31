/*$Id: Color.hh,v 1.7 2000/08/31 18:52:31 stefan Exp $
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
#ifndef _Color_hh
#define _Color_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <iostream>

inline Warsaw::Color brightness(const Warsaw::Color &c1, double adjust)
{
  Warsaw::Color c2;
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

inline void CMYtoRGB(Warsaw::Coord cyan, Warsaw::Coord magenta, Warsaw::Coord yellow, Warsaw::Color &color)
{
  color.red = 1. - cyan;
  color.green = 1. - magenta;
  color.blue = 1. - yellow;
}

inline void RGBtoHSV(const Warsaw::Color &color, Warsaw::Coord &hue, Warsaw::Coord &saturation, Warsaw::Coord &value)
{
  Warsaw::Coord max = color.red > color.green ? (color.red > color.blue ? color.red : color.blue) : color.green > color.blue ? color.green : color.blue;
  Warsaw::Coord min = color.red < color.green ? (color.red < color.blue ? color.red : color.blue) : color.green < color.blue ? color.green : color.blue;
  hue = max;
  saturation = max != 0. ? (max - min) / max : 0.; 
  if (saturation == 0.) hue = 0.; // undefined...
  else
   {
     double delta = max - min;
     if (color.red == max) hue = (color.green - color.blue) / delta;
     else if (color.green == max) hue = 2. + (color.blue - color.red) / delta;
     else if (color.blue == max) hue = 4. + (color.red - color.green) / delta;
     hue *= 60.;
     if (hue < 0.) hue += 360.;
   }
}

inline void HSVtoRGB(Warsaw::Coord hue, Warsaw::Coord saturation, Warsaw::Coord value, Warsaw::Color &color)
{
  if (saturation == 0.) color.red = color.green = color.blue = value;
  else
    {
      if (hue == 360.) hue = 0.;
      hue /= 60.;
      int i = static_cast<int>(hue);
      Warsaw::Coord f = hue - i;
      Warsaw::Coord p = value * (1. - saturation);
      Warsaw::Coord q = value * (1. - saturation * f);
      Warsaw::Coord t = value * (1. - saturation * (1. - f));
      switch (i)
        {
          case 0: color.red = value; color.green = t; color.blue = p; break;
          case 1: color.red = q; color.green = value; color.blue = p; break;
          case 2: color.red = p; color.green = value; color.blue = t; break;
          case 3: color.red = p; color.green = q; color.blue = value; break;
          case 4: color.red = t; color.green = p; color.blue = value; break;
          case 5: color.red = value; color.green = p; color.blue = q; break;
        }
    }
}

#endif /* _Color_hh */
