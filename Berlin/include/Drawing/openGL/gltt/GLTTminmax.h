/*
 * gltt graphics library
 * Copyright (C) 1998-1999 Stephane Rehel
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
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef __GLTTminmax_h
#define __GLTTminmax_h

/////////////////////////////////////////////////////////////////////////////

// Reported to be necessary by <da@skivs.ski.org>
// Feb 8 1998
#ifdef min
#undef min
#endif

inline double min( double d1, double d2 )
{
  return (d1<d2) ? d1 : d2;
}

inline double min( double d1, double d2, double d3 )
{
  return min( min(d1,d2), d3 );
}

/////////////////////////////////////////////////////////////////////////////

// Reported to be necessary by <da@skivs.ski.org>
// Feb 8 1998
#ifdef max
#undef max
#endif

inline double max( double d1, double d2 )
{
  return (d1>d2) ? d1 : d2;
}

inline double max( double d1, double d2, double d3 )
{
  return max( max(d1,d2), d3 );
}

/////////////////////////////////////////////////////////////////////////////

#endif // ifdef __GLTTminmax_h
