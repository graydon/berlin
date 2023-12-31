/*$Id: Beveler.hh,v 1.3 2000/08/31 18:52:32 stefan Exp $
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
#ifndef _Beveler_hh
#define _Beveler_hh

#include <Warsaw/config.hh>
#include <Warsaw/DrawTraversal.hh>

namespace Beveler
{
  void rect(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
	    const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
	    Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
  void leftArrow(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
		 const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
		 Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
  void rightArrow(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
		  const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
		  Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
  void upArrow(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
	       const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
	       Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
  void downArrow(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
		 const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
		 Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
  void diamond(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
	       const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
	       Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
  void circle(Warsaw::DrawTraversal_ptr, Warsaw::Coord,
	      const Warsaw::Color &, const Warsaw::Color &, const Warsaw::Color&,
	      Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, bool);
};

#endif
