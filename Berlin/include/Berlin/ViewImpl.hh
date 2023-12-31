/*$Id: ViewImpl.hh,v 1.1 2000/09/12 19:25:59 stefan Exp $
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
#ifndef _ViewImpl_hh
#define _ViewImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/View.hh>
#include <Berlin/ObserverImpl.hh>

class ViewImpl : public virtual POA_Warsaw::View,
		 public virtual ObserverImpl
{
public:
  virtual void destroy() {}
};

#endif 
