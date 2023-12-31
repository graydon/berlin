/*$Id: TextKitImpl.hh,v 1.24 2001/04/18 06:07:26 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _TextKitImpl_hh
#define _TextKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/TextKit.hh>           // for our IDL definition
#include <Text/TextChunk.hh>           // for our chunks
#include <Berlin/MonoGraphic.hh>       // for our decorators
#include <Berlin/KitImpl.hh>           // for our parent impl
#include <Berlin/ImplVar.hh>           // for the impls
#include <Babylon/String.hh>           // for Babylon::String
#include <Prague/Sys/Thread.hh>        // for Mutex
#include <map>                         // for the cache
#include <vector>                      // for the gc

class Compositor;
class Strut;

class TextKitImpl : public virtual POA_Warsaw::TextKit,
		    public KitImpl
{
  typedef std::map<Warsaw::Unichar, Warsaw::Graphic_var> cache_t;
 public:
  TextKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~TextKitImpl();
  virtual void bind(Warsaw::ServerContext_ptr);

  Warsaw::Graphic_ptr chunk(const Warsaw::Unistring &u);
  Warsaw::Graphic_ptr glyph(Warsaw::Unichar c);
  Warsaw::Graphic_ptr strut();
  Warsaw::Graphic_ptr simple_viewer(Warsaw::TextBuffer_ptr);  
  Warsaw::Graphic_ptr terminal(Warsaw::StreamBuffer_ptr);
  Warsaw::Graphic_ptr size(Warsaw::Graphic_ptr, CORBA::ULong);
  Warsaw::Graphic_ptr weight(Warsaw::Graphic_ptr, CORBA::ULong);
  Warsaw::Graphic_ptr family(Warsaw::Graphic_ptr, const Warsaw::Unistring &);
  Warsaw::Graphic_ptr subfamily(Warsaw::Graphic_ptr, const Warsaw::Unistring &);
  Warsaw::Graphic_ptr fullname(Warsaw::Graphic_ptr, const Warsaw::Unistring &);
  Warsaw::Graphic_ptr style(Warsaw::Graphic_ptr, const Warsaw::Unistring &);
  Warsaw::Graphic_ptr font_attribute(Warsaw::Graphic_ptr, const Warsaw::NVPair &);
 private:
  cache_t                _cache;
  Warsaw::DrawingKit_var _canonicalDK;

  Warsaw::LayoutKit_var _layout;
  Impl_var<Strut>       _strut;
  Prague::Mutex         _mutex;
  Compositor           *_lineCompositor;
  Compositor           *_pageCompositor;
};

#endif
