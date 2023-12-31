/*$Id: DrawingKitBase.hh,v 1.24 2001/04/18 06:07:25 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#ifndef _DrawingKitBase_hh
#define _DrawingKitBase_hh

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/DrawingKit3D.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/Raster.hh>
/*
 * bitset is buggy. Sorry, try again later...
 */
// #include <bitset>
#include <stack>

class DrawingKitBase : public virtual POA_Warsaw::DrawingKit
{
  enum gstate
  {
    st_trafo, 
    st_clip,
    st_fg_color,
    st_lt_color, 
    st_point_size, 
    st_line_width,
    st_line_end_style, 
    st_surface_fill_style,
    st_texture, 
    st_font_size, 
    st_font_weight,
    st_font_family, 
    st_font_subfamily,
    st_font_fullname,
    st_font_style, 
    st_font_attr,
    st_last
  };

  struct DrawState
  {
    DrawState() : flags(0) {}
//     bitset<st_last> flags;
    unsigned long flags;
    Warsaw::Transform_var saved_trafo;
    Warsaw::Region_var saved_clip;
    Warsaw::Color saved_fg_color;
    Warsaw::Color saved_lt_color;
    Warsaw::Coord saved_point_size;
    Warsaw::Coord saved_line_width;
    Warsaw::DrawingKit::Endstyle saved_line_end_style;
    Warsaw::DrawingKit::Fillstyle saved_surface_fill_style;
    Warsaw::Raster_var saved_texture;
    CORBA::ULong saved_font_size;
    CORBA::ULong saved_font_weight;
    Warsaw::Unistring_var saved_font_family;
    Warsaw::Unistring_var saved_font_subfamily;
    Warsaw::Unistring_var saved_font_fullname;
    Warsaw::Unistring_var saved_font_style;
    // something here...
    // for holding NVPair saved_font_attr;
  };
 public:
  virtual void save()
  {
    Prague::Trace trace("DrawingKitBase::save");
    states.push(DrawState());
  }
  virtual void restore();
  //######################################################
  //############### subclass signatures ##################
  //######################################################

  virtual Warsaw::Transform_ptr transformation() = 0;
  virtual void transformation(Warsaw::Transform_ptr);
  virtual Warsaw::Region_ptr clipping() = 0;
  virtual void clipping(Warsaw::Region_ptr);
  virtual Warsaw::Color foreground() = 0;
  virtual void foreground(const Warsaw::Color &);
  virtual Warsaw::Color lighting() = 0;
  virtual void lighting(const Warsaw::Color &);
  virtual Warsaw::Coord point_size() = 0;
  virtual void point_size(Warsaw::Coord);
  virtual Warsaw::Coord line_width() = 0;
  virtual void line_width(Warsaw::Coord);
  virtual Warsaw::DrawingKit::Endstyle line_endstyle() = 0;
  virtual void line_endstyle(Warsaw::DrawingKit::Endstyle);
  virtual Warsaw::DrawingKit::Fillstyle surface_fillstyle() = 0;
  virtual void surface_fillstyle(Warsaw::DrawingKit::Fillstyle);
  virtual Warsaw::Raster_ptr texture() = 0;
  virtual void texture(Warsaw::Raster_ptr);

  virtual CORBA::ULong font_size() = 0;
  virtual void font_size(CORBA::ULong);
  virtual CORBA::ULong font_weight() = 0;
  virtual void font_weight(CORBA::ULong);
  virtual Warsaw::Unistring *font_family() = 0;
  virtual void font_family(const Warsaw::Unistring &);
  virtual Warsaw::Unistring *font_subfamily() = 0;
  virtual void font_subfamily(const Warsaw::Unistring &);
  virtual Warsaw::Unistring *font_fullname() = 0;
  virtual void font_fullname(const Warsaw::Unistring &);
  virtual Warsaw::Unistring *font_style() = 0;
  virtual void font_style(const Warsaw::Unistring &);
  virtual Warsaw::DrawingKit::FontMetrics font_metrics() = 0;
  virtual Warsaw::DrawingKit::GlyphMetrics glyph_metrics(Warsaw::Unichar) = 0;
  virtual CORBA::Any *get_font_attribute(const Warsaw::Unistring &) = 0;
  virtual void font_attribute(const Warsaw::NVPair &);

  virtual void init() { }
  virtual void finish() { }
  virtual void flush() = 0;

  virtual Warsaw::DrawingKit3D_ptr dk3D() { return Warsaw::DrawingKit3D::_nil(); }

  virtual void set_transformation (Warsaw::Transform_ptr) = 0;
  virtual void set_clipping(Warsaw::Region_ptr) = 0;
  virtual void set_foreground(const Warsaw::Color &) = 0;
  virtual void set_lighting(const Warsaw::Color &) = 0;
  virtual void set_point_size(Warsaw::Coord) = 0;
  virtual void set_line_width(Warsaw::Coord) = 0;
  virtual void set_line_endstyle(Warsaw::DrawingKit::Endstyle) = 0;
  virtual void set_surface_fillstyle(Warsaw::DrawingKit::Fillstyle) = 0;
  virtual void set_texture(Warsaw::Raster_ptr) = 0;

  virtual void set_font_size(CORBA::ULong) = 0;
  virtual void set_font_weight(CORBA::ULong) = 0;
  virtual void set_font_family(const Warsaw::Unistring &) = 0;
  virtual void set_font_subfamily(const Warsaw::Unistring &) = 0;
  virtual void set_font_fullname(const Warsaw::Unistring &) = 0;
  virtual void set_font_style(const Warsaw::Unistring &) = 0;
  virtual void set_font_attribute(const Warsaw::NVPair &) = 0;

private:
  std::stack<DrawState> states;
};

inline void DrawingKitBase::restore()
{
  Prague::Trace trace("DrawingKitBase::restore");
  if (states.empty()) return; // no state to restore
  DrawState &prev = states.top();
  if(prev.flags & (1 << st_trafo))              set_transformation(prev.saved_trafo);
  if(prev.flags & (1 << st_clip))               set_clipping(prev.saved_clip);
  if(prev.flags & (1 << st_fg_color))           set_foreground(prev.saved_fg_color);
  if(prev.flags & (1 << st_lt_color))           set_lighting(prev.saved_lt_color);
  if(prev.flags & (1 << st_point_size))         set_point_size(prev.saved_point_size);
  if(prev.flags & (1 << st_line_width))         set_line_width(prev.saved_line_width);
  if(prev.flags & (1 << st_line_end_style))     set_line_endstyle(prev.saved_line_end_style);
  if(prev.flags & (1 << st_surface_fill_style)) set_surface_fillstyle(prev.saved_surface_fill_style);
  if(prev.flags & (1 << st_texture))            set_texture(prev.saved_texture);
  if(prev.flags & (1 << st_font_size))          set_font_size(prev.saved_font_size);
  if(prev.flags & (1 << st_font_weight))        set_font_weight(prev.saved_font_weight);
  if(prev.flags & (1 << st_font_family))        set_font_family((Warsaw::Unistring &)prev.saved_font_family);
  if(prev.flags & (1 << st_font_subfamily))     set_font_subfamily((Warsaw::Unistring &)prev.saved_font_subfamily);
  if(prev.flags & (1 << st_font_fullname))      set_font_fullname((Warsaw::Unistring &)prev.saved_font_fullname);
  if(prev.flags & (1 << st_font_style))         set_font_style((Warsaw::Unistring &)prev.saved_font_style);
    //    if(prev.flags[st_font_attr]) {
    //       for (unsigned long i = 0; i < prev.saved_font_attr.length())
    // 	     setFontAttr(prev.saved_font_attr[i]);
    //    }
  states.pop();
}

//########################################################
//############### public setter methods ##################
//########################################################

// sorry, I hate macros as much as the next guy,
// but in this case, templates are simply not cutting it.

#define REMEMBER(state,ty,val) \
 if (!(states.empty() || states.top().flags & (1 << st_## state)))\
{ \
  DrawState &st = states.top(); \
  ty tmp(val); \
  st.saved_## state = tmp; \
  st.flags |= (1 << st_## state); \
}

inline void DrawingKitBase::transformation(Warsaw::Transform_ptr t)
{
  Prague::Trace trace("DrawingKitBase::transformation");
  REMEMBER(trafo, Warsaw::Transform_var,transformation());
  set_transformation(t);
}

inline void DrawingKitBase::clipping(Warsaw::Region_ptr c)
{
  REMEMBER(clip, Warsaw::Region_var, clipping());
  set_clipping(c);
}

inline void DrawingKitBase::foreground(const Warsaw::Color &c)
{
  REMEMBER(fg_color, Warsaw::Color, foreground())
  set_foreground(c);
}

inline void DrawingKitBase::lighting(const Warsaw::Color &c)
{
  REMEMBER(lt_color, Warsaw::Color, lighting())
  set_lighting(c);
}

inline void DrawingKitBase::point_size(Warsaw::Coord s)
{
  REMEMBER(point_size, Warsaw::Coord, point_size());
  set_point_size(s);
}

inline void DrawingKitBase::line_width(Warsaw::Coord w)
{
  REMEMBER(line_width, Warsaw::Coord, line_width());
  set_line_width(w);
}

inline void DrawingKitBase::line_endstyle(Warsaw::DrawingKit::Endstyle s)
{
  REMEMBER(line_end_style, Warsaw::DrawingKit::Endstyle, line_endstyle());
  set_line_endstyle(s);
}

inline void DrawingKitBase::surface_fillstyle(Warsaw::DrawingKit::Fillstyle s)
{
  REMEMBER(surface_fill_style, Warsaw::DrawingKit::Fillstyle, surface_fillstyle());
  set_surface_fillstyle(s);
}

inline void DrawingKitBase::texture(Warsaw::Raster_ptr t)
{
  REMEMBER(texture, Warsaw::Raster_var, texture());
  set_texture(t);
}

  ////////
  // text
  ////////

inline void DrawingKitBase::font_size(CORBA::ULong s)
{
  REMEMBER(font_size,CORBA::ULong, font_size());
  set_font_size(s);
}

inline void DrawingKitBase::font_weight(CORBA::ULong w)
{
  REMEMBER(font_weight,CORBA::ULong, font_weight());
  set_font_weight(w);
}

inline void DrawingKitBase::font_family(const Warsaw::Unistring &f)
{
  REMEMBER(font_family, Warsaw::Unistring_var, font_family());
  set_font_family(f);
}

inline void DrawingKitBase::font_subfamily(const Warsaw::Unistring &f)
{
  REMEMBER(font_subfamily, Warsaw::Unistring_var, font_subfamily());
  set_font_subfamily(f);
}

inline void DrawingKitBase::font_fullname(const Warsaw::Unistring &f)
{
  REMEMBER(font_fullname, Warsaw::Unistring_var, font_fullname());
  set_font_fullname(f);
}

inline void DrawingKitBase::font_style(const Warsaw::Unistring &s)
{
  REMEMBER(font_style, Warsaw::Unistring_var, font_style());
  set_font_style(s);
}

inline void DrawingKitBase::font_attribute(const Warsaw::NVPair &nvp)
{
  // !FIXME! fill this in.. it's not _too_ hard
}

#endif
