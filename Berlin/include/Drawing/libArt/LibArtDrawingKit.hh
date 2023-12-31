/*$Id: LibArtDrawingKit.hh,v 1.20 2000/10/31 18:15:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999,2000 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 1999,2000 Graydon Hoare <graydon@pobox.com> 
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
#ifndef _LibArtDrawingKit_hh
#define _LibArtDrawingKit_hh

#include <Berlin/ImplVar.hh>
#include <Warsaw/Image.hh>
#include <Drawing/DrawingKitBase.hh>
#include <Drawing/libArt/LibArtFTFont.hh>
#include <Drawing/libArt/LibArtRaster.hh>
#include <Berlin/KitImpl.hh>
#include <Berlin/ObjectCache.hh>
#include <Berlin/Console.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Prague/Sys/Thread.hh>

#include <libart_lgpl/art_misc.h>
#include <libart_lgpl/art_alphagamma.h>
#include <libart_lgpl/art_rect.h>
#include <libart_lgpl/art_vpath.h>

#include <string>
#include <vector>

class LibArtDrawingKit : public DrawingKitBase, public KitImpl
{
public:
  LibArtDrawingKit(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~LibArtDrawingKit();

  virtual void transformation(Warsaw::Transform_ptr t) { DrawingKitBase::transformation(t);}
  virtual Warsaw::Transform_ptr transformation() { return Warsaw::Transform::_duplicate(tr);}
  virtual void clipping(Warsaw::Region_ptr r) { DrawingKitBase::clipping(r);}
  virtual Warsaw::Region_ptr clipping() { return Warsaw::Region::_duplicate(cl);}
  virtual void foreground(const Warsaw::Color &c) { DrawingKitBase::foreground(c);}
  virtual Warsaw::Color foreground() { return fg;}
  virtual void lighting(const Warsaw::Color &c) { DrawingKitBase::lighting(c);}
  virtual Warsaw::Color lighting() { return lt;}
  virtual void point_size(Warsaw::Coord c) { DrawingKitBase::point_size(c);}
  virtual Warsaw::Coord point_size() { return ps;}
  virtual void line_width(Warsaw::Coord c) { DrawingKitBase::line_width(c);}
  virtual Warsaw::Coord line_width() { return lw;}
  virtual void line_endstyle(Warsaw::DrawingKit::Endstyle e) { DrawingKitBase::line_endstyle(e);}
  virtual Warsaw::DrawingKit::Endstyle line_endstyle() { return es;}
  virtual void surface_fillstyle(Warsaw::DrawingKit::Fillstyle f) { DrawingKitBase::surface_fillstyle(f);}
  virtual Warsaw::DrawingKit::Fillstyle surface_fillstyle() { return fs;}
  virtual void texture(Warsaw::Raster_ptr r) { DrawingKitBase::texture(r);}
  virtual Warsaw::Raster_ptr texture() { //return tx ? Raster::_duplicate(tx->remote) : 
    return Warsaw::Raster::_nil();}

  virtual CORBA::ULong font_size() { return font->size();}
  virtual CORBA::ULong font_weight() { return font->weight();}
  virtual Warsaw::Unistring *font_family() { return font->family();}
  virtual Warsaw::Unistring *font_subfamily() { return font->subfamily();}
  virtual Warsaw::Unistring *font_fullname() { return font->fullname();}
  virtual Warsaw::Unistring *font_style() { return font->style();}
  virtual Warsaw::DrawingKit::FontMetrics font_metrics() { return font->metrics();}
  virtual Warsaw::DrawingKit::GlyphMetrics glyph_metrics(Warsaw::Unichar uc) { return font->metrics(uc);}
  virtual CORBA::Any *get_font_attribute(const Warsaw::Unistring & name) { return new CORBA::Any();}

  virtual void set_transformation(Warsaw::Transform_ptr);
  virtual void set_clipping(Warsaw::Region_ptr);
  virtual void set_foreground(const Warsaw::Color &);
  virtual void set_lighting(const Warsaw::Color &);
  virtual void set_point_size(Warsaw::Coord);
  virtual void set_line_width(Warsaw::Coord);
  virtual void set_line_endstyle(Warsaw::DrawingKit::Endstyle);
  virtual void set_surface_fillstyle(Warsaw::DrawingKit::Fillstyle);
  virtual void set_texture(Warsaw::Raster_ptr);

  virtual void set_font_size(CORBA::ULong);
  virtual void set_font_weight(CORBA::ULong);
  virtual void set_font_family(const Warsaw::Unistring &);
  virtual void set_font_subfamily(const Warsaw::Unistring &);
  virtual void set_font_fullname(const Warsaw::Unistring &);
  virtual void set_font_style(const Warsaw::Unistring &);
  virtual void set_font_attribute(const Warsaw::NVPair &);

  virtual Warsaw::Coord resolution(Warsaw::Axis a) { return drawable->resolution(a);}
  virtual void draw_path(const Warsaw::Path &);
//   virtual void drawPatch(const Warsaw::Patch &);
  virtual void draw_rectangle(const Warsaw::Vertex &, const Warsaw::Vertex &);
  virtual void draw_quadric(const Warsaw::DrawingKit::Quadric, Warsaw::Coord, Warsaw::Coord);
  virtual void draw_ellipse(const Warsaw::Vertex &, const Warsaw::Vertex &);
  virtual void draw_image(Warsaw::Raster_ptr);
  virtual void allocate_text(const Warsaw::Unistring &, Warsaw::Graphic::Requisition &);
  virtual void draw_text(const Warsaw::Unistring &);
  virtual void allocate_char(Warsaw::Unichar, Warsaw::Graphic::Requisition &);
  virtual void draw_char(Warsaw::Unichar);
  virtual void copy_drawable(Warsaw::Drawable_ptr, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  virtual void flush();

 private:
  void rasterize_pixbuf(ArtPixBuf *pixbuf);
  void identity_pixbuf(ArtPixBuf *pixbuf);

  Console::Drawable *drawable;
  Console::Drawable *buffer;
  ArtPixBuf *pb;
  ArtIRect bbox;
//   const ggi_directbuffer * buf;
  double xres, yres;
  Prague::Mutex mutex;

  double affine[6];
  double scaled_affine[6];

  Warsaw::Transform_var  tr;
  Warsaw::Region_var     cl;
  Warsaw::Color          fg;
  Warsaw::Color          lt;
  Warsaw::Coord          ps;
  Warsaw::Coord          lw;
  Warsaw::DrawingKit::Endstyle       es;
  Warsaw::DrawingKit::Fillstyle      fs;
  LibArtFTFont   *font;
  LibArtFont   *unifont;
  art_u8 alphabank[256][256];
  ArtAlphaGamma *agam;
  art_u32         art_fg;
  Console::Drawable::Pixel con_fg;
  ArtIRect screen,clip;
  
  ObjectCache<Warsaw::Raster_var, LibArtRaster> rasters;
};

#endif 
