/*$Id: LibArtDrawingKit.cc,v 1.33 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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

#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/IO.hh>
#include <Berlin/Provider.hh>
#include <Berlin/Console.hh>
#include "Drawing/libArt/LibArtDrawingKit.hh"
#include "Drawing/libArt/LibArtFTFont.hh"
#include "Drawing/libArt/LibArtUnifont.hh"

#include <libart_lgpl/art_pathcode.h>
#include <libart_lgpl/art_pixbuf.h>
#include <libart_lgpl/art_affine.h>
#include <libart_lgpl/art_rect.h>
#include <libart_lgpl/art_vpath.h>
#include <libart_lgpl/art_svp.h>
#include <libart_lgpl/art_svp_vpath.h>
#include <libart_lgpl/art_svp_wind.h>
#include <libart_lgpl/art_rect_svp.h>
#include <libart_lgpl/art_rgb_svp.h>
#include <libart_lgpl/art_rgb_pixbuf_affine.h>

using namespace Warsaw;

LibArtDrawingKit::~LibArtDrawingKit() {}
LibArtDrawingKit::LibArtDrawingKit(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p),
    drawable(Console::drawable()), 
    xres(drawable->resolution(xaxis)),
    yres(drawable->resolution(yaxis)),
    font(new LibArtFTFont(drawable)),
    unifont(new LibArtUnifont(drawable)),
    rasters(500)
  // textures(100), 
  // tx(0)
{
  screen.x0 = 0;
  screen.y0 = 0;
  screen.x1 = drawable->width();
  screen.y1 = drawable->height();
  
  agam = art_alphagamma_new (2.5);
  buffer = Console::create_drawable(drawable->width(), drawable->height(), 3);
  pb = art_pixbuf_new_const_rgb ((art_u8 *)buffer->write_buffer(), drawable->width(), drawable->height(), buffer->row_length());
  bbox.x0 = bbox.y0 = bbox.x1 = bbox.y1 = 0;    
  double step = 1. / 256.;
  for (int i = 0; i < 256; ++i)
    for (int j = 0; j < 256; ++j) 
      alphabank[i][j] = (art_u8)(i * (j * step));
}

static inline art_u32 artColor(const Color &c)
{
  return (((art_u8)(c.blue * 0xff) << 24) | 
	  ((art_u8)(c.green * 0xff) << 16) | 
	  ((art_u8)(c.red * 0xff) << 8) | 
	  ((art_u8)(c.alpha * 0xff)));
}

static inline void fix_order_of_irect(ArtIRect &ir)
{
  if (ir.x0 > ir.x1) {int tmp = ir.x0; ir.x1 = ir.x0; ir.x0 = tmp;}
  if (ir.y0 > ir.y1) {int tmp = ir.y0; ir.y0 = ir.y1; ir.y1 = tmp;}
}

void LibArtDrawingKit::set_transformation(Transform_ptr t)
{
  Prague::Trace trace("LibArtDrawingKit::set_transformation");
  if (CORBA::is_nil(t)) art_affine_identity(affine);
  else
    {
      tr = Transform::_duplicate(t);
      Transform::Matrix matrix;
      tr->store_matrix(matrix);
      affine[0] = matrix[0][0];
      affine[1] = matrix[1][0];
      affine[2] = matrix[0][1];
      affine[3] = matrix[1][1];
      affine[4] = matrix[0][3];
      affine[5] = matrix[1][3];
    }
  scaled_affine = affine;
  scaled_affine[0] *= xres;
  scaled_affine[1] *= xres;
  scaled_affine[2] *= yres;
  scaled_affine[3] *= yres;
  scaled_affine[4] *= xres;
  scaled_affine[5] *= yres;
}

void LibArtDrawingKit::set_clipping(Region_ptr r)
{
  if (CORBA::is_nil(r)) {clip = screen; return;}
  cl = Region::_duplicate(r);
    
  Lease_var<RegionImpl> climpl(Provider<RegionImpl>::provide());
  climpl->copy(cl);

  ArtDRect dclip = {climpl->lower.x * xres, climpl->lower.y * yres, 		      
		      climpl->upper.x * xres, climpl->upper.y * yres};
  art_drect_to_irect(&clip, &dclip); 
  art_irect_intersect(&clip,&clip,&screen);
}

void LibArtDrawingKit::set_foreground(const Color &c)
{
  fg = c;
  Color tmp;
  tmp.red   = fg.red*lt.red;
  tmp.green = fg.green*lt.green;
  tmp.blue  = fg.blue*lt.blue;
  tmp.alpha = fg.alpha;
  art_fg    = artColor(tmp);
  con_fg    = buffer->map(tmp);
}

void LibArtDrawingKit::set_lighting(const Color &c)
{
  lt = c;
  Color tmp;
  tmp.red   = fg.red*lt.red;
  tmp.green = fg.green*lt.green;
  tmp.blue  = fg.blue*lt.blue;
  art_fg    = artColor(tmp);
  con_fg    = buffer->map(tmp);
}

void LibArtDrawingKit::set_point_size(Coord s) { ps = s;}
void LibArtDrawingKit::set_line_width(Coord w) { lw = w;}
void LibArtDrawingKit::set_line_endstyle(Warsaw::DrawingKit::Endstyle style) { es = style;}
void LibArtDrawingKit::set_surface_fillstyle(Warsaw::DrawingKit::Fillstyle style) { fs = style;}
void LibArtDrawingKit::set_texture(Raster_ptr t) {}
void LibArtDrawingKit::set_font_size(CORBA::ULong) {}
void LibArtDrawingKit::set_font_weight(CORBA::ULong) {}
void LibArtDrawingKit::set_font_family(const Unistring&) {}
void LibArtDrawingKit::set_font_subfamily(const Unistring&) {}
void LibArtDrawingKit::set_font_fullname(const Unistring&) {}
void LibArtDrawingKit::set_font_style(const Unistring&) {}
void LibArtDrawingKit::set_font_attribute(const NVPair & nvp) {}

void LibArtDrawingKit::draw_path(const Path &p) 
{
  int len = p.length();
  ArtVpath vpath[fs == Warsaw::DrawingKit::outlined ? len : len + 1];
  ArtVpath *tvpath;  

  if (fs == Warsaw::DrawingKit::outlined)
    {
      for (int i = 0; i < len; ++i)
	{
	  vpath[i].x = p[i].x; 
	  vpath[i].y = p[i].y;
	  vpath[i].code = ART_LINETO;
	}
      vpath[0].code = ART_MOVETO_OPEN;
      vpath[len-1].code = ART_END;
      
    }
  else
    {
      for (int i = 0; i < len; ++i)
	{
	  vpath[i].x = p[i].x; 
	  vpath[i].y = p[i].y;
	  vpath[i].code = ART_LINETO;
	}
      vpath[0].code = ART_MOVETO;
      vpath[len].x = vpath[0].x;
      vpath[len].y = vpath[0].y;
      vpath[len].code = ART_END;
    }
  
  ArtDRect locd; ArtIRect loc;
  tvpath = art_vpath_affine_transform(vpath, scaled_affine);
  ArtSVP *svp1 = art_svp_from_vpath(tvpath); 
  ArtSVP *svp2 = art_svp_uncross(svp1);
  ArtSVP *svp = art_svp_rewind_uncrossed(svp2, ART_WIND_RULE_ODDEVEN);

  art_drect_svp(&locd, svp);
  art_drect_to_irect(&loc, &locd);
  art_irect_intersect(&loc,&loc,&clip);
  art_irect_union(&bbox,&bbox,&loc);
  fix_order_of_irect(loc); 
  art_rgb_svp_alpha(svp, loc.x0, loc.y0, loc.x1, loc.y1, art_fg,
		    ((art_u8 *)buffer->write_buffer()) + (loc.y0 * pb->rowstride) + (loc.x0 * 3), 
		    buffer->row_length(), agam);
  art_svp_free(svp);
  art_svp_free(svp1);
  art_svp_free(svp2);
}

//void LibArtDrawingKit::drawPatch(const Patch &);

void LibArtDrawingKit::draw_rectangle(const Vertex &bot, const Vertex &top) 
{
  // fast path opaque non-transformed rectangles
  if (fg.alpha == 1. &&
      affine[0] == 1 &&
      affine[1] == 0 &&
      affine[2] == 0 &&
      affine[3] == 1) {

    ArtIRect rect;
    rect.x0 = (int)((bot.x + affine[4]) * xres);
    rect.x1 = (int)((top.x  + affine[4])* xres);
    rect.y0 = (int)((bot.y + affine[5]) * yres);
    rect.y1 = (int)((top.y + affine[5]) * yres);
    art_irect_intersect(&rect,&rect,&clip);
    int width = (rect.x1 - rect.x0);
    int height = (rect.y1 - rect.y0);
    if ((height * width) < 1) return;
    buffer->set_color(con_fg);
    if (fs == Warsaw::DrawingKit::solid)
      buffer->draw_box(rect.x0, rect.y0, width, height);
    else
      {
	buffer->draw_hline(rect.x0, rect.y0, width);
	buffer->draw_hline(rect.x0, rect.y1, width);
	buffer->draw_vline(rect.x0, rect.y0, height);
	buffer->draw_vline(rect.x1, rect.y0, height);
      }
    art_irect_union (&bbox,&bbox,&rect);
    return;
    
    // non-degenerate rectangles
  } else {
    Path path;
    if (fs == Warsaw::DrawingKit::outlined) {
      path.length(4);
      path[0].x = bot.x, path[0].y = bot.y;
      path[1].x = top.x, path[1].y = bot.y;
      path[2].x = top.x, path[2].y = top.y;
      path[3].x = bot.x, path[2].y = top.y;
    } else {
      path.length(5);
      path[0].x = bot.x, path[0].y = bot.y;
      path[1].x = top.x, path[1].y = bot.y;
      path[2].x = top.x, path[2].y = top.y;
      path[3].x = bot.x, path[3].y = top.y;
      path[4].x = bot.x, path[4].y = bot.y;
    }
    draw_path(static_cast<const Path>(path));
  }
}

void LibArtDrawingKit::draw_quadric(const Warsaw::DrawingKit::Quadric, Warsaw::Coord, Warsaw::Coord)
{
}

void LibArtDrawingKit::draw_ellipse(const Vertex &, const Vertex &) {}

void LibArtDrawingKit::draw_image(Raster_ptr remote) {
  rasterize_pixbuf(rasters.lookup(Raster::_duplicate(remote))->pixbuf);
}

void LibArtDrawingKit::identity_pixbuf(ArtPixBuf *pixbuf) {
  // fast path for non-transformed grey-scale glyph images
  ArtIRect rect;
  rect.x0 = (int)(affine[4] * xres);
  rect.x1 = rect.x0 + pixbuf->width;
  rect.y0 = (int)(affine[5] * yres);
  rect.y1 = rect.y0 + pixbuf->height;

  // work out offset within source image
  int dx = clip.x0 - rect.x0;
  int dy = clip.y0 - rect.y0;
  if (dx < 0) dx = 0;
  if (dy < 0) dy = 0;

  art_irect_intersect(&rect,&rect,&clip);
  if (((rect.y1 - rect.y0) * (rect.x1 - rect.x0)) < 1) return;
  art_u8 *dst = pb->pixels + rect.y0 * pb->rowstride + rect.x0 * 3;
  art_u8 *src = pixbuf->pixels + dy * pixbuf->rowstride + dx;
  int width = (rect.x1 - rect.x0);
  int height = (rect.y1 - rect.y0);  
  art_u8 *atab = alphabank[(art_u8)(art_fg & 0xff)];
  art_u8 *rtab = alphabank[(art_u8)((art_fg >> 24) & 0xff)];
  art_u8 *gtab = alphabank[(art_u8)((art_fg >> 16) & 0xff)];
  art_u8 *btab = alphabank[(art_u8)((art_fg >> 8) & 0xff)];

  int t;
  int dst_skip = pb->rowstride - width * 3;
  int src_skip = pixbuf->rowstride - width;
  art_u8 *ptab;


  for (int row = 0; row < height; ++row, dst += dst_skip, src += src_skip) {
    for (int col = 0; col < width; ++col, dst += 3, ++src) {            
      ptab = alphabank[*src];
      dst[0]=t=dst[0] + atab[rtab[*src]] - atab[ptab[dst[0]]]; 
      t&=0x100; t>>=8; t-=1; t=~t; dst[0] |= (t & 0xff);
      dst[1]=t=dst[1] + atab[gtab[*src]] - atab[ptab[dst[1]]]; 
      t&=0x100; t>>=8; t-=1; t=~t; dst[1] |= (t & 0xff);
      dst[2]=t=dst[2] + atab[btab[*src]] - atab[ptab[dst[2]]]; 
      t&=0x100; t>>=8; t-=1; t=~t; dst[2] |= (t & 0xff);
    }
  }	
  art_irect_union (&bbox,&bbox,&rect);
  return;    
}


void LibArtDrawingKit::rasterize_pixbuf(ArtPixBuf *pixbuf) {

  // NOTE: this entire routine takes place "in device space"
  // since that is (a) the source of the raster and (b) the destination
  // of libart's transformation. the goal is to get everything into
  // device space and work with it there.

  double dev_affine[6] = {affine[0], affine[1], affine[2], affine[3], 
			    affine[4] * xres, affine[5] * yres };
  			      
  // pre-transformation target rectangle, in device space coords
  ArtDRect slocd = {0,0,(double)(pixbuf->width),(double)(pixbuf->height)};
  ArtDRect tslocd; 
  ArtIRect tsloci; 

  int width = pixbuf->width;
  int pix = ((pixbuf->n_channels * pixbuf->bits_per_sample + 7) >> 3); 
  int row = (width) * pix;
  int skip = (pixbuf->rowstride - row);
  int size = (fg.alpha == 1. ? 0 : (pixbuf->height - 1) * pixbuf->rowstride + width * pix);      

  art_u8 tmp[size];
  art_u8 *save = pixbuf->pixels;

  // alpha-correct the image
  if (fg.alpha != 1.) {
    art_u8 *end_write = tmp + (size - 1);
    art_u8 *reader = pixbuf->pixels;
    art_u8 *tab = alphabank[(art_u8)(art_fg & 0x000000ff)];
    art_u8 *eol;
    for (art_u8 *writer = tmp; writer < end_write; reader += skip, writer += skip) {
      memcpy(writer,reader,row);
      eol = writer + row;      
      while (writer < eol) {
	writer += 3; reader += 3;
	*writer++ = *(tab + *reader++);      
      }
    }
    pixbuf->pixels = tmp;
  }
  
  // transform target (in device space)
  art_drect_affine_transform(&tslocd,&slocd,dev_affine); 
  art_drect_to_irect(&tsloci, &tslocd); 
  fix_order_of_irect(tsloci); 

  // clip 
  art_irect_intersect(&tsloci,&tsloci,&clip);
  art_irect_union (&bbox,&bbox,&tsloci);
	  	 
  // paint
  art_rgb_pixbuf_affine((art_u8 *)buffer->write_buffer() + 
			(tsloci.y0 * pb->rowstride) + 
			(tsloci.x0 * 3), // 3 for "R,G,B" packed pixels			
			tsloci.x0, tsloci.y0, tsloci.x1, tsloci.y1,
			buffer->row_length(),
			pixbuf, dev_affine,
			ART_FILTER_NEAREST, agam);  

  pixbuf->pixels = save;
}

void LibArtDrawingKit::draw_text(const Unistring &u) 
{
  // presently disabled. should delegate to drawChar
}

void LibArtDrawingKit::draw_char(Unichar c)
{
  double x0 = affine[4];
  double y0 = affine[5];
  int width;
  int height;
  Graphic::Requisition r;
  
  if (c > 127)
    {
      unifont->allocateChar(c,r);
      width = (int) (r.x.maximum * xres);
      height = (int) (r.y.maximum * yres);
    }
  else
    {
      font->allocateChar(c,r);
      Warsaw::DrawingKit::GlyphMetrics gm = font->metrics(c);
      width = (int) (gm.width >> 6);
      height = (int) (gm.height >> 6);
    }
  
  ArtPixBuf *pb;
  int transformed = false;
  double matrix[4];
  for (int i = 0; i < 4; ++i) matrix[i] = affine[i];

  if (c > 127) {
    transformed = unifont->transform(matrix);
    unifont->getPixBuf(c,pb);
  } else {  
    transformed = font->transform(matrix);
    font->getPixBuf(c,pb);
  }

  if (transformed || (affine[0] == 1 &&
		      affine[1] == 0 &&
		      affine[2] == 0 &&
		      affine[3] == 1)) {

    affine[5] -= (affine[2] * r.x.maximum * r.x.align) + (affine[3] * r.y.maximum * r.y.align);
    identity_pixbuf(pb);      

  } else {   
    // *sigh* use primitive libart pixel functions
    affine[4] -= (r.y.maximum * r.y.align * affine[2]);
    affine[5] -= (r.y.maximum * r.y.align * affine[3]);        
    int pix = 4;
    int row = width * pix; 
    int size = height * row;
    art_u8 pixels[size];    
    //setup foreground color
    for (int i = 0; i < row; ++i)
      {
	pixels[i] =   (unsigned char) ((art_fg >> 24) & 0x000000ff);
	pixels[++i] = (unsigned char) ((art_fg >> 16) & 0x000000ff);
	pixels[++i] = (unsigned char) ((art_fg >> 8) & 0x000000ff);
	pixels[++i] = (unsigned char) 0;
      }
    for (int i = 0; i < height; ++i) memcpy (pixels + (i * row), pixels, row);
    for (int i = 0; i < (width * height); ++i) pixels[pix*i + 3] = pb->pixels[i];    
    ArtPixBuf *pb2 = art_pixbuf_new_const_rgba (pixels, width, height, row);  
    rasterize_pixbuf(pb2);
    art_pixbuf_free(pb2);
  }
  
  affine[4] = x0;
  affine[5] = y0;
}

void LibArtDrawingKit::allocate_char(Unichar c, Graphic::Requisition & req)
{
  if (c > 127) unifont->allocateChar(c,req);
  else font->allocateChar(c,req);
}


void LibArtDrawingKit::allocate_text(const Unistring & s, Graphic::Requisition & req)
{
  //   font->allocate(s,req);
}

void LibArtDrawingKit::copy_drawable(Drawable_ptr d, PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h)
{
  CORBA::Double x2 = affine[4] * buffer->resolution(xaxis);
  CORBA::Double y2 = affine[5] * buffer->resolution(yaxis);
  buffer->blit(d, x, y, w, h, static_cast<long>(x2 + x), static_cast<long>(y2 + y));
}

void LibArtDrawingKit::flush()
{   
  int x = bbox.x0;
  int y = bbox.y0;
  int w = bbox.x1 - bbox.x0;
  int h = bbox.y1 - bbox.y0;  
  buffer->flush(x, y, w, h);
  drawable->blit(*buffer, x, y, w, h, x, y);
  bbox.x0 = bbox.y0 = bbox.x1 = bbox.y1 = 0;  
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "LibArtDrawingKit"};
  return new KitFactoryImpl<LibArtDrawingKit> ("IDL:Warsaw/DrawingKit:1.0", properties, 1);
}
