/*$Id: LibArtFTFont.hh,v 1.17 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _LibArtFTFont_hh
#define _LibArtFTFont_hh

#include <Prague/Sys/MMap.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Unicode.hh>
#include <Berlin/Console.hh>
#include <Berlin/LRUCache.hh>
#include <Drawing/libArt/LibArtFont.hh>
#include <freetype/freetype.h>
#include <map>

//. this is a simple Freetype font, which doesn't support
//. ligatures or complex layout features
class LibArtFTFont : public LibArtFont
{
public:
  LibArtFTFont(Console::Drawable *drawable);
  virtual ~LibArtFTFont();
  virtual CORBA::ULong size();
  virtual void size(CORBA::ULong);
  virtual CORBA::ULong weight();
  virtual void weight(CORBA::ULong);
  virtual void family(const Warsaw::Unistring &);
  virtual void subfamily(const Warsaw::Unistring &);
  virtual void fullname(const Warsaw::Unistring &);
  virtual void style(const Warsaw::Unistring &);
  virtual Warsaw::Unistring *family();
  virtual Warsaw::Unistring *subfamily();
  virtual Warsaw::Unistring *fullname();
  virtual Warsaw::Unistring *style();
  virtual Warsaw::DrawingKit::FontMetrics metrics();
  virtual Warsaw::DrawingKit::GlyphMetrics metrics(Warsaw::Unichar &);
  virtual void getPixBuf(const Warsaw::Unichar, ArtPixBuf *&);
  virtual bool transform(double trafo[4]);
  virtual void allocateChar(Warsaw::Unichar, Warsaw::Graphic::Requisition &);

  void setup_face(FT_Face &f);
  void setup_size(FT_Face &f);
  bool load_glyph(Warsaw::Unichar c, FT_Face &f);
  void matrix(FT_Matrix &m) {m = _matrix;}

  double get_scale() const { return _scale;} 
  
protected:

  typedef unsigned int atom;  
  class Atomizer
  {
  protected:
    atom _atom;
    std::map<Babylon::String, atom> _atoms;
  public:
    atom atomize(Babylon::String &);
  };
  Atomizer _a;
  atom atomize(Babylon::String &u) {return _a.atomize(u);}
    
  double _xres, _yres, _xdpi, _ydpi;  
  typedef unsigned int PtSize;
  FT_Matrix _matrix;
  typedef std::pair<atom,atom> FamStyle;
  typedef std::pair<PtSize,FamStyle> FaceSpec;
  typedef std::pair<Warsaw::Unichar,FaceSpec>  GlyphSpec;
  typedef std::pair<FT_Matrix, GlyphSpec>  TGlyphSpec;

  class TGlyphSpec_cmp
  {
  public:
    bool operator ()(const TGlyphSpec &a, const TGlyphSpec &b)
    {
      return 
	// this is why a generalized product type constructor is better than
	// ad-hoc memory structure definition. *sigh*
	((a.first.xx < b.first.xx)) ||
	((a.first.xx == b.first.xx) && (a.first.xy < b.first.xy)) ||
	((a.first.xx == b.first.xx) && (a.first.xy == b.first.xy) && (a.first.yx < b.first.yx)) ||
	((a.first.xx == b.first.xx) && (a.first.xy == b.first.xy) && (a.first.yx == b.first.yx) && (a.first.yy < b.first.yy)) ||
	((a.first.xx == b.first.xx) && (a.first.xy == b.first.xy) && (a.first.yx == b.first.yx) && (a.first.yy == b.first.yy) &&
	 (a.second < b.second))
	;      
    }
  };

  class GlyphMetricsFactory
  {
  private:
    LibArtFTFont *_font;
    FT_Library   *_lib;
  public:
    GlyphMetricsFactory(LibArtFTFont *f, FT_Library *l) : _font(f), _lib(l) {}
    Warsaw::DrawingKit::GlyphMetrics produce(const TGlyphSpec &);
    void recycle(Warsaw::DrawingKit::GlyphMetrics) {};
  };
  
  class FaceMetricsFactory
  {
  private:
    LibArtFTFont *_font;
    FT_Library   *_lib;
  public:
    FaceMetricsFactory(LibArtFTFont *f, FT_Library *l) : _font(f), _lib(l) {}
    Warsaw::DrawingKit::FontMetrics produce(const FaceSpec &);
    void recycle(Warsaw::DrawingKit::FontMetrics) {};
  };

  class GlyphFactory
  {
  private:
    LibArtFTFont *_font;
    FT_Library   *_lib;
  public:
    GlyphFactory(LibArtFTFont *f, FT_Library *l) : _font(f), _lib(l) {};
    ArtPixBuf *produce(const TGlyphSpec &);
    void recycle(ArtPixBuf *pb) { art_pixbuf_free(pb);};
  };
  
  atom                        _family;
  atom                        _style;
  Babylon::String             _familyStr;
  Babylon::String             _styleStr;
  PtSize                      _size; 
  double                      _scale;
  FT_Library                  _library;
  FT_Face                     _face;
  std::map<FamStyle, FT_Face> _faces;

  // caches!
  LRUCache<TGlyphSpec,ArtPixBuf *, GlyphFactory, 
	   std::map<TGlyphSpec,ArtPixBuf *,TGlyphSpec_cmp> > _glyphCache;
  LRUCache<FaceSpec, Warsaw::DrawingKit::FontMetrics, FaceMetricsFactory> _faceMetricsCache;
  LRUCache<TGlyphSpec, Warsaw::DrawingKit::GlyphMetrics, GlyphMetricsFactory,
	   std::map<TGlyphSpec, Warsaw::DrawingKit::GlyphMetrics,TGlyphSpec_cmp> > _glyphMetricsCache;
private:
  bool chooseFaceInteractively(const std::map<FamStyle, FT_Face> &, const char *, Babylon::String &, Babylon::String &);
};

#endif
