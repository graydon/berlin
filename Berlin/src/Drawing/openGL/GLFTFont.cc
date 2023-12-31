/*$Id: GLFTFont.cc,v 1.2 2001/01/09 21:35:10 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
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

#include "Berlin/Logger.hh"
#include "Drawing/openGL/GLFTFont.hh"
#include <Prague/Sys/Directory.hh>
#include <fstream>
#include <string>
#include <cstdlib>

#define FLOOR(x)  ((x) & -64)
#define CEIL(x)   (((x)+63) & -64)
#define TRUNC(x)  ((x) >> 6)
#define MAX(X,Y) (X>Y?X:Y)
#define MIN(X,Y) (X<Y?X:Y)

using namespace Prague;

bool GLFTFont::chooseFaceInteractively(const dictionary_t &faces, const char *env, Unicode::String &fam, Unicode::String &style)
{
  int idx = -1;
  if (env[0] == '\0')
    {
      cout << "list of available fonts :\n";
      unsigned int i = 0;
      for (dictionary_t::const_iterator j = faces.begin(); j != faces.end(); ++i, ++j)
	{
	  cout << i << ' ' << (*j).second->family_name << ' ' << (*j).second->style_name << endl;
	}
      cout << "please choose a number :"; cin >> idx;
    }
  else idx = atoi(env);
  dictionary_t::const_iterator j = faces.begin();
  for (unsigned int i = 0; i != idx && j != faces.end(); ++i, ++j);
  if (j == faces.end()) return false;
  fam = Unicode::String((*j).second->family_name);
  style = Unicode::String((*j).second->style_name);
  return true;
}

GLFTFont::GLFTFont(GGI::Drawable *drawable)
  : xdpi(96.),
    ydpi(96.),
    xres(drawable->resolution(xaxis)),
    yres(drawable->resolution(yaxis)),
    mySize(14)
{
  if (FT_Init_FreeType(&library))
    {
      cerr << "failed to open freetype library" << endl;
      exit(-1);    
    }
  char *env = getenv("BERLIN_ROOT");
  if (!env)
    {
      cerr << "Please set environment variable BERLIN_ROOT first" << endl;
      exit(-1);
    }
  string fname = string(env) + "/etc/fonts.txt";
  ifstream fontdirlist(fname.c_str());
  if (!fontdirlist)
    {
      Logger::log(Logger::text) << "cannot open " << fname << endl;
    }
  else
    {
      string fontdir;
      while (fontdirlist >> fontdir)
	{
	  Directory directory(fontdir, Directory::alpha);
	  Logger::log(Logger::text) << "GLFTFont: scanning font dir " << fontdir << endl;
	  for (Directory::iterator i = directory.begin(); i != directory.end(); i++)
	    {
	      if ((*i)->name() == "." || (*i)->name() == "..") continue;	  
	      string fontfile = (*i)->longName();
	      if (FT_New_Face(library, fontfile.c_str(), 0, &face))
		{
		  Logger::log(Logger::text) << "LibArtFTFont: can't open font " << fontfile << endl;
		  continue;
		}
	      family = Unicode::String(face->family_name);
	      style = Unicode::String(face->style_name);
 	      style_t key(atomize(family), atomize(style));
 	      Logger::log(Logger::text) << "found FT-readable font "
 					<< family << " (" << key.first << ") " << style << " (" << key.second << ") in "
					<< fontfile << endl;
	      faces[key] = face;
	    }
	}
      fontdirlist.close();
      Logger::log(Logger::text) << "completed scaning font directories" << endl;
      char *env = getenv("BERLIN_FONT_CHOOSER");
      Unicode::String tmpFam, tmpStyle;
      if (env && chooseFaceInteractively(faces, env, tmpFam, tmpStyle))
	{
	  family = tmpFam;
	  style  = tmpStyle;
	}
    }
}


GLFTFont::~GLFTFont() {}
unsigned long GLFTFont::size() { return size;}
unsigned long GLFTFont::weight() { return 100;}
void GLFTFont::size(unsigned long sz) { size = sz;}
void GLFTFont::weight(unsigned long wt) {}

Unistring *GLFTFont::family()
{ 
  return new Unistring (Unicode::to_CORBA(Unicode::String(family)));
}

Unistring *GLFTFont::subfamily() { return 0;}
void GLFTFont::subfamily(const Unistring &fname) {}
void GLFTFont::fullname(const Unistring &fname) {}

void GLFTFont::family(const Unistring &fname)
{ 
  family = Unicode::to_internal(fname);
  key = style_t(atomize(family), atomize(style));
}

void GLFTFont::style(const Unistring &sname)
{ 
  style = Unicode::to_internal(sname);
  key = style_t(atomize(family), atomize(style));
}

Unistring *GLFTFont::fullname()
{ 
  Unicode::String str = family;
  str += Unicode::Char(' ');
  str += style;
  return new Unistring(Unicode::to_CORBA(str));
}

Unistring *GLFTFont::style()
{ 
  return new Unistring(Unicode::to_CORBA(Unicode::String(myStyleStr)));
}

DrawingKit::FontMetrics GLFTFont::metrics()
{
  DrawingKit::FontMetrics fm;
  FT_Face newface;
  setup_face(newface);
  setup_size(newface);

  fm.ascender = newface->size->metrics.ascender;
  fm.descender = newface->size->metrics.descender;
  fm.height = newface->size->metrics.height;
  fm.max_advance = newface->size->metrics.max_advance;
  return fm;
}

DrawingKit::GlyphMetrics GLFTFont::metrics(Unichar &uc)
{
  DrawingKit::GlyphMetrics gm;
  FT_Face newface;
  setup_face(newface);
  setup_size(newface);
  if (!load_glyph(uc, newface)) return gm;
  FT_GlyphSlot glyph = newface->glyph;

  gm.width = glyph->metrics.width;
  gm.height = glyph->metrics.height;
  gm.horiBearingX = glyph->metrics.horiBearingX;
  gm.horiBearingY = glyph->metrics.horiBearingY;
  gm.horiAdvance =  glyph->metrics.horiAdvance;
  gm.vertBearingX = glyph->metrics.vertBearingX;
  gm.vertBearingY = glyph->metrics.vertBearingY;
  gm.vertAdvance = glyph->metrics.vertAdvance;
  return gm;
}

void GLFTFont::getPixBuf(const Unichar ch, ArtPixBuf &pb)
{
  FT_Face newface;
  setup_face(newface);
  setup_size(newface);
  if (!load_glyph(ch,newface)) return;
  if (!render_pixbuf(pb,newface->glyph,myLibrary)) return;      
}

void GLFTFont::allocateChar(Unichar ch, Graphic::Requisition &r)
{
  FT_Face newface;
  setup_face(newface);
  setup_size(newface);
  FT_GlyphSlot glyph = newface->glyph;
  if (!load_glyph(ch,newface)) return;
  r.x.natural = r.x.minimum = r.x.maximum = ((double)(glyph->metrics.horiAdvance >> 6)) / xres;
  r.x.defined = true;
  r.x.align = 0.;
  r.y.natural = r.y.minimum = r.y.maximum = ((double)(glyph->metrics.height >> 6)) / yres; 
  r.y.defined = true;
  r.y.align = (glyph->metrics.height == 0) ? 0. : 
    ((double)glyph->metrics.horiBearingY)/((double)glyph->metrics.height); 
}

void GLFTFont::setup_face(FT_Face &f)
{
  if (faces.find(key) != faces.end()) f = faces[key];
  else f = face;
}

void GLFTFont::setup_size(FT_Face &f)
{
  FT_Set_Char_Size(f,                // handle to face object           
		   size << 6,     // char_width in 1/64th of points  
		   size << 6,     // char_height in 1/64th of points 
		   (unsigned int)xdpi,  // horizontal device resolution    
		   (unsigned int)ydpi); // vertical device resolution      
}

bool GLFTFont::load_glyph(Unichar c, FT_Face &f)
{
  FT_CharMap  found = 0;
  FT_CharMap  charmap;  
  for (int n = 0; n < f->num_charmaps; n++ )
    {
      charmap = f->charmaps[n];
      if (charmap->encoding == ft_encoding_unicode)
	{	
	  found = charmap;
	  break;
	}
    }
  if (!found)
    { 
      // no way of translating!
      return false; 
    }
  /* now, select the charmap for the face object */
  if (FT_Set_Charmap( f, found )) return false;
  int idx = FT_Get_Char_Index(f,(unsigned char)c);
  if (FT_Load_Glyph (f,idx,0)) return false;
  return true;
}

static inline bool render_pixbuf(char *buffer, FT_GlyphSlot &glyph, FT_Library &library)
{
  // first, render the glyph into an intermediate buffer  
  if (glyph->format != ft_glyph_format_outline) return false;
  int size = pb.width * pb.height;
  char bit_buffer[size];
  memset(bit_buffer,0,size);      

  FT_Bitmap  bit;
  bit.width      = pb.width;
  bit.rows       = pb.height;
  bit.pitch      = pb.width;
  bit.pixel_mode = ft_pixel_mode_grays;
  bit.buffer     = bit_buffer;      

  int left  = FLOOR(glyph->metrics.horiBearingX);
  int bottom = FLOOR(glyph->metrics.horiBearingY - glyph->metrics.height);
  FT_Outline_Translate(&glyph->outline, -left, -bottom);
  if (FT_Outline_Get_Bitmap(library, &glyph->outline, &bit)) return false;
      
  while (--size > -1)
    pb.pixels[(size << 2) + 3] = bit_buffer[size];
  return true;
}

