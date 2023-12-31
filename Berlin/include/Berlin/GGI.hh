/*$Id: GGI.hh,v 1.16 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _GGI_hh
#define _GGI_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Input.hh>
#include <Berlin/Logger.hh>
#include <vector>
extern "C"
{
#include <ggi/ggi-unix.h>
}

class GGIConsole;

class GGIDrawable
{
  friend class GGIConsole;
  friend class DrawableTie<GGIDrawable>;
public:
  typedef ggi_pixel Pixel;
  GGIDrawable(const char *, Warsaw::PixelCoord = 0, Warsaw::PixelCoord = 0, Warsaw::PixelCoord = 0);
  ~GGIDrawable();
  Warsaw::Drawable::PixelFormat pixel_format();
  Warsaw::Drawable::BufferFormat buffer_format();
  Warsaw::PixelCoord width() const;
  Warsaw::PixelCoord height() const;
  Warsaw::PixelCoord vwidth() const;
  Warsaw::PixelCoord vheight() const;
  Warsaw::Coord resolution(Warsaw::Axis a) const;
  Warsaw::Coord dpi(Warsaw::Axis a) const;
  Warsaw::PixelCoord row_length() const;
  Pixel map(const Warsaw::Color &) const;
  void *read_buffer() const;
  void *write_buffer() const;
  /*
   * read one or more pixels from framebuffer
   */
  void read_pixel(Warsaw::PixelCoord, Warsaw::PixelCoord, Pixel &) const;
  void read_pixels(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *) const;
  /*
   * draw primitives with the current color (Pixel)
   */
  void set_color(Pixel);
  void draw_pixel(Warsaw::PixelCoord, Warsaw::PixelCoord);
  void draw_hline(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void draw_vline(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void draw_line(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void draw_box(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  /*
   * draw primitives with the given color (Pixel)
   */
  void put_pixel(Warsaw::PixelCoord, Warsaw::PixelCoord, Pixel);
  void put_hline(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *);
  void put_vline(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *);
  void draw_pixels(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *);
  /*
   * fast blits
   */
  void blit(Warsaw::PixelCoord, Warsaw::PixelCoord,
	    Warsaw::PixelCoord, Warsaw::PixelCoord,
	    Warsaw::PixelCoord, Warsaw::PixelCoord);
  void blit(const GGIDrawable &,
	    Warsaw::PixelCoord, Warsaw::PixelCoord,
	    Warsaw::PixelCoord, Warsaw::PixelCoord,
	    Warsaw::PixelCoord, Warsaw::PixelCoord);
  void blit(Warsaw::Drawable_ptr,
	    Warsaw::PixelCoord, Warsaw::PixelCoord,
	    Warsaw::PixelCoord, Warsaw::PixelCoord,
	    Warsaw::PixelCoord, Warsaw::PixelCoord);

  void flush();
  void flush(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  /*
   * if you really must ask...
   */
  ggi_visual_t visual() const { return _visual;}
  ggi_mode     mode() const { return _mode;}
  const ggi_directbuffer *buffer(unsigned int i) const { return ggiDBGetBuffer (_visual, i);}
  void init() { }
  void finish() { }
private:
  ggi_visual_t  _visual;
  ggi_mode      _mode;
};

class GGIConsole
{
  typedef std::vector<DrawableTie<GGIDrawable> *> dlist_t;
public:
  typedef GGIDrawable Drawable;
  GGIConsole(int &, char **, PortableServer::POA_ptr);
  ~GGIConsole();
  static DrawableTie<Drawable> *drawable();
  static DrawableTie<Drawable> *create_drawable(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  static DrawableTie<Drawable> *create_drawable(Drawable *);
  static Warsaw::Drawable_ptr activate_drawable(DrawableTie<GGIDrawable> *);
  static DrawableTie<Drawable> *reference_to_servant(Warsaw::Drawable_ptr);

  Warsaw::Input::Event *next_event();
  void wakeup();
  void activate_autoplay() { _autoplay = true;}
private:
  Warsaw::Input::Event *synthesize(const ggi_event &);
  /*
   * to be used for event notification
   */
  ggi_visual_t  _visual;
  /*
   * to be used for pointing devices
   */
  long          _size[2];
  long          _position[2];
  double        _resolution[2];
  bool          _autoplay;
  int           _wakeupPipe[2];
  static dlist_t                 _drawables;
  static PortableServer::POA_var _poa;
};

inline Warsaw::Drawable::PixelFormat pixel_format();
inline Warsaw::Drawable::BufferFormat buffer_format();
inline Warsaw::PixelCoord GGIDrawable::width() const { return _mode.visible.x;}
inline Warsaw::PixelCoord GGIDrawable::height() const { return _mode.visible.y;}
inline Warsaw::PixelCoord GGIDrawable::vwidth() const { return _mode.virt.x;}
inline Warsaw::PixelCoord GGIDrawable::vheight() const { return _mode.virt.y;}
inline Warsaw::Coord GGIDrawable::resolution(Warsaw::Axis a) const
{
  /*
   * mode.size is in mm
   * our base unit is 0.1 mm...
   */
  return a == Warsaw::xaxis ?
    0.1 * _mode.visible.x / _mode.size.x :
    0.1 * _mode.visible.y / _mode.size.y;
}
inline Warsaw::Coord GGIDrawable::dpi(Warsaw::Axis a) const
{ return resolution(a) * 254.0;}
inline Warsaw::PixelCoord GGIDrawable::row_length() const
{ return ggiDBGetBuffer(_visual, 0)->buffer.plb.stride;}
inline GGIDrawable::Pixel GGIDrawable::map(const Warsaw::Color &c) const
{
  ggi_color c2;
  // GGI _appears_ to use 16 bit color + alpha throughout. *sigh*
  static double scale = 0xffff;
  c2.r = static_cast<uint16>(c.red * scale);
  c2.g = static_cast<uint16>(c.green * scale);
  c2.b = static_cast<uint16>(c.blue * scale);
  c2.a = static_cast<uint16>(c.alpha * scale);
  return ggiMapColor(_visual, &c2);
}
inline void *GGIDrawable::read_buffer() const
{ return ggiDBGetBuffer (_visual, 0)->read;}
inline void *GGIDrawable::write_buffer() const
{ return ggiDBGetBuffer (_visual, 0)->write;}
inline void GGIDrawable::read_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Pixel &p) const
{ ggiGetPixel(_visual, x, y, &p);}
inline void GGIDrawable::read_pixels(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h, void *p) const
{
  ggiGetBox(_visual, x, y, w, h, p);
}

inline void GGIDrawable::set_color(Pixel p) { ggiSetGCForeground(_visual, p);}
inline void GGIDrawable::draw_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y)
{ ggiDrawPixel(_visual, x, y);}
inline void GGIDrawable::draw_hline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w)
{ ggiDrawHLine(_visual, x, y, w);}
inline void GGIDrawable::draw_vline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord h)
{ ggiDrawVLine(_visual, x, y, h);}
inline void GGIDrawable::draw_line(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{ ggiDrawLine(_visual, x, y, x + w, y + h);}
inline void GGIDrawable::draw_box(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{ ggiDrawBox(_visual, x, y, w, h);}

inline void GGIDrawable::put_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Pixel p)
{ ggiPutPixel(_visual, x, y, p);}
inline void GGIDrawable::put_hline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, void *p)
{ ggiPutHLine(_visual, x, y, w, p);}
inline void GGIDrawable::put_vline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord h, void *p)
{ ggiPutVLine(_visual, x, y, h, p);}
inline void GGIDrawable::draw_pixels(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h, void *p)
{ ggiPutBox(_visual, x, y, w, h, p);}

inline void GGIDrawable::blit(Warsaw::PixelCoord x1, Warsaw::PixelCoord y1,
			      Warsaw::PixelCoord w, Warsaw::PixelCoord h,
			      Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{
  ggiCopyBox(_visual, x1, y1, w, h, x2, y2);
}
inline void GGIDrawable::blit(const GGIDrawable &d,
			      Warsaw::PixelCoord x1, Warsaw::PixelCoord y1,
			      Warsaw::PixelCoord w, Warsaw::PixelCoord h,
			      Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{
  ggiCrossBlit(d._visual, x1, y1, w, h, _visual, x2, y2);
}
inline void GGIDrawable::blit(Warsaw::Drawable_ptr d,
			      Warsaw::PixelCoord x1, Warsaw::PixelCoord y1,
			      Warsaw::PixelCoord w, Warsaw::PixelCoord h,
			      Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{
  DrawableTie<GGIDrawable> *servant = GGIConsole::reference_to_servant(d);
  if (servant) blit(servant->impl(), x1, y1, w, h, x2, y2);
  else Logger::log(Logger::drawing) << "GGIDrawable::blit: unable to obtain servant from reference" << endl;
}

inline void GGIDrawable::flush() { ggiFlush(_visual);}
inline void GGIDrawable::flush(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{ ggiFlushRegion(_visual, x, y, w, h);}

#endif
