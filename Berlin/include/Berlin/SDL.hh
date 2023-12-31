/*$Id: SDL.hh,v 1.9 2001/04/21 23:23:44 sdt Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 2001 Philip Philonenko <philonenko@orgacom.ru> 
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
#ifndef _SDL_hh
#define _SDL_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Input.hh>
#include <Prague/Sys/Tracer.hh>
#include <Berlin/Logger.hh>
#include <vector>
extern "C"
{
#include <SDL.h>
}

class SDLConsole;

////////////////////////////////////////////////////////////////////////////////
// SDLDrawable declaration
////////////////////////////////////////////////////////////////////////////////
class SDLDrawable
{
  friend class SDLConsole;
  friend class DrawableTie<SDLDrawable>;
public:
  SDLDrawable(const char *, Warsaw::PixelCoord = 640, Warsaw::PixelCoord = 480, Warsaw::PixelCoord = 0);
  ~SDLDrawable();

  
  typedef Uint32 Pixel; // Uint32 is SDL color type

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
  void read_pixel(Warsaw::PixelCoord, Warsaw::PixelCoord, Pixel &);
  void read_pixels(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *);
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
  void blit(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void blit(const SDLDrawable &, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void blit(Warsaw::Drawable_ptr, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);

  void flush();
  void flush(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void init() { }
  void finish() { }

  SDL_Surface * surface() { return _surface; };

private:
  SDL_Surface *_surface;
  int          _width;
  int          _height;
  int          _depth;
  int          _color;
  bool         _need_locking;
};


////////////////////////////////////////////////////////////////////////////////
// SDLConsole declaration
////////////////////////////////////////////////////////////////////////////////
class SDLConsole
{
  typedef SDLDrawable Drawable;
  typedef std::vector<DrawableTie<SDLDrawable> *> dlist_t;

public:
  SDLConsole(int &, char **, PortableServer::POA_ptr);
  ~SDLConsole();
  static DrawableTie<Drawable> *drawable();
  static DrawableTie<Drawable> *create_drawable(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  static DrawableTie<Drawable> *create_drawable(Drawable *);
  static Warsaw::Drawable_ptr activate_drawable(DrawableTie<SDLDrawable> *);
  static DrawableTie<Drawable> *reference_to_servant(Warsaw::Drawable_ptr);

  Warsaw::Input::Event *next_event();
  void wakeup();
  void activate_autoplay() { _autoplay = true;}

private:
  Warsaw::Input::Event *synthesize(const SDL_Event &);

  bool   _autoplay;
  long   _size[2];
  long   _position[2];
  double _resolution[2];
  int    _wakeupPipe[2];
  SDL_Surface *  _surface;
  static dlist_t _drawables;
  static PortableServer::POA_var _poa;

};


////////////////////////////////////////////////////////////////////////////////
// SDLDrawable inlines
////////////////////////////////////////////////////////////////////////////////
inline void *SDLDrawable::read_buffer() const
{ 
  Prague::Trace("SDLDrawable::read_buffer");
  return _surface->pixels;
}

inline void *SDLDrawable::write_buffer() const
{ 
  Prague::Trace("SDLDrawable::write_buffer");
  return _surface->pixels;
}

inline Warsaw::PixelCoord SDLDrawable::width() const { 
  return _width;
}

inline Warsaw::PixelCoord SDLDrawable::height() const { 
  return _height;
}

inline Warsaw::PixelCoord SDLDrawable::vwidth() const { 
  return _width;
}

inline Warsaw::PixelCoord SDLDrawable::vheight() const { 
  return _height;
}

inline Warsaw::Coord SDLDrawable::resolution(Warsaw::Axis a) const
{
  return 0.1; // hack, see below
  //  return a == Warsaw::xaxis ?
  //    0.1 * 1.0:
  //    0.1 * 1.0; // FIXME: get some physical size in there.
}

inline Warsaw::Coord SDLDrawable::dpi(Warsaw::Axis a) const
{
  return resolution(a) * 254.0;
}


inline Warsaw::PixelCoord SDLDrawable::row_length() const
{ 
  // return bytes-per-row
  return _width * _depth;
}

inline SDLDrawable::Pixel SDLDrawable::map(const Warsaw::Color &c) const
{
  double scale = 0xff;
  return SDL_MapRGBA( _surface->format, 
		      static_cast<Uint8>(c.red * scale), 
		      static_cast<Uint8>(c.green * scale),
		      static_cast<Uint8>(c.blue * scale),
                      static_cast<Uint8>(c.alpha * scale)
		    );
}

inline void SDLDrawable::set_color(Pixel c)
{
  _color = c;
}

inline void SDLDrawable::draw_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y)
{
  put_pixel( x, y, _color );
}

inline void SDLDrawable::draw_hline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w)
{
  //  draw_line(x,y,w,0);
  for (int i = 0; i < w; i++) put_pixel(x + i, y, _color);
}

inline void SDLDrawable::draw_vline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord h)
{
  //  draw_line(x,y,0,h);
  for (int i = 0; i < h; i++) put_pixel(x, y + h, _color);
}

inline void SDLDrawable::put_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Pixel c)
{
  if (x >= _width || y >= _height || x < 0 || y < 0) return;
  if (_need_locking) { SDL_LockSurface(_surface); }

  Uint8* p = (Uint8 *)_surface->pixels + y * _surface->pitch + x * _depth;

  switch( _depth ) {
  case 1: *p = c; break;
  case 2: *(Uint16 *)p = c; break;
  case 3: {
    if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
      p[0] = (c >> 16) & 0xff;
      p[1] = (c >> 8) & 0xff;
      p[2] = c & 0xff;
    } else {
      p[0] = c & 0xff;
      p[1] = (c >> 8) & 0xff;
      p[2] = (c >> 16) & 0xff;
    }
    break;
  }
  case 4: *(Uint32 *)p = c; break;
  }

  if (_need_locking) { SDL_UnlockSurface(_surface); }
}

inline void SDLDrawable::put_hline(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *)
{
  Prague::Trace("SDLDrawable::put_hline");
}

inline void SDLDrawable::put_vline(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *)
{
  Prague::Trace("SDLDrawable::put_vline");
}

inline void SDLDrawable::draw_pixels(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, void *)
{
  Prague::Trace("SDLDrawable::draw_pixels");
}

inline void SDLDrawable::blit(Warsaw::PixelCoord x1, Warsaw::PixelCoord y1, 
			      Warsaw::PixelCoord w, Warsaw::PixelCoord h, 
			      Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{
  Prague::Trace("SDLDrawable::blit: surface-surface");
  SDL_Rect r1,r2;
  r1.x = x1; r1.y = y1;
  r1.w = w; r1.h = h;
  r2.x = x2; r2.y = y2;
  r2.w = w; r2.h = h;
  if (r1.x < 0) { r1.w += r1.x; r1.x = 0; } if (r2.x < 0) { r2.w += r2.x; r2.x = 0; }
  if (r1.y < 0) { r1.h += r1.y; r1.y = 0; } if (r2.y < 0) { r2.h += r2.y; r2.y = 0; }
  SDL_BlitSurface(_surface, &r1, _surface, &r2);
}

inline void SDLDrawable::blit(const SDLDrawable &src, Warsaw::PixelCoord x1, Warsaw::PixelCoord y1, 
			      Warsaw::PixelCoord w, Warsaw::PixelCoord h,
			      Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{
  Prague::Trace("SDLDrawable::blit: other-surface");
  SDL_Rect r1,r2;
  r1.x = x1; r1.y = y1;
  r1.w = w; r1.h = h;
  r2.x = x2; r2.y = y2;
  r2.w = w; r2.h = h;
  if (r1.x < 0) { r1.w += r1.x; r1.x = 0; } if (r2.x < 0) { r2.w += r2.x; r2.x = 0; }
  if (r1.y < 0) { r1.h += r1.y; r1.y = 0; } if (r2.y < 0) { r2.h += r2.y; r2.y = 0; }
  SDL_BlitSurface(src._surface, &r1, _surface, &r2);
}

inline void SDLDrawable::blit(Warsaw::Drawable_ptr d, 
			      Warsaw::PixelCoord x1, Warsaw::PixelCoord y1, 
			      Warsaw::PixelCoord w, Warsaw::PixelCoord h, 
			      Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{
  Prague::Trace("SDLDrawable::blit 3");
  DrawableTie<SDLDrawable> *servant = SDLConsole::reference_to_servant(d);
  if (servant) blit(servant->impl(), x1, y1, w, h, x2, y2);
  else Logger::log(Logger::drawing) << "SDLDrawable::blit: unable to obtain servant from reference" << endl;
}

inline void SDLDrawable::flush()
{
  Prague::Trace("SDLDrawable::flush 1");
  SDL_UpdateRect( _surface, 0, 0, _width, _height );
}

inline void SDLDrawable::flush(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{
  Prague::Trace("SDLDrawable::flush 2");
  if (x < 0) { w += x; x = 0; }
  if (y < 0) { h += y; y = 0; }
  SDL_UpdateRect( _surface, x, y, w, h );
}


#endif
