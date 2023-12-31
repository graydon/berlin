/*$Id: Console.hh,v 1.8 2001/04/23 21:14:44 tobias Exp $
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
#ifndef _Console_hh
#define _Console_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Input.hh>
#include <Warsaw/Drawable.hh>
#include <Berlin/config.hh>

template <typename T>
class DrawableTie : public virtual POA_Warsaw::Drawable,
		    public virtual PortableServer::RefCountServantBase
{
public:
  typedef typename T::Pixel Pixel;
  typedef Warsaw::Drawable::PixelFormat PixelFormat;
  typedef Warsaw::Drawable::BufferFormat BufferFormat;
  DrawableTie(T *);
  ~DrawableTie();
  PixelFormat pixel_format();
  BufferFormat buffer_format();
  Warsaw::PixelCoord width() const;
  Warsaw::PixelCoord height() const;
  Warsaw::PixelCoord vwidth() const;
  Warsaw::PixelCoord vheight() const;
  Warsaw::Coord resolution(Warsaw::Axis) const;
  Warsaw::Coord dpi(Warsaw::Axis) const;
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
  void blit(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void blit(const DrawableTie &,
	    Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  void blit(Warsaw::Drawable_ptr,
	    Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);

  void flush();
  void flush(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);

  /**
   * Init and finish hooks called by the server when the scene is
   * about to be drawn, and when it has been drawn, respectively. This
   * is a suitable place to add calls for building display lists, for
   * example (this is what is done in the GLUT and CAVELib consoles).
   **/
  void init() { t->init(); }
  void finish() { t->finish(); }
    
  T &impl() { return *t;}
private:
  DrawableTie(const DrawableTie &);
  DrawableTie &operator = (const DrawableTie &);
  T *t;
};

template <typename T>
class ConsoleTie
{
  struct Reaper { ~Reaper() { delete ConsoleTie::t;}};
  friend struct Reaper;
public:
  typedef DrawableTie<typename T::Drawable> Drawable;
  ConsoleTie();
  ~ConsoleTie();
  static void open(int &argc, char **argv, PortableServer::POA_ptr);
  static DrawableTie<typename T::Drawable> *drawable();
  static DrawableTie<typename T::Drawable> *create_drawable(Warsaw::PixelCoord, Warsaw::PixelCoord, Warsaw::PixelCoord);
  static DrawableTie<typename T::Drawable> *create_drawable(typename T::Drawable *);
  static Warsaw::Drawable_ptr activate_drawable(DrawableTie<typename T::Drawable> *);
  static DrawableTie<typename T::Drawable> *reference_to_servant(Warsaw::Drawable_ptr);

  static Warsaw::Input::Event *next_event();
  static void wakeup();
  static void activate_autoplay();
private:
  static T *t;
  static Reaper reaper;
};

template <typename T>
inline DrawableTie<T>::DrawableTie(T *tt) : t(tt)
{}

template <typename T>
inline DrawableTie<T>::~DrawableTie()
{ delete t;}

template <typename T>
inline Warsaw::Drawable::PixelFormat DrawableTie<T>::pixel_format()
{ return t->pixel_format();}

template <typename T>
inline Warsaw::Drawable::BufferFormat DrawableTie<T>::buffer_format()
{ return t->buffer_format();}

template <typename T>
inline Warsaw::PixelCoord DrawableTie<T>::width() const
{ return t->width();}

template <typename T>
inline Warsaw::PixelCoord DrawableTie<T>::height() const
{ return t->height();}

template <typename T>
inline Warsaw::PixelCoord DrawableTie<T>::vwidth() const
{ return t->vwidth();}

template <typename T>
inline Warsaw::PixelCoord DrawableTie<T>::vheight() const
{ return t->vheight();}

template <typename T>
inline Warsaw::Coord DrawableTie<T>::resolution(Warsaw::Axis a) const
{ return t->resolution(a);}

template <typename T>
inline Warsaw::Coord DrawableTie<T>::dpi(Warsaw::Axis a) const
{ return t->dpi(a);}

template <typename T>
inline Warsaw::PixelCoord DrawableTie<T>::row_length() const
{ return t->row_length();}

template <typename T>
inline DrawableTie<T>::Pixel DrawableTie<T>::map(const Warsaw::Color &c) const
{ return t->map(c);}

template <typename T>
inline void *DrawableTie<T>::read_buffer() const
{ return t->read_buffer();}

template <typename T>
inline void *DrawableTie<T>::write_buffer() const
{ return t->write_buffer();}

template <typename T>
inline void DrawableTie<T>::read_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Pixel &p) const
{ t->read_pixel(x, y, p);}

template <typename T>
inline void DrawableTie<T>::read_pixels(Warsaw::PixelCoord x, Warsaw::PixelCoord y,
					Warsaw::PixelCoord w, Warsaw::PixelCoord h, void *p) const
{ t->read_pixels(x, y, w, h, p);}

template <typename T>
inline void DrawableTie<T>::set_color(Pixel p)
{ t->set_color(p);}

template <typename T>
inline void DrawableTie<T>::draw_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y)
{ t->draw_pixel(x, y);}

template <typename T>
inline void DrawableTie<T>::draw_hline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w)
{ t->draw_hline(x, y, w);}

template <typename T>
inline void DrawableTie<T>::draw_vline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord h)
{ t->draw_vline(x, y, h);}

template <typename T>
inline void DrawableTie<T>::draw_line(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{ t->draw_line(x, y, w, h);}

template <typename T>
inline void DrawableTie<T>::draw_box(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{ t->draw_box(x, y, w, h);}

template <typename T>
inline void DrawableTie<T>::put_pixel(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Pixel p)
{ t->put_pixel(x, y, p);}

template <typename T>
inline void DrawableTie<T>::put_hline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, void *p)
{ t->put_hline(x, y, w, p);}

template <typename T>
inline void DrawableTie<T>::put_vline(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord h, void *p)
{ t->put_vline(x, y, h, p);}

template <typename T>
inline void DrawableTie<T>::draw_pixels(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h, void *p)
{ t->draw_pixels(x, y, w, h, p);}

template <typename T>
inline void DrawableTie<T>::blit(Warsaw::PixelCoord x1, Warsaw::PixelCoord y1,
				 Warsaw::PixelCoord w, Warsaw::PixelCoord h,
				 Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{ t->blit(x1, y1, w, h, x2, y2);}

template <typename T>
inline void DrawableTie<T>::blit(const DrawableTie &d,
				 Warsaw::PixelCoord x1, Warsaw::PixelCoord y1,
				 Warsaw::PixelCoord w, Warsaw::PixelCoord h,
				 Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{ t->blit(*d.t, x1, y1, w, h, x2, y2);}

template <typename T>
inline void DrawableTie<T>::blit(Warsaw::Drawable_ptr d,
				 Warsaw::PixelCoord x1, Warsaw::PixelCoord y1,
				 Warsaw::PixelCoord w, Warsaw::PixelCoord h,
				 Warsaw::PixelCoord x2, Warsaw::PixelCoord y2)
{ t->blit(d, x1, y1, w, h, x2, y2);}

template <typename T>
inline void DrawableTie<T>::flush()
{ t->flush();}

template <typename T>
inline void DrawableTie<T>::flush(Warsaw::PixelCoord x, Warsaw::PixelCoord y, Warsaw::PixelCoord w, Warsaw::PixelCoord h)
{ t->flush(x, y, w, h);}

template <typename T>
inline ConsoleTie<T>::ConsoleTie() {}

template <typename T>
inline ConsoleTie<T>::~ConsoleTie() {}

template <typename T>
inline void ConsoleTie<T>::open(int &argc, char **argv, PortableServer::POA_ptr poa)
{ if (!t) t = new T(argc, argv, poa);}

template <typename T>
inline DrawableTie<typename T::Drawable> *ConsoleTie<T>::drawable()
 { return T::drawable();}

template <typename T>
inline DrawableTie<typename T::Drawable> *ConsoleTie<T>::create_drawable(Warsaw::PixelCoord w, Warsaw::PixelCoord h, Warsaw::PixelCoord d)
{ return T::create_drawable(w, h, d);}

template <typename T>
inline DrawableTie<typename T::Drawable> *ConsoleTie<T>::create_drawable(typename T::Drawable *d)
{ return T::create_drawable(d);}

template <typename T>
Warsaw::Drawable_ptr ConsoleTie<T>::activate_drawable(DrawableTie<typename T::Drawable> *d)
{ return T::activate_drawable(d);}

template <typename T>
inline DrawableTie<typename T::Drawable> *ConsoleTie<T>::reference_to_servant(Warsaw::Drawable_ptr d)
{ return T::reference_to_servant(d);}

template <typename T>
inline Warsaw::Input::Event *ConsoleTie<T>::next_event()
{ return t->next_event();}

template <typename T>
inline void ConsoleTie<T>::wakeup()
{ t->wakeup();}

template <typename T>
inline void ConsoleTie<T>::activate_autoplay()
{ t->activate_autoplay();}

#if defined(CONSOLE_GGI)
#  include <Berlin/GGI.hh>

typedef ConsoleTie<GGIConsole> Console;

#elif defined(CONSOLE_SDL)
#  include <Berlin/SDL.hh>

typedef ConsoleTie<SDLConsole> Console;

#elif defined(CONSOLE_CAVELIB)
#  include <Berlin/CAVE.hh>

typedef ConsoleTie<CAVEConsole> Console;

#elif defined(CONSOLE_GLUT)
#  include <Berlin/GLUT.hh>

typedef ConsoleTie<GLUTConsole> Console;

#elif defined(CONSOLE_DIRECTFB)
#  include <Berlin/DirectFB.hh>

typedef ConsoleTie<DirectFBConsole> Console;

#else
#  warning "no console type defined"
#endif

#endif /* _Console_hh */
