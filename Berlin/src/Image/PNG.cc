/*$Id: PNG.cc,v 1.9 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
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

#include <Warsaw/config.hh>
#include "Image/PNG.hh"
#include <Berlin/Logger.hh>
#include <Prague/Sys/Memory.hh>
#include <Prague/Sys/Tracer.hh>
#include <iostream>
#include <fstream>
#include <streambuf.h>
#include <png.h>

using namespace Prague;
using namespace Warsaw;

class PNG::ibuf : public std::streambuf
{
public:
  ibuf(const Raster::Data &data)
    {
      char *begin = (char *)data.get_buffer();
      char *end   = begin + data.length();
      setg(begin, begin, end);
    }
};

class PNG::obuf : public std::streambuf
{
public:
  unsigned char *data() const
    {
      unsigned char *ret = new unsigned char[_buf.size()];
      Prague::Memory::copy(&*_buf.begin(), ret, _buf.size());
      return ret;
    }
  std::streamsize  length() const { return _buf.size();}
  std::streamsize  xsputn(const char *s, std::streamsize n)
    {
      _buf.insert(_buf.end(), s, s + n);
      return n;
    }
private:
  std::vector<unsigned char> _buf;
};

class PNG::Decoder 
{
  static const size_t magic = 8;
public:
  Decoder(std::streambuf *, png_structp, png_infop, png_infop);
  operator bool () const { return _valid;}
  unsigned char **decode();
private:
  static void read(png_structp, png_bytep, png_size_t);
  static void warning(png_structp, png_const_charp);
  static void error(png_structp, png_const_charp);
  std::streambuf *_input;
  bool            _valid;
  png_structp     _png;
  png_infop       _info;
  png_infop       _end;
};

class PNG::Encoder 
{
public:
  Encoder(std::streambuf *, png_structp, png_infop, png_infop);
  void encode(unsigned char *const *);
private:
  static void write(png_structp, png_bytep, png_size_t);
  static void flush(png_structp);
  static void warning(png_structp, png_const_charp);
  static void error(png_structp, png_const_charp);
  streambuf  *_output;
  png_structp _png;
  png_infop   _info; 
  png_infop   _end;
};

inline PNG::Encoder::Encoder(std::streambuf *sb, png_structp p, png_infop i, png_infop e)
  : _output(sb), _png(p), _info(i), _end(e)
{
  png_set_write_fn (_png, _output, &PNG::Encoder::write, &PNG::Encoder::flush);
  png_set_error_fn(_png, _output, &PNG::Encoder::error, &PNG::Encoder::warning);
  png_set_write_status_fn(_png, 0);
}

inline void PNG::Encoder::encode(unsigned char *const *rows)
{
  Trace trace("Encoder::encode");
  png_write_info(_png, _info);
  png_write_image(_png, const_cast<unsigned char **>(rows));
  png_write_end(_png, _end);
}

inline void PNG::Encoder::write(png_structp png_ptr, png_bytep image, png_size_t length) 
{
  std::streambuf *sbuf = static_cast<std::streambuf *>(png_ptr->io_ptr);
  sbuf->sputn((char*)image, (size_t)length);
}

inline void PNG::Encoder::flush(png_structp png_ptr) 
{
  std::streambuf *sbuf = static_cast<std::streambuf *>(png_ptr->io_ptr);
#ifdef __GLIBCPP__
  sbuf->pubsync();
#else
  sbuf->sync();
#endif
}

inline void PNG::Encoder::warning(png_structp, png_const_charp msg)
{
  Logger::log(Logger::corba) << "PNG::Encoder::warning : " << msg << std::endl;
}

inline void PNG::Encoder::error(png_structp, png_const_charp msg)
{
  Logger::log(Logger::corba) << "PNG::Encoder::error : " << msg << std::endl;
}

inline PNG::Decoder::Decoder(streambuf *sbuf, png_structp p, png_infop i, png_infop e)
  : _input(sbuf), _valid(false), _png(p), _info(i), _end(e)
{
  png_byte header[magic];
  _input->sgetn((char*)header, magic);
  _valid = !(png_sig_cmp(header, 0, magic));
  if (_valid)
    {
      png_set_sig_bytes(_png, magic);
      png_set_read_fn(_png, _input, &PNG::Decoder::read);
      png_set_error_fn(_png, _input, &PNG::Decoder::error, &PNG::Decoder::warning);
      png_set_read_status_fn(_png, 0);
      png_read_info(_png, _info);
    }
}

inline unsigned char **PNG::Decoder::decode()

{
  Trace trace("PNGDecoder::decode");
  if (!_valid)
    {
      std::cerr << "PNG::Decoder::decode : invalid raster !" << std::endl;
      return 0;
    }
  png_uint_32 height = png_get_image_height(_png, _info);
  png_uint_32 rowbytes = png_get_rowbytes(_png, _info);
  unsigned char **rows = new unsigned char *[height];
  for (png_uint_32 i = 0; i < height; i++) rows[i] = new unsigned char[rowbytes];
  png_read_image(_png, rows);
  png_read_end(_png, _end);
  return rows;
}

inline void PNG::Decoder::read(png_structp png_ptr, png_bytep image, png_size_t length) 
{
  std::streambuf *input = static_cast<std::streambuf *>(png_ptr->io_ptr);
  input->sgetn((char *)image, (size_t)length);
}

inline void PNG::Decoder::warning(png_structp, png_const_charp msg)
{
  Logger::log(Logger::image) << "PNG::Decoder::warning : " << msg << std::endl;
}

inline void PNG::Decoder::error(png_structp, png_const_charp msg)
{
  Logger::log(Logger::image) << "PNG::Decoder::error : " << msg << std::endl;
}

PNG::PNG()
{
  _rpng = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  _rinfo = png_create_info_struct(_rpng);
  _rend = png_create_info_struct(_rpng);
}

PNG::~PNG()
{
  clear();
  png_destroy_read_struct(&_rpng, &_rinfo, &_rend);
}

void PNG::clear()
{
  png_destroy_read_struct(&_rpng, &_rinfo, &_rend);
  _rpng = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  _rinfo = png_create_info_struct(_rpng);
  _rend = png_create_info_struct(_rpng);  
}

void PNG::header(Raster::Info &info)
{
  info.width = _rinfo->width;
  info.height = _rinfo->height;
  info.depth = _rinfo->bit_depth;
  info.colortype = _rinfo->color_type;
  info.compression = _rinfo->compression_type;
  info.filter = _rinfo->filter_type;
  info.interlace = _rinfo->interlace_type;
}

Raster::Data *PNG::marshal(unsigned char *const *rows)
{
  png_structp wpng = png_create_write_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  png_infop winfo = png_create_info_struct(wpng);
  png_uint_32 width, height;
  int  depth, color, interlace, compression, filter;
  /*
   * transfer the IHDR chunk
   */
  png_get_IHDR(_rpng, _rinfo, &width, &height, &depth, &color, &interlace, &compression, &filter);
  png_set_IHDR(wpng, winfo, width, height, depth, color, interlace, compression, filter);
  /*
   * set up buffer to hold new data
   */
  obuf buffer;
  Encoder encoder(&buffer, wpng, winfo, _rend);
  encoder.encode(rows);
  Raster::Data *data = new Raster::Data(static_cast<CORBA::ULong>(buffer.length()), static_cast<CORBA::ULong>(buffer.length()),
					reinterpret_cast<CORBA::Octet *>(buffer.data()), static_cast<CORBA::Boolean>(true));
  png_destroy_write_struct(&wpng, &winfo);
  return data;
}

unsigned char **PNG::demarshal(const Raster::Data &data)
{
  clear();
  ibuf buffer(data);
  Decoder decoder(&buffer, _rpng, _rinfo, _rend);
  unsigned char **rows = decoder.decode();
  return rows;
}

Color PNG::pixel(unsigned long x, unsigned long y, unsigned char *const *rows)
{
  Color color;
  color.red = color.green = color.blue = 0.; color.alpha = 1.;
  if (x >= _rinfo->width || y >= _rinfo->height)
    {
      std::cerr << "PNG::pixel: illegal index !" << std::endl;
      return color;
    }
  else
    {
      if (_rinfo->color_type != rgbalpha) std::cerr << "wrong color type : " << (int) _rinfo->color_type << std::endl;
      if (_rinfo->bit_depth != 8) std::cerr << "wrong depth : " << (int) _rinfo->bit_depth << std::endl;
      const unsigned char *pixel = rows[y] + 4*x;
      color.red = static_cast<double>(*pixel) / 256;
      color.green = static_cast<double>(*(pixel + 1)) / 256;
      color.blue = static_cast<double>(*(pixel + 2)) / 256;
      color.alpha = static_cast<double>(*(pixel + 3)) / 256;
    }
  return color;
}

void PNG::pixel(unsigned long x, unsigned long y, const Color &color, unsigned char **rows)
{
  if (x >= _rinfo->width || y >= _rinfo->height)
    {
      std::cerr << "RasterImpl::loadPixel: illegal index !" << std::endl;
      return;
    }
  else
    {
      if (_rinfo->color_type != rgbalpha) std::cerr << "wrong color type : " << static_cast<int>(_rinfo->color_type) << std::endl;
      if (_rinfo->bit_depth != 8) std::cerr << "wrong depth : " << static_cast<int>(_rinfo->bit_depth) << endl;
      unsigned char *pixel = rows[y] + 4*x;
      *pixel++ = static_cast<png_byte>(color.red * 256);
      *pixel++ = static_cast<png_byte>(color.green * 256);
      *pixel++ = static_cast<png_byte>(color.blue * 256);
      *pixel = static_cast<png_byte>(color.alpha * 256);
    }
}

Raster::ColorSeq *PNG::pixels(unsigned long xlower, unsigned long ylower, unsigned long xupper, unsigned long yupper,
			      unsigned char *const *rows)
{
  if (xupper < xlower || yupper < ylower ||
      xupper > _rinfo->width || yupper > _rinfo->height ||
      xlower > _rinfo->width || ylower > _rinfo->height)
    {
      std::cerr << "PNG::pixels: illegal indexes !\n";
      std::cerr << xlower << ' ' << ylower << ' ' << xupper << ' ' << yupper
	   << " not contained in " << ' ' << _rinfo->width << 'x' << _rinfo->height << std::endl;
      return 0;
    }
  png_uint_32 width = xupper - xlower;
  png_uint_32 height = yupper - ylower;
  
  Raster::ColorSeq *colors = new Raster::ColorSeq;
  colors->length(width*height);
  
  const unsigned char *row = 0;
  unsigned char *buffer = 0;
  if (_rinfo->color_type != rgbalpha) row = buffer = new unsigned char[_rinfo->width*_rinfo->height*4];
  for (png_uint_32 y = ylower, i = 0; y != yupper; y++, i++)
    {
      switch (_rinfo->color_type)
	{
	case palette: palette_to_rgbalpha(rows[y], rows[y] + _rinfo->rowbytes, buffer); break;
	case gray: gray_to_rgbalpha(rows[y], rows[y] + _rinfo->rowbytes, buffer); break;
	case grayalpha: grayalpha_to_rgbalpha(rows[y], rows[y] + _rinfo->rowbytes, buffer); break;
	case rgb: rgb_to_rgbalpha(rows[y], rows[y] + _rinfo->rowbytes, buffer); break;
	default: row = rows[y];
	}
      for (png_uint_32 x = xlower, j = 0; x != xupper; x++, j++)
	{
	  const unsigned char *pixel = row + 4*x;
	  Color &color = (*colors)[i*width + j];
	  color.red = static_cast<double>(*pixel) / 256;
	  color.green = static_cast<double>(*(pixel + 1)) / 256;
	  color.blue = static_cast<double>(*(pixel + 2)) / 256;
	  color.alpha = static_cast<double>(*(pixel + 3)) / 256;
	}
    }
  delete [] buffer;
  return colors;
}

void PNG::pixels(unsigned long xlower, unsigned long ylower, unsigned long xupper, unsigned long yupper,
		 const Raster::ColorSeq &pixels, unsigned char **rows)
{
  std::cerr << "sorry, PNG::pixels not yet implemented" << std::endl;
}

unsigned char **PNG::read(const std::string &file)
{
  unsigned char **rows = 0;
  std::ifstream ifs(file.c_str());
  if (!ifs) cerr << "PNG : file " << file << " unreadable" << std::endl;
  else
    {
      Decoder decoder(ifs.rdbuf(), _rpng, _rinfo, _rend);
      rows = decoder.decode();
    }
  return rows;
}

void PNG::write(const std::string &file, unsigned char *const *rows)
{
  png_structp wpng = png_create_write_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  png_infop winfo = png_create_info_struct(wpng);
  png_uint_32 width, height;
  int depth, color, interlace, compression, filter;

  /*
   * transfer the IHDR chunk
   */
  png_get_IHDR(_rpng, _rinfo, &width, &height, &depth, &color, &interlace, &compression, &filter);
  png_set_IHDR(wpng, winfo, width, height, depth, color, interlace, compression, filter);

  std::ofstream ofs(file.c_str());
  Encoder encoder(ofs.rdbuf(), wpng, winfo, _rend);
  encoder.encode(rows);
  png_destroy_write_struct(&wpng, &winfo);
}

void PNG::palette_to_rgbalpha(const unsigned char *begin, const unsigned char *end, unsigned char *to)
//. expands palette png into rgba
{
  if (_rinfo->color_type != PNG_COLOR_TYPE_PALETTE) return;
  size_t length = end - begin;
  switch (_rinfo->bit_depth)
    {
    case 1:
      {
	const unsigned char *sp = begin + ((length - 1) >> 3);
	unsigned char *dp = to + length - 1;
	int shift = 7 - (int)((length + 7) & 7);
	for (size_t i = 0; i < length; i++)
	  {
	    if ((*sp >> shift) & 0x1) *dp = 1;
	    else *dp = 0;
	    if (shift == 7)
	      {
		shift = 0;
		sp--;
	      }
	    else shift++;
	    dp--;
	  }
	break;
      }
    case 2:
      {
	const unsigned char *sp = begin + ((length - 1) >> 2);
	unsigned char *dp = to + length - 1;
	int shift = (3 - ((length + 3) & 3)) << 1;
	for (size_t i = 0; i < length; i++)
	  {
	    unsigned char value = (*sp >> shift) & 0x3;
	    *dp = value;
	    if (shift == 6)
	      {
		shift = 0;
		sp--;
	      }
	    else shift += 2;
	    dp--;
	  }
	break;
      }
    case 4:
      {
	const unsigned char *sp = begin + ((length - 1) >> 1);
	unsigned char *dp = to + length - 1;
	int shift = (length & 1) << 2;
	for (size_t i = 0; i < length; i++)
	  {
	    char value = (*sp >> shift) & 0xf;
	    *dp = value;
	    if (shift == 4)
	      {
		shift = 0;
		sp--;
	      }
	    else shift += 4;
	    dp--;
	  }
	break;
      }
    case 8:
      {
	Memory::copy(begin, to, length);
	break;
      }
    }
  const unsigned char *sp = to + length - 1;
  unsigned char *dp = to + (length << 2) - 1;
  for (unsigned long i = 0; i < length; i++)
    {
      if ((int)(*sp) >= _rpng->num_trans) *dp-- = 0xff;
      else *dp-- = _rpng->trans[*sp];
      *dp-- = _rpng->palette[*sp].blue;
      *dp-- = _rpng->palette[*sp].green;
      *dp-- = _rpng->palette[*sp].red;
      sp--;
    }
}

void PNG::gray_to_rgbalpha(const unsigned char *begin, const unsigned char *end, unsigned char *to)
{
  if (_rinfo->color_type != gray) return;
  if (_rinfo->bit_depth == 8)
    for (; begin < end; begin++)
      {
	*(to++) = *begin;//red
	*(to++) = *begin;//green
	*(to++) = *begin;//blue
	*(to++) =  0xff; //alpha
      }
  else
    for (; begin < end; begin += 2)// 16 bit
      {
	*(to++) = *begin;
	*(to++) = *(begin + 1);
	*(to++) = *begin;
	*(to++) = *(begin + 1);
	*(to++) = *(begin++);
	*(to++) = *(begin++);
	*(to++) =  0xff;
	*(to++) =  0xff;
      }
}

void PNG::grayalpha_to_rgbalpha(const unsigned char *begin, const unsigned char *end, unsigned char *to)
//. expands gray alpha png into rgba
{
  if (_rinfo->color_type != grayalpha) return;
  if (_rinfo->bit_depth == 8)
    for (; begin != end; begin++)
      {
	*(to++) = *begin;  // red
	*(to++) = *begin;  // green
	*(to++) = *begin++;// blue
	*(to++) = *begin;  // alpha
      }
  else
    for (; begin < end; begin++)
      {
	*(to++) = *begin;       // red
	*(to++) = *(begin + 1); // red
	*(to++) = *begin;       // green
	*(to++) = *(begin + 1); // green
	*(to++) = *(begin++);   // blue
	*(to++) = *(begin++);   // blue
	*(to++) = *(begin++);   // alpha
	*(to++) = *begin;       // alpha
      }
}

void PNG::rgb_to_rgbalpha(const unsigned char *begin, const unsigned char *end, unsigned char *to)
{
  if (_rinfo->color_type != rgb) return;
  if (_rinfo->bit_depth == 8)
    for (; begin < end; begin++)
      {
	*(to++) = *(begin++);//red
	*(to++) = *(begin++);//green
	*(to++) = *begin;//blue
	*(to++) =  0xff; //alpha
      }
  else
    for (; begin < end; begin++)// 16 bit
      {
	*(to++) = *(begin++);
	*(to++) = *(begin++);
	*(to++) = *(begin++);
	*(to++) = *(begin++);
	*(to++) = *(begin++);
	*(to++) = *begin;
	*(to++) =  0xff;
	*(to++) =  0xff;
      }
}

