/*$Id: PNG.cc,v 1.2 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#include "Image/PNG.hh"
#include "Prague/Sys/Memory.hh"
#include <iostream>
#include <fstream>
#include <streambuf.h>
#include <png.h>

using namespace Prague;

class PNG::ibuf : public streambuf
{
public:
  ibuf(const Raster::Data &data)
    {
      char *begin = (char *)data.NP_data(); // soon to be replaced with get_buffer()  -stefan
      char *end   = begin + data.length();
      setg (begin, begin, end);
    }
};

class PNG::obuf : public streambuf
{
public:
  unsigned char *data() const
    {
      unsigned char *ret = new unsigned char[buf.size()];
      Prague::Memory::copy(buf.begin(), ret, buf.size());
      return ret;
    }
  streamsize  length() const { return buf.size();}
  streamsize  xsputn(const char *s, streamsize n)
    {
      buf.insert(buf.end(), s, s + n);
      return n;
    }
private:
  vector<unsigned char> buf;
};

class PNG::Decoder 
{
  static const size_t magic = 8;
public:
  Decoder(streambuf *, png_structp, png_infop, png_infop);
  operator bool () const { return valid;}
  unsigned char **decode();
private:
  static void read(png_structp, png_bytep, png_size_t);
  static void warning(png_structp, png_const_charp);
  static void error(png_structp, png_const_charp);
  streambuf *input;
  bool valid;
  png_structp png;
  png_infop   info, end;
};

class PNG::Encoder 
{
public:
  Encoder(streambuf *, png_structp, png_infop, png_infop);
  void encode(unsigned char *const *);
private:
  static void write(png_structp, png_bytep, png_size_t);
  static void flush(png_structp);
  static void warning(png_structp, png_const_charp);
  static void error(png_structp, png_const_charp);
  streambuf *output;
  png_structp png;
  png_infop info; 
  png_infop end;
};

inline PNG::Encoder::Encoder(streambuf *sb, png_structp p, png_infop i, png_infop e)
  : output(sb), png(p), info(i), end(e)
{
  png_set_write_fn (png, output, &PNG::Encoder::write, &PNG::Encoder::flush);
  png_set_error_fn(png, output, &PNG::Encoder::error, &PNG::Encoder::warning);
  png_set_write_status_fn(png, 0);
}

inline void PNG::Encoder::encode(unsigned char *const *rows)
{
  SectionLog section("Encoder::encode");
  png_write_info(png, info);
  png_write_image(png, const_cast<unsigned char **>(rows));
  png_write_end(png, end);
}

inline void PNG::Encoder::write(png_structp png_ptr, png_bytep image, png_size_t length) 
{
  streambuf *sbuf = static_cast<streambuf *>(png_ptr->io_ptr);
  sbuf->xsputn((char*)image, (size_t)length);
}

inline void PNG::Encoder::flush(png_structp png_ptr) 
{
  streambuf *sbuf = static_cast<streambuf *>(png_ptr->io_ptr);
  sbuf->sync();
}

inline void PNG::Encoder::warning(png_structp, png_const_charp msg)
{
  Logger::log(Logger::corba) << "PNG::Encoder::warning : " << msg << endl;
}

inline void PNG::Encoder::error(png_structp, png_const_charp msg)
{
  Logger::log(Logger::corba) << "PNG::Encoder::error : " << msg << endl;
}

inline PNG::Decoder::Decoder(streambuf *sbuf, png_structp p, png_infop i, png_infop e)
  : input(sbuf), valid(false), png(p), info(i), end(e)
{
  png_byte header[magic];
  input->xsgetn((char*)header, magic);
  valid = !(png_sig_cmp(header, 0, magic));
  if (valid)
    {
      png_set_sig_bytes(png, magic);
      png_set_read_fn(png, input, &PNG::Decoder::read);
      png_set_error_fn(png, input, &PNG::Decoder::error, &PNG::Decoder::warning);
      png_set_read_status_fn(png, 0);
      png_read_info(png, info);
    }
}

inline unsigned char **PNG::Decoder::decode()

{
  SectionLog section("PNGDecoder::decode");
  if (!valid)
    {
      cerr << "PNG::Decoder::decode : invalid raster !" << endl;
      return 0;
    }
  png_uint_32 height = png_get_image_height(png, info);
  png_uint_32 rowbytes = png_get_rowbytes(png, info);
  unsigned char **rows = new unsigned char *[height];
  for (png_uint_32 i = 0; i < height; i++) rows[i] = new unsigned char[rowbytes];
  png_read_image(png, rows);
  png_read_end(png, end);
  return rows;
}

inline void PNG::Decoder::read(png_structp png_ptr, png_bytep image, png_size_t length) 
{
  streambuf *input = static_cast<streambuf *>(png_ptr->io_ptr);
  input->xsgetn((char *)image, (size_t)length);
}

inline void PNG::Decoder::warning(png_structp, png_const_charp msg)
{
  Logger::log(Logger::image) << "PNG::Decoder::warning : " << msg << endl;
}

inline void PNG::Decoder::error(png_structp, png_const_charp msg)
{
  Logger::log(Logger::image) << "PNG::Decoder::error : " << msg << endl;
}

PNG::PNG()
{
  rpng = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  rinfo = png_create_info_struct(rpng);
  rend = png_create_info_struct(rpng);
}

PNG::~PNG()
{
  clear();
  png_destroy_read_struct(&rpng, &rinfo, &rend);
}

void PNG::clear()
{
  png_destroy_read_struct(&rpng, &rinfo, &rend);
  rpng = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  rinfo = png_create_info_struct(rpng);
  rend = png_create_info_struct(rpng);  
}

void PNG::header(Raster::Info &info)
{
  info.width = rinfo->width;
  info.height = rinfo->height;
  info.depth = rinfo->bit_depth;
  info.colortype = rinfo->color_type;
  info.compression = rinfo->compression_type;
  info.filter = rinfo->filter_type;
  info.interlace = rinfo->interlace_type;
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
  png_get_IHDR(rpng, rinfo, &width, &height, &depth, &color, &interlace, &compression, &filter);
  png_set_IHDR(wpng, winfo, width, height, depth, color, interlace, compression, filter);
  /*
   * set up buffer to hold new data
   */
  obuf buffer;
  Encoder encoder(&buffer, wpng, winfo, rend);
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
  Decoder decoder(&buffer, rpng, rinfo, rend);
  unsigned char **rows = decoder.decode();
  return rows;
}

Color PNG::pixel(unsigned long x, unsigned long y, unsigned char *const *rows)
{
  Color color;
  color.red = color.green = color.blue = 0.; color.alpha = 1.;
  if (x >= rinfo->width || y >= rinfo->height)
    {
      cerr << "PNG::pixel: illegal index !" << endl;
      return color;
    }
  else
    {
      if (rinfo->color_type != rgbalpha) cerr << "wrong color type : " << (int) rinfo->color_type << endl;
      if (rinfo->bit_depth != 8) cerr << "wrong depth : " << (int) rinfo->bit_depth << endl;
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
  if (x >= rinfo->width || y >= rinfo->height)
    {
      cerr << "RasterImpl::loadPixel: illegal index !" << endl;
      return;
    }
  else
    {
      if (rinfo->color_type != rgbalpha) cerr << "wrong color type : " << (int) rinfo->color_type << endl;
      if (rinfo->bit_depth != 8) cerr << "wrong depth : " << (int) rinfo->bit_depth << endl;
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
      xupper > rinfo->width || yupper > rinfo->height ||
      xlower > rinfo->width || ylower > rinfo->height)
    {
      cerr << "PNG::pixels: illegal indexes !\n";
      cerr << xlower << ' ' << ylower << ' ' << xupper << ' ' << yupper
	   << " not contained in " << ' ' << rinfo->width << 'x' << rinfo->height << endl;
      return 0;
    }
  png_uint_32 width = xupper - xlower;
  png_uint_32 height = yupper - ylower;
  
  Raster::ColorSeq *colors = new Raster::ColorSeq;
  colors->length(width*height);
  
  const unsigned char *row = 0;
//   if (rinfo->color_type != rgbalpha) cerr << "wrong color type : " << (int) rinfo->color_type << endl;
//   if (rinfo->bit_depth != 8) cerr << "wrong depth : " << (int) rinfo->bit_depth << endl;
  unsigned char *buffer = 0;
  if (rinfo->color_type == palette) row = buffer = new unsigned char[rinfo->width*rinfo->height*4];
  for (png_uint_32 y = ylower, i = 0; y != yupper; y++, i++)
    {
      if (rinfo->color_type == palette) expand(rows[y], rows[y] + rinfo->rowbytes, buffer);
      else row = rows[y];
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
  cerr << "sorry, PNG::pixels not yet implemented" << endl;
//   clear();
//   Raster::Info info;
//   info.width = width;
//   info.height = height;
//   info.depth = 8;
//   info.colortype = PNG_COLOR_TYPE_RGB_ALPHA;
//   info.compression = PNG_COMPRESSION_TYPE_BASE;
//   info.filter = PNG_FILTER_TYPE_BASE;
//   info.interlace = PNG_INTERLACE_NONE;
//   png_set_IHDR(rpng, rinfo, info.width, info.height, info.depth, info.colortype, info.interlace, info.compression, info.filter);
//   rows = new png_bytep[height];
//   png_uint_32 rowbytes = (info.width * 32 + 7) >> 3;
//   for (png_uint_32 i = 0; i != info.height; i++)
//     {
//       rows[i] = new png_byte[rowbytes];
//       Prague::Memory::copy(data.NP_data() + i * rowbytes, rows[i], rowbytes);
//     }
}

unsigned char **PNG::read(const string &file)
{
  unsigned char **rows = 0;
  ifstream ifs(file.c_str());
  if (!ifs) cerr << "PNG : file " << file << " unreadable" << endl;
  else
    {
      Decoder decoder(ifs.rdbuf(), rpng, rinfo, rend);
      rows = decoder.decode();
    }
  return rows;
}

void PNG::write(const string &file, unsigned char *const *rows)
{
  png_structp wpng = png_create_write_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
  png_infop winfo = png_create_info_struct(wpng);
  png_uint_32 width, height;
  int depth, color, interlace, compression, filter;

  /*
   * transfer the IHDR chunk
   */
  png_get_IHDR(rpng, rinfo, &width, &height, &depth, &color, &interlace, &compression, &filter);
  png_set_IHDR(wpng, winfo, width, height, depth, color, interlace, compression, filter);

  ofstream ofs(file.c_str());
  Encoder encoder(ofs.rdbuf(), wpng, winfo, rend);
  encoder.encode(rows);
  png_destroy_write_struct(&wpng, &winfo);
}

void PNG::expand(const unsigned char *begin, const unsigned char *end, unsigned char *to)
//. expands palette png into rgba
{
  if (rinfo->color_type != PNG_COLOR_TYPE_PALETTE) return;
  size_t length = end - begin;
  switch (rinfo->bit_depth)
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
      if ((int)(*sp) >= rpng->num_trans) *dp-- = 0xff;
      else *dp-- = rpng->trans[*sp];
      *dp-- = rpng->palette[*sp].blue;
      *dp-- = rpng->palette[*sp].green;
      *dp-- = rpng->palette[*sp].red;
      sp--;
    }
}
