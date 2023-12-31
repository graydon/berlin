/*$Id: Dictionary.cc,v 1.11 1999/11/08 21:27:55 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Tobias Hunger <Tobias_Hunger@gmx.de>
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

#include <Prague/Unicode/Dictionary.hh>
#include <Prague/Sys/Directory.hh>
#include <Prague/Sys/DLL.hh>
#include <Prague/Filter/gzstream.hh>
#include <fstream>
#include <string>
#include <iostream>
#include <iomanip>

using namespace Prague;
using namespace Unicode;

// static variables:
Dictionary       *Dictionary::dictionary = 0;
Dictionary::Guard Dictionary::guard;
Mutex             Dictionary::singletonMutex;

Dictionary *Dictionary::instance()
{
  // Create Dictionary just once
  {
    MutexGuard guard(singletonMutex);
    if (!dictionary) dictionary = new Dictionary;
  }

  return dictionary;
} // instance



Dictionary::Block * Dictionary::find_char(_Char UC)
{
  unsigned short i = 0;
  
  // ASCII happens so often that this line speeds things up a bit
  if (UC > data[i].end)
    {
      // binary search for non-ASCII characters
      unsigned short start = 1; // 0 was allready checked
      unsigned short end = size - 1; 
      i = (start + end) / 2;
      while (i > start && i < end)
	{
	  if (UC < data[i].start) end = i;
	  else if (UC > data[i].end) start = i;
	  else break; // we found it...
	  i = (start + end) / 2;
	}
    }
  if ( !(UC >= data[i].start && UC <= data[i].end)) // UC belongs to no block
    throw UndefinedProperty(UC, Unicode::PROP_CHARACTER);
  if (data[i].block == 0) { // need to dynamically load the relevant skript
    if (data[i].file == "") // Block is unsupported
      throw BlockError(data[i].start, data[i].end, "Block is not supported.");
    data[i].block = new Prague::Plugin<Dictionary::Block>(data[i].file);
    if(*(data[i].block) == 0) // Failed to load the plugin for this block
      throw BlockError(data[i].start, data[i].end, data[i].block->error());
  }

  return *(data[i].block);
}; // Dictionary::find_char


void Dictionary::find_next_in_file(ifstream &file)
{
  char c;
  while ((c = file.get()) == ' ' ||
	 c == '\t' ||
	 c == '\n' ||
	 c == '#')
    {
      if (c == '#')
	{
	  while ( (c=file.get()) != '\n' )
	    ;
	  file.putback(c);
	}
    }
  file.putback(c);
} // find_next_in_file


void Dictionary::update(const string &dir)
{
  string tmp;

  MutexGuard guard(mutex);
  clean();

  string file = dir;
  file += "UnicodeBlocks.txt";

  ifstream blocks(file.c_str());
  if (!blocks)
    throw(FileError(file, "Can´t open file."));
  // HEADER
  find_next_in_file(blocks);
  blocks >> size;

  data = new Data[size];
  // BODY
  find_next_in_file(blocks);
  
  for (size_t i = 0; i < size; i++)
    {
      blocks >> hex >> data[i].start;
      blocks >> hex >> data[i].end;
      blocks >> tmp;

      if (tmp == "unsupported")
	data[i].file = "";
      else
	data[i].file = plugindir + tmp;
      
      data[i].canRemove = 0;
      data[i].block = 0;
    }
  find_next_in_file(blocks);
  
  // FOOTER
  // FIXME: Add version string support
} // update_dictionary



Dictionary::Dictionary()
  : data(0), size(0)
{
  my_version.resize(1);
  my_version[0] = NULL_UNICODE;
  char *env = getenv("BERLIN_ROOT");
  if (!env)
    {
      cerr << "Please set environment variable BERLIN_ROOT first" << endl;
      exit(-1);
    }
  plugindir = env;
  plugindir += "/etc/Unicode/";
  update(plugindir);
} // Dictionary::Dictionary


Dictionary::~Dictionary() {
  clean();
} // Dictionary::~Dictionary()

void Dictionary::clean() {
  for (size_t i = 0; i < size; i++)
    delete data[i].block;
  delete [] data;
};
