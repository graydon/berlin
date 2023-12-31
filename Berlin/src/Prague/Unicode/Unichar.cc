/*$Id: Unichar.cc,v 1.6 1999/11/08 22:14:54 tobias Exp $
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
#include <Prague/Unicode/Unichar.hh>

// QUERIES:

Unicode::Char Unicode::Char::uppercase() const {
  return Unicode::Dictionary::instance()->uppercase(my_unicode);
} // uppercase

Unicode::Char Unicode::Char::lowercase() const {
  return Unicode::Dictionary::instance()->lowercase(my_unicode);
} // lowercase

Unicode::Char Unicode::Char::titlecase() const {
  return Unicode::Dictionary::instance()->titlecase(my_unicode);
} // titlecase

float Unicode::Char::numericValue() const {
  return Unicode::Dictionary::instance()->numericValue(my_unicode);
} // numericValue

int Unicode::Char::digitValue() const {
  return Unicode::Dictionary::instance()->digitValue(my_unicode);
} // digitValue

int Unicode::Char::decDigitValue() const {
  return Unicode::Dictionary::instance()->decDigitValue(my_unicode);
} // declDigitValue

string Unicode::Char::blockname() const {
  return Unicode::Dictionary::instance()->blockname(my_unicode);
} // blockname

Unicode::Gen_Cat Unicode::Char::category() const {
  return Unicode::Dictionary::instance()->category(my_unicode);
} // category

Unicode::Bidir_Props Unicode::Char::direction() const {
  return Unicode::Dictionary::instance()->bidirProps(my_unicode);
} // direction

Unicode::Can_Comb_Class Unicode::Char::combClass() const {
  return Unicode::Dictionary::instance()->combClass(my_unicode);
} // CombClass

Unicode::Char_Decomp Unicode::Char::decompType() const {
  return Unicode::Dictionary::instance()->decompType(my_unicode);
} // decompType

Unicode::String Unicode::Char::decompString() const {
  return Unicode::Dictionary::instance()->decompString(my_unicode);
} // decompString

bool Unicode::Char::mustMirror() const {
  return Unicode::Dictionary::instance()->mustMirror(my_unicode);
} // MustMirror

Unicode::EA_Width Unicode::Char::EAWidth() const {
  return Unicode::Dictionary::instance()->EAWidth(my_unicode);
} // EAWidth

Unicode::Line_Break Unicode::Char::linebreak() const {
  return Unicode::Dictionary::instance()->linebreak(my_unicode);
} // LineBreak

// TESTS:

bool Unicode::Char::is_lowercase() const {
  return (this->category() == Unicode::CAT_Ll);
} // is_lowercase

bool Unicode::Char::is_uppercase() const {
  return (this->category() == Unicode::CAT_Lu);
} // is_uppercase

bool Unicode::Char::is_titlecase() const {
  return (this->category() == Unicode::CAT_Lt);
} // is_titlecase

bool Unicode::Char::is_digit() const {
  return (this->category() == Unicode::CAT_Nd);
} // is_digit

bool Unicode::Char::is_defined() const {
  try {
    this->category();
    return 1;
  } 
  catch (Unicode::UndefinedProperty) {
    return 0;
  }
  catch (Unicode::BlockError) {
    return 1;
  }
} // is_defined

bool Unicode::Char::is_supported() const {
  try {
    this->category();
    return 1;
  }
  catch (Unicode::UndefinedProperty) {
    return 0;
  }
  catch (Unicode::BlockError) {
    return 0;
  }
} // is_supported

bool Unicode::Char::is_alpha() const {
  Gen_Cat tmp = this->category();
  return ( (tmp >= Unicode::CAT_Lu &&
	    tmp <= Unicode::CAT_Lt) ||
	   tmp == Unicode::CAT_Lm  ||
	   tmp == Unicode::CAT_Lo );
} // is_alpha

bool Unicode::Char::is_space() const {
  Gen_Cat tmp = this->category();
  return (tmp >= Unicode::CAT_Zs &&
	  tmp <= Unicode::CAT_Zp);
} // is_space

bool Unicode::Char::is_control() const {
  Gen_Cat tmp = this->category();
  return (tmp == Unicode::CAT_Cc ||
	  tmp == Unicode::CAT_Cf ||
	  tmp == Unicode::CAT_Zl ||
	  tmp == Unicode::CAT_Zp);
} // is_control

bool Unicode::Char::is_printable() const {
  Gen_Cat tmp = this->category();
  return ( ! (tmp >= Unicode::CAT_Cc &&
	      tmp <= Unicode::CAT_Cn) );
} // is_print

bool Unicode::Char::is_ascii() const {
  return ( my_unicode < 128 );
} // is_ascii

bool Unicode::Char::is_mark() const {
  Gen_Cat tmp = this->category();
  return ( tmp >= Unicode::CAT_Mn &&
	   tmp <= Unicode::CAT_Me );
} // is_mark

bool Unicode::Char::is_base() const {
  return ( this->is_alpha() || this->is_number() || this->is_mark() );
} // is_base

bool Unicode::Char::is_punctuation() const {
  Gen_Cat tmp = this->category();
  return ( tmp >= Unicode::CAT_Pc &&
	   tmp <= Unicode::CAT_Po );
} // is_punctuation

bool Unicode::Char::is_number() const {
  Gen_Cat tmp = this->category();
  return ( tmp >= Unicode::CAT_Nd &&
	   tmp <= Unicode::CAT_Nl );
} // is_number

// TRANSFORMATIONS:
void Unicode::Char::to_lowercase() {
  *this = this->lowercase();
} // to_lowercase

void Unicode::Char::to_uppercase() {
  *this = this->uppercase();
} // to_uppercase

void Unicode::Char::to_titlecase() {
  *this = this->titlecase();
} // to_titlecase

// UTILITIES
ostream & Unicode::Char::Write(ostream & out) const {
  if (my_unicode <= 0x007F)
    out << char(my_unicode);
  else {
    ios::fmtflags outflags = out.setf(ios::uppercase);
    out << "U+"
	<< hex << setfill('0') << setw(4)
	<< my_unicode;
    out.setf(outflags);
  }
  return out;
} // Write
