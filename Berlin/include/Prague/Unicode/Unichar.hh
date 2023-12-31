/*$Id: Unichar.hh,v 1.4 1999/11/08 21:29:51 tobias Exp $
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
#ifndef _Unichar_hh
#define _Unichar_hh

#include <Prague/Unicode/Unicode.hh>
#include <streambuf.h>
#include <iostream.h>
#include <iomanip.h>

namespace Unicode {
  /*
   * This header-file defines functions on single
   * unicode characters. For functions on unicode
   * strings check out Prague/Unicode/Unistring.hh
   */
  
  class Char {
  public:

    // QUERIES:
    _Char myUnicode() const { return my_unicode; }
    //. returns the Unicode value of this Unicode character.

    Char uppercase() const;
    //. return the uppercase equivalent of a given
    //. unicode character.
    //. Warning: For some characters of some locales
    //.          this might not be the character you
    //.          exspect!
    
    Char lowercase() const;
    //. return the lowercase equivalent of a given
    //. unicode character.
    //. Warning: For some characters of some locales
    //.          this might not be the character you
    //.          exspect!
    
    Char titlecase() const;
    //. return the titlecase equivalent of a given
    //. unicode character.
    //. Warning: For some characters of some locales
    //.          this might not be the character you
    //.          exspect!

    float numericValue() const;
    //. return the numeric value of a given unicode character.

    int digitValue() const;
    //. return the digit value of a given unicode character.
    
    int decDigitValue() const;
    //. return the decimal digit value of a given unicode character.
    
    string blockname() const;
    //. returns the name of the block a given unicode character
    //. belongs to.
    
    Gen_Cat category() const;
    //. returns the general type of the given unicode
    //. character.
    
    Bidir_Props direction() const;
    //. returns the linguistic direction property of the
    //. given unicode character.

    Can_Comb_Class combClass() const;
    //. returns the cannonical comnbining class of
    //. the given unicode character.

    Char_Decomp decompType() const;
    //. returns the decomposition type of a given
    //. unicode character.

    String decompString() const;
    //. returns the decomposition string of a given
    //. unicode character.

    bool mustMirror() const;
    //. returns true, if this character must be mirrored in
    //. when in a right-to-left context. and false otherwise. 

    EA_Width  EAWidth() const;
    //. returns the cell width of the character for the
    //. given unicode.

    Line_Break linebreak() const;
    //. returns the line-breaking property of the
    //. given unicode character.
    
    // TESTS:
    
    bool is_lowercase() const;
    //. returns true if the input Unicode character is
    //. lowercase and false otherwise.
    
    bool is_uppercase() const;
    //. returns true if the input Unicode character is
    //. uppercase and false otherwise. 
    
    bool is_titlecase() const;
    //. returns true if the input Unicode character is
    //. titlecase and false otherwise.
    
    bool is_digit() const;
    //. returns true if the input Unicode character is
    //. a digit and false otherwise.
    
    bool is_defined() const;
    //. returns true if the input Unicode character is
    //. defined and false otherwise.
    
    bool is_supported() const;
    //. returns true if the input Unicode character is
    //. defined and supported by this implementation.
    
    bool is_alpha() const;
    //. returns true if the input Unicode character is
    //. a letter and false otherwise.
    
    bool is_space() const;
    //. returns true if the input Unicode character is
    //. a space character and false otherwise.
    
    bool is_control() const;
    //. returns true if the input Unicode character is
    //. control character and false otherwise.
    
    bool is_printable() const;
    //. returns true if the input Unicode character is
    //. printable and false otherwise. This does not
    //. tell wether the character is included in a given font.
    
    bool is_base() const;
    //. returns true if the input Unicode character is
    //. in base form and false otherwise.
    
    bool is_ascii() const;
    //. returns true if the input Unicode character is
    //. a ASCII character and false otherwise.
    
    bool is_mark() const;
    //. returns true if the input Unicode character is
    //. a mark and false otherwise.
    
    bool is_number() const;
    //. returns true if the input Unicode character is
    //. a number and false otherwise.
  
    bool is_punctuation() const;
    //. returns true if the input Unicode character is
    //. a punctuation mark and false otherwise. 
    
    // TRANSFORMATIONS:

    void to_lowercase();
    //. returns the lowercase equivalent to a given unicode
    //. character. If there is no such aequivalent this
    //. function returns the input character.
    
    void to_uppercase();
    //. returns the uppercase equivalent to a given unicode
    //. character. If there is no such aequivalent this
    //. function returns the input character.
    
    void to_titlecase();
    //. returns the titlecase equivalent to a given unicode
    //. character. If there is no such aequivalent this
    //. function returns the input character.
  
    // CONSTRUCTORS:
    Char() { my_unicode = Unicode::NULL_UNICODE; }
    // FIXME: Only chars <= 0x007F should be allowed! 
    Char(const char & C) { my_unicode = _Char(C); }
    Char(const _Char & _UC) { my_unicode = _UC; }
    Char(const int & I) { my_unicode = _Char(I); };

    Char(const Char & UC) { my_unicode = UC.myUnicode(); }

    // OPERATORS:
    Char & operator = (const Char UC) {
      my_unicode = UC.my_unicode;
      return *this;
    } // operator = (Char)

    Char & operator = (const _Char _UC){
      my_unicode = _UC;
      return *this;
    } // operator = (_Char)

    Char & operator = (const char C) {
      my_unicode = _Char(C);
      return *this;
    } // operator = (char)

    Char & operator = (const int I) {
      my_unicode = _Char(I);
      return *this;
    } // operator = (int)

    // FIXME: Postfix and prefix-versions are
    // identical at the moment!
    Char & operator ++ (int) {
      my_unicode++;
      return *this;
    } // operator ++ (int)

    Char & operator -- (int) {
      my_unicode--;
      return *this;
    } // operator -- (int)

    Char & operator ++ () {
      my_unicode++;
      return *this;
    } // operator ++ ()

    Char & operator -- () {
      my_unicode--;
      return *this;
    } // operator -- ()

    Char & operator += (Char UC) {
      my_unicode += UC.my_unicode;
      return *this;
    } // operator +=

    Char & operator += (_Char _UC) {
      my_unicode += _UC;
      return *this;
    } // operator +=

    Char & operator += (int I) {
      my_unicode += _Char(I);
      return *this;
    } // operator +=

    Char & operator += (char C) {
      my_unicode += _Char(C);
      return *this;
    } // operator +=

    Char & operator -= (Char UC) {
      my_unicode -= UC.my_unicode;
      return *this;
    } // operator -=

    Char & operator -= (_Char _UC) {
      my_unicode -= _UC;
      return *this;
    } // operator -=

    Char & operator -= (int I) {
      my_unicode -= _Char(I);
      return *this;
    } // operator -=

    Char & operator -= (char C) {
      my_unicode -= _Char(C);
      return *this;
    } // operator -=

    Char & operator *= (Char UC) {
      my_unicode *= UC.my_unicode;
      return *this;
    } // operator *=

    Char & operator *= (_Char _UC) {
      my_unicode *= _UC;
      return *this;
    } // operator *=

    Char & operator *= (int I) {
      my_unicode *= _Char(I);
      return *this;
    } // operator *=

    Char & operator *= (char C) {
      my_unicode *= _Char(C);
      return *this;
    } // operator *=

    // UTILITIES:
    ostream & Write(ostream &) const;

    bool equal(const Unicode::Char & UC) const {
      return my_unicode == UC.my_unicode; 
    } // equal

    bool less (const Unicode::Char & UC) const {
      return my_unicode < UC.my_unicode ;
    } // less

    // Destructor:
    // ~Char() // nothing special to do...
  private:
    _Char my_unicode;
  }; // class Char
  
} // namespace Unicode

// OPERATORS:
inline ostream & operator <<
(ostream & out, const Unicode::Char & UC) {
  return UC.Write(out);
}

// The relational operators look at the unicode values
// ONLY! So semantically identical characters will not be
// recognized.
inline bool operator ==
(const Unicode::Char & lhs, const Unicode::Char & rhs) {
    return lhs.equal(rhs);
}

inline bool operator !=
(const Unicode::Char & lhs, const Unicode::Char & rhs) {
    return ! (lhs == rhs);
}

inline bool operator <
(const Unicode::Char & lhs, const Unicode::Char & rhs) {
    return lhs.less(rhs);
}

inline bool operator >
(const Unicode::Char & lhs, const Unicode::Char & rhs) {
    return rhs < lhs;
}

inline bool operator <=
(const Unicode::Char & lhs, const Unicode::Char & rhs) {
    return ! (lhs > rhs);
}

inline bool operator >=
(const Unicode::Char & lhs, const Unicode::Char & rhs) {
    return rhs <= lhs;
}

#endif // _Unichar_hh
