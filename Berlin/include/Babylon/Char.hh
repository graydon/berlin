/*$Id: Char.hh,v 1.10 2001/04/18 06:07:25 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999,2000 Tobias Hunger <Tobias@berlin-consortium.org>
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
#ifndef _Babylon_Char_hh
#define _Babylon_Char_hh

#include <Babylon/defs.hh>
#include <Babylon/Dictionary.hh>
#include <streambuf.h>
#include <iostream.h>
#include <iomanip.h>

/*
 * This header-file defines functions on single
 * unicode characters. For functions on unicode
 * strings check out Babylon/String.hh
 */

namespace Babylon {

    class Char {
    public:
	// ------------------------------------------------------------
	// CONVERSION:
	// ------------------------------------------------------------


	//. Transcodes the character to UTF-8.
	//. Throws : Trans_Error
	//.          if transcoding was not successful.
	UTF8_string utf8() const throw (Trans_Error);

	//. Transcodes the character to UTF-16.
	//. Throws : Trans_Error
	//.          if transcoding was not successful.
	//.
	//. Warning: Not all characters of ISO 10646 can get
	//.          transcoded to UTF-16!
	UTF16_string utf16() const throw (Trans_Error);

	//. Transcodes the character to UTF-32.
	//. Throws : Trans_Error
	//.          if transcoding was not successful.
	UTF32_string utf32() const throw(Trans_Error);

	//. Creates a character from an UTF-8 encoded string.
	//. Throws : Trans_Error
	//.          if transcoding was not successful.
	//. Returns: Iterator to the character in the UTF-8
	//.          string after the last one used to encode
	//.          this character.
	UTF8_string::const_iterator utf8(const UTF8_string &,
					 UTF8_string::const_iterator)
	    throw (Trans_Error);

	//. Creates a character from an UTF-16 encoded string.
	//. Throws : Trans_Error
	//.          if transcoding was not successful.
	//. Returns: Iterator to the character in the UTF-16
	//.          string after the last one used to encode
	//.          this character.
	UTF16_string::const_iterator utf16(const UTF16_string &,
					   UTF16_string::const_iterator)
	    throw (Trans_Error);

	//. Creates a character from an UTF-32 encoded string.
	//. Throws : Trans_Error
	//.          if transcoding was not successful.
	//. Returns: Iterator to the character in the UTF-32
	//.          string after the last one used to encode
	//.          this character.
	UTF32_string::const_iterator utf32(const UTF32_string &,
					   UTF32_string::const_iterator)
	    throw(Trans_Error);

	// ------------------------------------------------------------
	// QUERIES:
	// ------------------------------------------------------------

	//. Returns the (Scalar-) Unicode value of the character.
	UCS4 value() const { return m_value; }

    
	//. Gets the uppercase equivalent of the character.
	//. If no uppercase equivalent is defined then a copy of
	//. the current chracter is returned.
	//.
	//. Warning: For some characters of some locales
	//.          this might not be the character you
	//.          exspect!
	Char uppercase() const throw (Block_Error) {
	    return Dictionary::instance()->uppercase(m_value);
	}

	//. Gets the lowercase equivalent of the character.
	//. If no uppercase equivalent is defined then a copy of
	//. the current chracter is returned.
	//.
	//. Warning: For some characters of some locales
	//.          this might not be the character you
	//.          exspect!
	Char lowercase() const throw (Block_Error) {
	    return Dictionary::instance()->lowercase(m_value);
	}

	//. Gets the titlecase equivalent of the character.
	//. If no uppercase equivalent is defined then a copy of
	//. the current chracter is returned.
	//.
	//. Warning: For some characters of some locales
	//.          this might not be the character you
	//.          exspect!
	Char titlecase() const throw (Block_Error) {
	    return Dictionary::instance()->titlecase(m_value);
	}
	    
	//. Gets the numeric value of the character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no numeric property set.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	float numeric_value() const throw (Undefined_Property, Block_Error)  {
	    return Dictionary::instance()->numeric_value(m_value);
	}
	//. return the numeric value of a given unicode character.
    
	//. Gets the digit value of the character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no digit value property set.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	int digit_value() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->digit_value(m_value);
	}

	//. Gets the decimal digit value of the character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	int dec_digit_value() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->dec_digit_value(m_value);
	}

	//. Returns the name of the block (aka. script) the character belongs to.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	std::string blockname() const throw (Block_Error) {
	    return Dictionary::instance()->blockname(m_value);
	}
    
	//. Gets the general category of the character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	Gen_Cat category() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->category(m_value);
	}

    	//. Returns the linguistic direction property of the
	//. given unicode character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	Bidir_Props direction() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value);
	}

	//. Returns the cannonical comnbining class of
	//. the given unicode character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	Can_Comb_Class comb_class() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->comb_class(m_value);
	}

	//. Returns the decomposition type of a given
	//. unicode character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	Char_Decomp decomp_type() const
	    throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->decomp_type(m_value);
	}

    	//. Returns the decomposition string of a given
	//. unicode character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	String decompose() const throw (Undefined_Property, Block_Error);

	//. Returns true, if this character must be mirrored
	//. when it appears in a right-to-left context. and false
	//. otherwise. 
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool must_mirror() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->must_mirror(m_value);
	}

	//. Returns the (East Asian) cell width of the character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	EA_Width  EA_width() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->EA_width(m_value);
	}

    	//. Returns the line-breaking property of the character.
	//. Throws : Undefined_Property
	//.          if it character is not defined or
	//.          has no decimal digit value property set.
	//.          THIS SHOULD NEVER HAPPEN, as all characters should
	//.          have this defined.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	Line_Break linebreak() const throw (Undefined_Property, Block_Error) {
	    return Dictionary::instance()->linebreak(m_value);
	}

	// ------------------------------------------------------------
	// TESTS:
	// ------------------------------------------------------------

	//. Returns true, if this character is defined and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_defined() const 
	    throw (Block_Error) {
	    return Dictionary::instance()->is_defined(m_value);
	}
    
	//. Returns true, if this character is a Space and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Space()
	    throw (Block_Error) {
	    return Dictionary::instance()->category(m_value) == CAT_Zs;
	}

	//. Returns true, if this character is a ISO Control Character and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_ISO_Control()
	    throw (Block_Error) {
	    return Dictionary::instance()->category(m_value) == CAT_Cc;
	}

	//. Returns true, if this character is used for Punctuation and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Punctuation()
	    throw (Block_Error) {
	    return (Dictionary::instance()->category(m_value) == CAT_Pc ||
		    Dictionary::instance()->category(m_value) == CAT_Pd ||
		    Dictionary::instance()->category(m_value) == CAT_Ps ||
		    Dictionary::instance()->category(m_value) == CAT_Pe ||
		    Dictionary::instance()->category(m_value) == CAT_Pi ||
		    Dictionary::instance()->category(m_value) == CAT_Pf ||
		    Dictionary::instance()->category(m_value) == CAT_Po);
	}

	//. Returns true, if this character is the line separator and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Line_Separator()
	    throw (Block_Error) {
	    return Dictionary::instance()->category(m_value) == CAT_Zl;
	}

	//. Returns true, if this character is the paragraph separator and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Paragraph_Separator()
	    throw (Block_Error) {
	    return Dictionary::instance()->category(m_value) == CAT_Zp;
	}

	//. Returns true, if this character is a currency symbol and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!	
	bool is_Currency_Symbol()
	    throw (Block_Error) {
	    return Dictionary::instance()->category(m_value) == CAT_Sc;
	}
	
	//. Returns true, if this character is should be written left to right
	//. and false otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!			
	bool is_Bidi_Left_to_Right()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_L;
	}

	//. Returns true, if this character must be treated like
	//. a european number by the Bidi algorithemn and false otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_European_Digit()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_EN;
	}

	//. Returns true, if this character must be treated like
	//. a european number separator by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Eur_Num_Separator()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_ES;
	}	
	
	//. Returns true, if this character must be treated like
	//. a european number terminator by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Eur_Num_Terminator()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_ET;
	}

	//. Returns true, if this character must be treated like
	//. a arabic number by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Arabic_Digit()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_AN;
	}
		
	//. Returns true, if this character must be treated like
	//. a common separator by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Common_Separator()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_CS;
	}

	//. Returns true, if this character must be treated like
	//. a block separator by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Block_Separator()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_B;
	}

	//. Returns true, if this character must be treated like
	//. a segment separator by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Segment_Separator()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_S;
	}

	//. Returns true, if this character must be treated like
	//. a white space by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Whitespace()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_WS;
	}

	//. Returns true, if this character must be treated like
	//. a non spacing mark by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Non_spacing_Mark()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_NSM;
	}

	//. Returns true, if this character must be treated like
	//. a boundary neutral by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Boundary_Neutral()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_BN;
	}

	//. Returns true, if this character pops the directional formating
	//. and false otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_PDF()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_PDF;
	}

	//. Returns true, if this character is a bidirectional override or
	//. embedding character and false otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Bidi_Embedding_or_Override()
	    throw (Block_Error) {
	    return (Dictionary::instance()->bidir_props(m_value) == BIDIR_LRE ||
		    Dictionary::instance()->bidir_props(m_value) == BIDIR_RLE ||
		    Dictionary::instance()->bidir_props(m_value) == BIDIR_LRO ||
		    Dictionary::instance()->bidir_props(m_value) == BIDIR_RLO);
	}

	//. Returns true, if this character must be treated like
	//. a other neutral by the Bidi algorithemn and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!				
	bool is_Bidi_Other_Neutral()
	    throw (Block_Error) {
	    return Dictionary::instance()->bidir_props(m_value) == BIDIR_ON;
	}

	//. Returns true, if this character is a virama and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Virama()
	    throw (Block_Error) {
	    return Dictionary::instance()->comb_class(m_value) == CC_VIRAMAS;
	}

	//. Returns true, if this character is printable and false
	//. otherwise. 
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	bool is_Printable()
	    throw (Block_Error) {
	    return (Dictionary::instance()->category(m_value) != CAT_MAX &&
		    Dictionary::instance()->category(m_value) != CAT_Cc &&
		    Dictionary::instance()->category(m_value) != CAT_Cf);
	}

	//. Returns true, if this is NOT character and false otherwise. 
	bool is_Not_a_Character() {
	    return (m_value & 0xFFFD == 0);
	}

	// Derived Properties:
	bool is_Math()
	    throw (Block_Error) {
	    return (Dictionary::instance()->category(m_value) == CAT_Sm ||
		    Dictionary::instance()->is_Other_Math(m_value));
	}

	bool is_Alphabetic() const
	    throw (Block_Error);

	bool is_Lowercase() const
	    throw (Block_Error) {
	    return (Dictionary::instance()->category(m_value) == CAT_Ll ||
		    Dictionary::instance()->is_Other_Lowercase(m_value));
	}

	bool is_Uppercase() const
	    throw (Block_Error) {
	    return (Dictionary::instance()->category(m_value) == CAT_Lu ||
		    Dictionary::instance()->is_Other_Uppercase(m_value));
	}

	// This does not really belong into the Derived Properties:
	bool is_Titlecase() const
	    throw (Block_Error) {
	    return (Dictionary::instance()->category(m_value) == CAT_Lt);
	}

	bool is_ID_Start() const throw (Block_Error);
	bool is_ID_Continue() const throw (Block_Error);

	// FIXME: Closure forms not added yet.
	bool is_XID_Start() const throw (Block_Error) {
	    return is_ID_Start();
	}

	bool is_XID_Continue() const throw (Block_Error) {
	    return is_ID_Continue();
	}

	bool is_Decimal() const throw (Block_Error) {
	    return (Dictionary::instance()->is_Decimal_Digit(m_value) &&
		    Dictionary::instance()->is_Digit(m_value) &&
		    Dictionary::instance()->is_Numeric(m_value));
	}

	bool is_Digit() const throw (Block_Error) {
	    return (Dictionary::instance()->is_Digit(m_value) &&
		    Dictionary::instance()->is_Numeric(m_value));
	}

	bool is_Numeric() const throw (Block_Error) {
	    return Dictionary::instance()->is_Numeric(m_value);
	}

	bool is_Private_Use() const {
	    return ((m_value >= 0xE000 && m_value <= 0xF8FF) ||
		    (m_value >= 0xF0000 && m_value <= 0xFFFFD) ||
		    (m_value >= 0x100000 && m_value <= 0x10FFFD));
	}
	
	// ------------------------------------------------------------
	// TRANSFORMATIONS:
	// ------------------------------------------------------------
    
	//. Turns ths character to lowercase.
	//. Leaves it as it is if there is no lowercase equivalent defined
	//. for it.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	void to_lower() throw (Block_Error);

	//. Turns ths character to uppercase.
	//. Leaves it as it is if there is no lowercase equivalent defined
	//. for it.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	void to_upper() throw (Block_Error);

	//. Turns ths character to titlecase.
	//. Leaves it as it is if there is no lowercase equivalent defined
	//. for it.
	//. Throws : Block_Error
	//.          if the block containing the character
	//.          could not get loaded.
	//.          THIS SHOULD NEVER HAPPEN!
	void to_title() throw (Block_Error);
    
	// ------------------------------------------------------------
	// CONSTRUCTORS:
	// ------------------------------------------------------------

	Char() { m_value = Babylon::UC_NULL; }
	Char(const UCS4 uc)   { m_value = uc; }
	Char(const Char & uc) { m_value = uc.value(); }
	bool equal(Char UC) const { return m_value == UC.m_value;}
	bool less(Char UC) const { return m_value < UC.m_value;}
	// ------------------------------------------------------------
	// OPERATORS:
	// ------------------------------------------------------------
        // The relational operators look at the unicode values
        // ONLY! So semantically identical characters will not be
        // recognized.
        bool operator == (Char UC) const { return m_value == UC.m_value;}
 	bool operator != (Char UC) const { return m_value != UC.m_value;}
 	bool operator < (Char UC) const { return m_value < UC.m_value;}
 	bool operator > (Char UC) const { return m_value > UC.m_value;}
 	bool operator <= (Char UC) const { return m_value <= UC.m_value;}
 	bool operator >= (Char UC) const { return m_value >= UC.m_value;}
	Char &operator = (Char uc) { m_value = uc.m_value; return *this;}
	Char &operator = (UCS4 uc) { m_value = uc; return *this;}
	Char &operator = (char c) { m_value = UCS4(c); return *this;}
	Char &operator = (int i) { m_value = UCS4(i); return *this;}
	Char operator ++ (int) { Char before(*this); m_value++; return before;}
	Char operator -- (int) { Char before(*this); m_value--; return before;}
	Char &operator ++ () { m_value++; return *this;}
	Char &operator -- () { m_value--; return *this;}
	Char &operator += (Char uc) { m_value += uc.m_value; return *this;}
	Char &operator += (UCS4 uc) { m_value += uc; return *this;}
	Char &operator += (int i) { m_value += UCS4(i); return *this;}
	Char &operator += (char c) { m_value += UCS4(c); return *this;}
	Char &operator -= (Char uc) { m_value -= uc.m_value; return *this;}
	Char &operator -= (UCS4 uc) { m_value -= uc; return *this;}
	Char &operator -= (int i) { m_value -= UCS4(i); return *this;}
	Char &operator -= (char c) { m_value -= UCS4(c); return *this;}
	Char &operator *= (Char uc) { m_value *= uc.m_value; return *this;}
	Char &operator *= (UCS4 uc) { m_value *= uc; return *this;}
	Char &operator *= (int i) { m_value *= UCS4(i); return *this;}
	Char &operator *= (char c) { m_value *= UCS4(c); return *this;}

    private:
	UCS4 m_value;

    }; // class Char

} // namespace Babylon

#endif // _Babylon_Char_hh
