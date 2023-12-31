/*$Id: defs.hh,v 1.12 2001/04/10 18:08:16 tobias Exp $
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

/* This file supports Unicode version 3.1 beta */

#ifndef _Babylon_Defs_hh
#define _Babylon_Defs_hh

#ifdef sun
#define u_int16_t uint16_t
#define u_int32_t uint32_t
#include <sys/types.h>
#endif

#include <string>
#include <exception>
#include <strstream>
#include <iomanip>

namespace Babylon {

    /*
     * The following two lines are architecture dependent!
     * Use an unsigned 16bit integer type for _Char and
     * an unsigned 32bit integer type for _Skalar.
     *
     * DO NOT USE wchar_t, since that type is not garanteed
     * to be 16 bit wide in all implementations of a compiler.
     *
     * _Char must be a 16bit wide integer. Anything else is
     * not conformant to the unicode standard.
     */
    typedef char UCS1; // has to be char
    typedef u_int16_t UCS2;
    typedef u_int32_t UCS4;
    typedef std::basic_string<UCS1> UTF8_string;
    typedef std::basic_string<UCS2> UTF16_string;
    typedef std::basic_string<UCS4> UTF32_string;

    // These classes are defined in Char.hh and String.hh
    class Char;
    class String; 
    
    // max. number of char_decomps a single unicode character can have

    enum gen_cat_enum {
	//
	// NORMATIVE:
	// Case information might be incorrect for your locale!
	CAT_Lu, // Letter, Uppercase
	CAT_Ll, // Letter, Lowercase
	CAT_Lt, // Letter, Titlecase
	
	CAT_Mn, // Mark, Non-Spacing
	CAT_Mc, // Mark, Spacing Combining
	CAT_Me, // Mark, Enclosing
	
	CAT_Nd, // Number, Decimal Digit
	CAT_Nl, // Number, Letter
	CAT_No, // Number, Other
	
	CAT_Zs, // Separator, Space
	CAT_Zl, // Separator, Line
	CAT_Zp, // Separator, Paragraph
	
	CAT_Cc, // Other, Control
	CAT_Cf, // Other, Format
	CAT_Cs, // Other, Surrogate
	CAT_Co, // Other, Private Use
	CAT_Cn, // Other, Not Assigned
	//
	// INFORMATIVE:
	CAT_Lm, // Letter, Modifier
	CAT_Lo, // Letter, Other
	
	CAT_Pc, // Punctuation, Connector
	CAT_Pd, // Punctuation, Dash
	CAT_Ps, // Punctuation, Open
	CAT_Pe, // Punctuation, Close
	CAT_Pi, // Punctuation, Initial quote
	CAT_Pf, // Punctuation, Final quote
	CAT_Po, // Punctuation, Other
	
	CAT_Sm, // Symbol, Math
	CAT_Sc, // Symbol, Currency
	CAT_Sk, // Symbol, Modifier
	CAT_So, // Symbol, Other
	//
	// IMPLEMENTATION DEPENDANT:
	CAT_MAX // This will stay <=32 according to the standard.
    }; // enum gen_cat_enum
    typedef enum gen_cat_enum Gen_Cat;
    
    // NORMATIVE:
    // These Values will be between 0 and 255 according to the standard.
    // The actual values may change, only the ordering is considered to
    // be important.
    enum can_comb_class_enum {
	CC_SPACING=0, // Spacing, enclosing, reordrant, and surrounding
	CC_OVERLAY=1, // Overlays and interior
	CC_NUKTAS=7,
	CC_HIRAGANA=8, // Hiragana/Katakana voiced marks
	CC_VIRAMAS=9,
	CC_FIXED_POS_START=10, // Start of fixed position classes
	CC_FIXED_POS_END=199, // End of fixed position classes
	CC_BELOW_LEFT_ATTACHED=200,
	CC_BELOW_ATTACHED=202,
	CC_BELOW_RIGHT_ATTACHED=204, 
	CC_LEFT_ATTACHED=208, // Left attached (reordrant around single base character)
	CC_RIGHT_ATTACHED=210,
	CC_ABOVE_LEFT_ATTACHED=212,
	CC_ABOVE_ATTACHED=214,
	CC_ABOVE_RIGHT_ATTACHED=216,
	CC_BELOW_LEFT=218,
	CC_BELOW=220,
	CC_BELOW_RIGHT=222,
	CC_LEFT=224, // Left (reordrant around single base character)
	CC_RIGHT=226,
	CC_ABOVE_LEFT=228,
	CC_ABOVE=230,
	CC_ABOVE_RIGHT=232,
	CC_DOUBLE_BELOW=233,
	CC_DOUBLE_ABOVE=234,
	CC_BELOW_SUBSCRIPT=240, // Below (iota subscript) 
	// Implementation dependent:
	CC_MAX
    }; // enum can_comb_class_enum
    typedef enum can_comb_class_enum Can_Comb_Class;
    
    // NORMATIVE:
    enum bidir_props_enum {
	BIDIR_L,   // Left-to-Right
	BIDIR_LRE, // Left-to-Right Embedding
	BIDIR_LRO, // Left-to-Right Override
	BIDIR_R,   // Right-to-Left
	BIDIR_AL,  // Right-to-Left Arabic 
	BIDIR_RLE, // Right-to-Left Embedding
	BIDIR_RLO, // Right-to-Left Override
	BIDIR_PDF, // Pop Directional Format
	BIDIR_EN,  // European Number
	BIDIR_ES,  // European Number Seperator
	BIDIR_ET,  // European Number Terminator
	BIDIR_AN,  // Arabic Number
	BIDIR_CS,  // Common Number Seperator
	BIDIR_NSM, // Non-Spacing Mark
	BIDIR_BN,  // Boundary Neutral
	BIDIR_B,   // Paragraph Separator
	BIDIR_S,   // Segment Separator
	BIDIR_WS,  // Whitespace
	BIDIR_ON,  // Other Neutral
	// IMPLEMENTATION DEPENDANT:
	BIDIR_MAX
    }; // enum bidir_prop_enum
    typedef enum bidir_props_enum Bidir_Props;
    
    // NORMATIVE:
    enum char_decomp_enum {
	DECOMP_CANONICAL=0, // This is the only NORMATIVE property here.
	DECOMP_FONT,        // A font variant
	DECOMP_NOBREAK,     // A no-break version of a space or hyphen
	DECOMP_INITIAL,     // An initial presentation form
	DECOMP_MEDIAL,      // A medial presentation form
	DECOMP_FINAL,       // A final presentation form
	DECOMP_ISOLATED,    // An isolated presentation form
	DECOMP_CIRCLE,      // An encircled form
	DECOMP_SUPER,       // A superscript form
	DECOMP_SUB,         // A subscript form
	DECOMP_VERTICAL,    // A vertical layout presentation form
	DECOMP_WIDE,        // A wide (or zenkaku) compatibility character
	DECOMP_NARROW,      // A narrow (or hankaku) compatibility character
	DECOMP_SMALL,       // A small variant form
	DECOMP_SQUARE,      // A CJK squared font variant
	DECOMP_FRACTION,    // A vulgar fraction form
	DECOMP_COMPAT,      // Otherwise unspecified compatibility character 
	DECOMP_MAX          // THIS IS IMPLEMENATION DEPENDANT!
    }; // enum char_decomp_enum
    typedef enum char_decomp_enum Char_Decomp;
    
    // East Asian Cell Width Property 
    // INFORMATIVE:
    enum EA_width_enum {
	EA_WIDTH_W,  // Wide
	EA_WIDTH_F,  // Full
	EA_WIDTH_Na, // Narrow
	EA_WIDTH_H,  // Half
	EA_WIDTH_A,  // Ambiguous
	EA_WIDTH_N,  // Neutral (Characters that do not appear in
	             // legacy East Asian character sets 
	// IMPLEMENATTION DEPEMDANT:
	EA_WIDTH_MAX
    }; // enum EA_width_enum
    typedef enum EA_width_enum EA_Width;
    
    enum line_break_enum {
	// NORMATIVE:
	LB_BK, LB_CR, LB_LF, LB_CM, LB_SG, LB_GL, LB_CB, LB_SP,
	LB_ZW,
	// INFORMATIVE:
	LB_XX, LB_OP, LB_CL, LB_QU, LB_NS, LB_EX, LB_SY, LB_IS,
	LB_PR, LB_PO, LB_NU, LB_AL, LB_ID, LB_IN, LB_HY, LB_BB,
	LB_BA, LB_SA, LB_AI, LB_B2,
	// IMPLEMENTATION DEPENDANT:
	LB_MAX
    }; // enum line_break_enum
    typedef enum line_break_enum Line_Break;
    
    // IMPLEMENTATION DEPENDANT:
    enum unichar_props_enum {
	PROP_CHARACTER,       // The character itself
	PROP_UNICODE_VALUE,   // Number of the Unicode character
	PROP_GEN_CAT,         // general category
	PROP_CHAR_DECOMP,     // character decomposition
	PROP_COMB_CLASS,      // cannonical combining class
	PROP_BIDIR_PROPS,     // bidirectional properties
	PROP_DEC_DIGIT_VALUE, // decimal digit value
	PROP_DIGIT_VALUE,     // digit value
	PROP_NUMERIC_VALUE,   // numeric value
	PROP_IS_MIRRORED,     // is mirrored
	PROP_UPPER_EQUIV,     // UPPERCASE equivalent
	PROP_LOWER_EQUIV,     // lowercase equivalent
	PROP_TITLE_EQUIV,     // Titlecase equivalent
	PROP_SCRIPT,          // script the character belongs to
	PROP_EA_WIDTH,        // East Asien cell width of the character
	PROP_LINE_BREAKING,   // Line Breaking Property
	PROP_MAX
    }; // enum unichar_props_enum
    typedef enum unichar_props_enum Char_Props;
    
    // IMPLEMENTATION DEPENDANT:
    enum control_character_enum {
	UC_NULL                        = 0x0000,
	UC_START_OF_HEADING            = 0x0001,
	UC_START_OF_TEXT               = 0x0002,
	UC_END_OF_TEXT                 = 0x0003,
	UC_END_OF_TRANSMISSION         = 0x0004,
	UC_ENQUIRY                     = 0x0005,
	UC_ACKNOWLEDGE                 = 0x0006,
	UC_BELL                        = 0x0007,
	UC_BACKSPACE                   = 0x0008,
	UC_HORIZONTAL_TABULATION       = 0x0009,
	UC_LINE_FEED                   = 0x000A,
	UC_VERTICAL_TABULATION         = 0x000B,
	UC_FORM_FEED                   = 0x000C,
	UC_CARRIAGE_RETURN             = 0x000D,
	UC_SHIFT_OUT                   = 0x000E,
	UC_SHIFT_IN                    = 0x000F,
	UC_DATA_LINK_ESCAPE            = 0x0010,
	UC_DEVICE_CONTROL_ONE          = 0x0011,
	UC_DEVICE_CONTROL_TWO          = 0x0012,
	UC_DEVICE_CONTROL_THREE        = 0x0013,
	UC_DEVICE_CONTROL_FOUR         = 0x0014,
	UC_NEGATIVE_ACKNOWLEDGE        = 0x0015,
	UC_SYNCHRONOUS_IDLE            = 0x0016,
	UC_END_OF_TRANSMISSION_BLOCK   = 0x0017,
	UC_CANCEL                      = 0x0018,
	UC_END_OF_MEDIUM               = 0x0019,
	UC_SUBSTITUTE                  = 0x001A,
	UC_ESCAPE                      = 0x001B,
	UC_FILE_SEPARATOR              = 0x001C,
	UC_GROUP_SEPERATOR             = 0x001D,
	UC_RECORD_SEPARATOR            = 0x001E,
	UC_UNIT_SEPERATOR              = 0x001F,
	UC_DELETE                      = 0x007F,
	UC_BREAK_PERMITTED_HERE        = 0x0082,
	UC_NO_BREAK_HERE               = 0x0083,
	UC_INDEX                       = 0x0084,
	UC_NEXT_LINE                   = 0x0085,
	UC_START_OF_SELECTED_AREA      = 0x0086,
	UC_END_OF_SELECTED_AREA        = 0x0087,
	UC_CHARACTER_TABULATION_SET    = 0x0088,
	UC_CHARACTER_TABULATION_WITH_JUSTIFICATION
	= 0x0089,
	UC_LINE_TABULATION_SET         = 0x008A,
	UC_PARTIAL_LINE_DOWN           = 0x008B,
	UC_PARTIAL_LINE_UP             = 0x008C,
	UC_REVERSE_LINE_FEED           = 0x008D,
	UC_SINGLE_SHIFT_TWO            = 0x008E,
	UC_SINGLE_SHIFT_THREE          = 0x008F,
	UC_DEVICE_CONTROL_STRING       = 0x0090,
	UC_PRIVATE_USE_ONE             = 0x0091,
	UC_PRIVATE_USE_TWO             = 0x0092,
	UC_SET_TRANSMIT_STATE          = 0x0093,
	UC_CANCEL_CHARACTER            = 0x0094,
	UC_MESSAGE_WAITING             = 0x0095,
	UC_START_OF_GUARDED_AREA       = 0x0096,
	UC_END_OF_GUARDED_AREA         = 0x0097,
	UC_START_OF_STRING             = 0x0098,
	UC_SINGLE_CHARACTER_INTRODUCER = 0x009A,
	UC_CONTROL_SEQUENCE_INTRODUCER = 0x009B,
	UC_STRING_TERMINATOR           = 0x009C,
	UC_OPERATING_SYSTEM_COMMAND    = 0x009D,
	UC_PRIVACY_MESSAGE             = 0x009E,
	UC_APPLICATION_PROGRAMM_COMMAND= 0x009F,
	
	// PRIVATE USE:
	KEY_SHIFT                   = 0xE000,
	KEY_CTRL                    = 0xE001,
	KEY_MENUE                   = 0xE019,
	KEY_PRINT_SCREEN            = 0xE01B,
	KEY_INSERT                  = 0xE022,
	KEY_BREAK                   = 0XE029,
	KEY_CURSOR_UP               = 0xE032,
	KEY_CURSOR_DOWN             = 0xE033,
	KEY_CURSOR_LEFT             = 0xE034,
	KEY_CURSOR_RIGHT            = 0xE035,
	KEY_PAGE_UP                 = 0xE036,
	KEY_PAGE_DOWN               = 0xE037,
	KEY_HOME                    = 0xE038,
	KEY_END                     = 0xE039,
	KEY_F1                      = 0xE101,
	KEY_F2                      = 0xE102,
	KEY_F3                      = 0xE103,
	KEY_F4                      = 0xE104,
	KEY_F5                      = 0xE105,
	KEY_F6                      = 0xE106,
	KEY_F7                      = 0xE107,
	KEY_F8                      = 0xE108,
	KEY_F9                      = 0xE109,
	KEY_F10                     = 0xE10A,
	KEY_F11                     = 0xE10B,
	KEY_F12                     = 0xE10C,
	KEY_ALT                     = 0xE302,
	KEY_WIN                     = 0xE303,
	KEY_ALT_GR                  = 0xE306,
	KEY_CAPS_LOCK               = 0xE387,
	KEY_NUM_LOCK                = 0xE388,
	KEY_SCROLL_LOCK             = 0xE389,

	// IMPLEMENTATION DEPENDENT
	UC_MAX_DEFINED              = 0x10FFFE
    }; // control_character_enum
    typedef enum control_character_enum Control_Char;
    
    enum norm_enum {
	NORM_D    = 0,
	NORM_C    = 1,
	NORM_KD   = 2,
	NORM_KC   = 3,
	NORM_NONE = 4
    }; // norm_enum
    typedef enum norm_enum Norm;
    
    enum trans_error_enum {
	TRANS_CAN_NOT_DECODE,
	TRANS_CAN_NOT_ENCODE
    };
    typedef enum trans_error_enum Trans_Error;

    // Classes to throw around as exceptions:
    
    class Undefined_Property : std::exception {
    public:
	UCS4 m_error_uc;
	Char_Props m_error_prop;
	
	Undefined_Property(UCS4 uc,
			   const Char_Props prop) {
	    m_error_uc = uc;
	    m_error_prop = prop;
	}

	const char * what() const throw() {
	    std::strstream res;
	    res << std::setw(4) << std::setfill('0') << std::hex; 
	    switch (m_error_prop) {
	    case PROP_CHARACTER:
		res << "(" <<  m_error_uc << " Character is undefined";
		break;
	    case PROP_UNICODE_VALUE:
		res << "(" << m_error_uc
		    << ") Character has no unicode value.. how did this happen?";
		break;
	    case PROP_GEN_CAT:
		res << "(" << m_error_uc
		    << ") Character has no general category... how did this happen?";
		break;
	    case PROP_CHAR_DECOMP:
		res << "(" << m_error_uc << ") Character has no decomposition";
		break;
	    case PROP_COMB_CLASS :
		res << "(" << m_error_uc
		    << ") Character has no canonical combining class.";
		break;
	    case PROP_BIDIR_PROPS:
		res << "(" << m_error_uc << ") Character has no bidir property.";
		break;
	    case PROP_DEC_DIGIT_VALUE:
		res << "(" << m_error_uc << ") Character has no decimal digit value.";
		break;
	    case PROP_DIGIT_VALUE:
		res << "(" << m_error_uc << ") Character has no digit value.";
		break;
	    case PROP_NUMERIC_VALUE:
		res << "(" << m_error_uc << ") Character has no numeric value.";
		break;
	    case PROP_IS_MIRRORED:
		res << "(" << m_error_uc 
		    << ") Mirroring property missing... how did this happen?";
		break;
	    case PROP_UPPER_EQUIV:
		res << "(" << m_error_uc
		    << ") Uppercase equivalent missing... how did this happen?";
		break;
	    case PROP_LOWER_EQUIV:
		res << "(" << m_error_uc
		    << ") Lowercase equivalent missing... how did this happen?";
		break;
	    case PROP_TITLE_EQUIV:
		res << "(" << m_error_uc
		    << ") Titlecase equivalent missing... how did this happen?";
		break;
	    case PROP_SCRIPT:
		res << "(" << m_error_uc
		    << ") Character belongs to no script... how did this happen?";
		break;
	    case PROP_EA_WIDTH:
		res << "(" << m_error_uc
		    << ") EA width property missing... how did this happen?";
		break;
	    case PROP_LINE_BREAKING:
		res << "(" << m_error_uc
		    << ") linebreak property missing... how did this happen?";
		break;
	    case PROP_MAX:
		res << "(" << m_error_uc
		    << ") PROP_MAX throw... how did this happen?";
		break;	
	    }
	    return res.str();
	}
    }; // class Undefined_Property
    
  class Transfer_Error : std::exception
  {
  public:
    Trans_Error error;
    Transfer_Error(Trans_Error transError) : error(transError) {}
    ~Transfer_Error() throw() {}
    const char * what() const throw()
    {
      switch (error)
	{
	case TRANS_CAN_NOT_ENCODE:
	  return("Can not encode from Babylon to foreign format.");
	default:
	  return("Can not decode from foreign format to Babylon.");
	}
    }
  }; // Transfer_Error

  class Block_Error : std::exception
  {
  public:
    UCS4 m_block_start;
    UCS4 m_block_end;
    std::string m_error_message;
    Block_Error(UCS4 startUC, UCS4 endUC, const std::string &em)
    {
      m_block_start = startUC;
      m_block_end = endUC;
      m_error_message = em;
    }
    ~Block_Error() throw() {}
    const char * what() const throw()
    {
      std::strstream res;
      res << std::hex << std::setw(4) << std::setfill('0');
      res << "(" << m_block_start << "-" << m_block_end << "): "
	  << m_error_message;
      return res.str();
    }
  }; // class Block_Error
    
} // namespace Babylon;

#endif // _Babylon_Defs_hh
