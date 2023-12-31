/*$Id: UnicodeDefs.hh,v 1.9 1999/11/08 21:29:51 tobias Exp $
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

/* This file supports Unicode version 3.0 beta */

#ifndef _UnicodeDefs_hh
#define _UnicodeDefs_hh

#include <Prague/Sys/Thread.hh>
#include <string>

/*
 * Don´t use wchar_t to store unicode characters! wchar_t is
 * compiler dependant and may change its size from on version
 * of a compiler to the next!
 *
 * Unicode characters MUST be stored in a 16bit wide unsigned
 * integer format. Everything else is not conformant to the
 * standard.
 *
 */

namespace Unicode {

  typedef unsigned short _Char;
  typedef basic_string<_Char> _String;

  // These classes are defined in Unichar.hh and Unistring.hh
  class Char;
  class String; 

  const _Char NULL_UNICODE     = 0x0000;

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
    CC_BELOW_SUBSCRIPT=240 // Below (iota subscript) 
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
    DECOMP_NO_DECOMP=0, // THIS IS IMPLEMENTATION DEPENDANT!
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
    LB_BK,
    LB_CR,
    LB_LF,
    LB_CM,
    LB_SG,
    LB_GL,
    LB_CB,
    LB_SP,
    LB_ZW,
    // INFORMATIVE:
    LB_XX,
    LB_OP,
    LB_CL,
    LB_QU,
    LB_NS,
    LB_EX,
    LB_SY,
    LB_IS,
    LB_PR,
    LB_PO,
    LB_NU,
    LB_AL,
    LB_ID,
    LB_IN,
    LB_HY,
    LB_BB,
    LB_BA,
    LB_SA,
    LB_AI,
    LB_B2,
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
  typedef enum unichar_props_enum CharProps;

  class FileError {
  public:
    string ErrorFilename;
    string ErrorMessage;

    FileError(const string filename, const string message) {
      ErrorFilename = filename;
      ErrorMessage  = message;
    }
  };

  class UndefinedProperty {
  public:
    _Char ErrorUC;
    unichar_props_enum ErrorProp;

    UndefinedProperty(const _Char UC,
		      const CharProps Prop) {
      ErrorUC = UC;
      ErrorProp = Prop;
    }
  }; // class UndefinedProperty

  class BlockError {
  public:
    _Char BlockStart;
    _Char BlockEnd;
    string ErrorMessage;

    BlockError(const _Char StartUC,
	       const _Char EndUC,
	       const string EM) {
      BlockStart = StartUC;
      BlockEnd = EndUC;
      ErrorMessage = EM;
    }
  }; // class BlockError

} // namespace Unicode;

#endif // _UnicodeDefs_hh
