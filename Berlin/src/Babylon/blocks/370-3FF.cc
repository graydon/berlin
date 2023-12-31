/*$Id: 370-3FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:17 +0200.
 *
 * This plugin to libPrague is free software; you can redistribute it
 * and/or  modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA
 */

#include <Babylon/defs.hh>
#include <Babylon/Dictionary.hh>
#include <bitset>
#include <map>

namespace Babylon {

  class Greek370 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Greek370() {
      m_first_letter = 0x370;
      m_last_letter  = 0x3FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000391, 0x00000300)] = 0x1FBA;
      m_composeMap[make_pair(0x00000391, 0x00000301)] = 0x0386;
      m_composeMap[make_pair(0x00000391, 0x00000304)] = 0x1FB9;
      m_composeMap[make_pair(0x00000391, 0x00000306)] = 0x1FB8;
      m_composeMap[make_pair(0x00000391, 0x00000313)] = 0x1F08;
      m_composeMap[make_pair(0x00000391, 0x00000314)] = 0x1F09;
      m_composeMap[make_pair(0x00000391, 0x00000345)] = 0x1FBC;
      m_composeMap[make_pair(0x00000395, 0x00000300)] = 0x1FC8;
      m_composeMap[make_pair(0x00000395, 0x00000301)] = 0x0388;
      m_composeMap[make_pair(0x00000395, 0x00000313)] = 0x1F18;
      m_composeMap[make_pair(0x00000395, 0x00000314)] = 0x1F19;
      m_composeMap[make_pair(0x00000397, 0x00000300)] = 0x1FCA;
      m_composeMap[make_pair(0x00000397, 0x00000301)] = 0x0389;
      m_composeMap[make_pair(0x00000397, 0x00000313)] = 0x1F28;
      m_composeMap[make_pair(0x00000397, 0x00000314)] = 0x1F29;
      m_composeMap[make_pair(0x00000397, 0x00000345)] = 0x1FCC;
      m_composeMap[make_pair(0x00000399, 0x00000300)] = 0x1FDA;
      m_composeMap[make_pair(0x00000399, 0x00000301)] = 0x038A;
      m_composeMap[make_pair(0x00000399, 0x00000304)] = 0x1FD9;
      m_composeMap[make_pair(0x00000399, 0x00000306)] = 0x1FD8;
      m_composeMap[make_pair(0x00000399, 0x00000308)] = 0x03AA;
      m_composeMap[make_pair(0x00000399, 0x00000313)] = 0x1F38;
      m_composeMap[make_pair(0x00000399, 0x00000314)] = 0x1F39;
      m_composeMap[make_pair(0x0000039F, 0x00000300)] = 0x1FF8;
      m_composeMap[make_pair(0x0000039F, 0x00000301)] = 0x038C;
      m_composeMap[make_pair(0x0000039F, 0x00000313)] = 0x1F48;
      m_composeMap[make_pair(0x0000039F, 0x00000314)] = 0x1F49;
      m_composeMap[make_pair(0x000003A1, 0x00000314)] = 0x1FEC;
      m_composeMap[make_pair(0x000003A5, 0x00000300)] = 0x1FEA;
      m_composeMap[make_pair(0x000003A5, 0x00000301)] = 0x038E;
      m_composeMap[make_pair(0x000003A5, 0x00000304)] = 0x1FE9;
      m_composeMap[make_pair(0x000003A5, 0x00000306)] = 0x1FE8;
      m_composeMap[make_pair(0x000003A5, 0x00000308)] = 0x03AB;
      m_composeMap[make_pair(0x000003A5, 0x00000314)] = 0x1F59;
      m_composeMap[make_pair(0x000003A9, 0x00000300)] = 0x1FFA;
      m_composeMap[make_pair(0x000003A9, 0x00000301)] = 0x038F;
      m_composeMap[make_pair(0x000003A9, 0x00000313)] = 0x1F68;
      m_composeMap[make_pair(0x000003A9, 0x00000314)] = 0x1F69;
      m_composeMap[make_pair(0x000003A9, 0x00000345)] = 0x1FFC;
      m_composeMap[make_pair(0x000003AC, 0x00000345)] = 0x1FB4;
      m_composeMap[make_pair(0x000003AE, 0x00000345)] = 0x1FC4;
      m_composeMap[make_pair(0x000003B1, 0x00000300)] = 0x1F70;
      m_composeMap[make_pair(0x000003B1, 0x00000301)] = 0x03AC;
      m_composeMap[make_pair(0x000003B1, 0x00000304)] = 0x1FB1;
      m_composeMap[make_pair(0x000003B1, 0x00000306)] = 0x1FB0;
      m_composeMap[make_pair(0x000003B1, 0x00000313)] = 0x1F00;
      m_composeMap[make_pair(0x000003B1, 0x00000314)] = 0x1F01;
      m_composeMap[make_pair(0x000003B1, 0x00000342)] = 0x1FB6;
      m_composeMap[make_pair(0x000003B1, 0x00000345)] = 0x1FB3;
      m_composeMap[make_pair(0x000003B5, 0x00000300)] = 0x1F72;
      m_composeMap[make_pair(0x000003B5, 0x00000301)] = 0x03AD;
      m_composeMap[make_pair(0x000003B5, 0x00000313)] = 0x1F10;
      m_composeMap[make_pair(0x000003B5, 0x00000314)] = 0x1F11;
      m_composeMap[make_pair(0x000003B7, 0x00000300)] = 0x1F74;
      m_composeMap[make_pair(0x000003B7, 0x00000301)] = 0x03AE;
      m_composeMap[make_pair(0x000003B7, 0x00000313)] = 0x1F20;
      m_composeMap[make_pair(0x000003B7, 0x00000314)] = 0x1F21;
      m_composeMap[make_pair(0x000003B7, 0x00000342)] = 0x1FC6;
      m_composeMap[make_pair(0x000003B7, 0x00000345)] = 0x1FC3;
      m_composeMap[make_pair(0x000003B9, 0x00000300)] = 0x1F76;
      m_composeMap[make_pair(0x000003B9, 0x00000301)] = 0x03AF;
      m_composeMap[make_pair(0x000003B9, 0x00000304)] = 0x1FD1;
      m_composeMap[make_pair(0x000003B9, 0x00000306)] = 0x1FD0;
      m_composeMap[make_pair(0x000003B9, 0x00000308)] = 0x03CA;
      m_composeMap[make_pair(0x000003B9, 0x00000313)] = 0x1F30;
      m_composeMap[make_pair(0x000003B9, 0x00000314)] = 0x1F31;
      m_composeMap[make_pair(0x000003B9, 0x00000342)] = 0x1FD6;
      m_composeMap[make_pair(0x000003BF, 0x00000300)] = 0x1F78;
      m_composeMap[make_pair(0x000003BF, 0x00000301)] = 0x03CC;
      m_composeMap[make_pair(0x000003BF, 0x00000313)] = 0x1F40;
      m_composeMap[make_pair(0x000003BF, 0x00000314)] = 0x1F41;
      m_composeMap[make_pair(0x000003C1, 0x00000313)] = 0x1FE4;
      m_composeMap[make_pair(0x000003C1, 0x00000314)] = 0x1FE5;
      m_composeMap[make_pair(0x000003C5, 0x00000300)] = 0x1F7A;
      m_composeMap[make_pair(0x000003C5, 0x00000301)] = 0x03CD;
      m_composeMap[make_pair(0x000003C5, 0x00000304)] = 0x1FE1;
      m_composeMap[make_pair(0x000003C5, 0x00000306)] = 0x1FE0;
      m_composeMap[make_pair(0x000003C5, 0x00000308)] = 0x03CB;
      m_composeMap[make_pair(0x000003C5, 0x00000313)] = 0x1F50;
      m_composeMap[make_pair(0x000003C5, 0x00000314)] = 0x1F51;
      m_composeMap[make_pair(0x000003C5, 0x00000342)] = 0x1FE6;
      m_composeMap[make_pair(0x000003C9, 0x00000300)] = 0x1F7C;
      m_composeMap[make_pair(0x000003C9, 0x00000301)] = 0x03CE;
      m_composeMap[make_pair(0x000003C9, 0x00000313)] = 0x1F60;
      m_composeMap[make_pair(0x000003C9, 0x00000314)] = 0x1F61;
      m_composeMap[make_pair(0x000003C9, 0x00000342)] = 0x1FF6;
      m_composeMap[make_pair(0x000003C9, 0x00000345)] = 0x1FF3;
      m_composeMap[make_pair(0x000003CA, 0x00000300)] = 0x1FD2;
      m_composeMap[make_pair(0x000003CA, 0x00000301)] = 0x0390;
      m_composeMap[make_pair(0x000003CA, 0x00000342)] = 0x1FD7;
      m_composeMap[make_pair(0x000003CB, 0x00000300)] = 0x1FE2;
      m_composeMap[make_pair(0x000003CB, 0x00000301)] = 0x03B0;
      m_composeMap[make_pair(0x000003CB, 0x00000342)] = 0x1FE7;
      m_composeMap[make_pair(0x000003CE, 0x00000345)] = 0x1FF4;
      m_composeMap[make_pair(0x000003D2, 0x00000301)] = 0x03D3;
      m_composeMap[make_pair(0x000003D2, 0x00000308)] = 0x03D4;

    }


    ~Greek370() {
    }

    UCS4 first_letter() const {
      return m_first_letter;
    }

    UCS4 last_letter() const {
      return m_last_letter;
    }

    bool is_undef_block() const {
      return 0;
    }

    // query functions:

    std::string blockname(const UCS4 uc) const {
      return "Greek";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Greek370::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Greek370::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Greek370::m_title[uc - m_first_letter];
    }

    int dec_digit_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      return 0;
    }

    int digit_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Digit(const UCS4 uc) const {
      return 0;
    }

    float numeric_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Numeric(const UCS4 uc) const {
      return 0;
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Greek370::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Greek370::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Greek370::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Greek370::m_decompStr[uc - m_first_letter][0];
      us[1] = Greek370::m_decompStr[uc - m_first_letter][1];
      if (us[1] == 0x0000u) {
        us.resize(1);
      }

      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(Greek370::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Greek370::m_ea[uc - m_first_letter]);
    }

    UCS4 compose (const UCS4 start, const UCS4 last) {
      return m_composeMap[make_pair(start, last)];
    }

    bool is_White_space(const UCS4 uc) const {
      return 0;
    }

    bool is_Bidi_Control(const UCS4 uc) const {
      return 0;
    }

    bool is_Join_Control(const UCS4 uc) const {
      return 0;
    }

    bool is_Dash(const UCS4 uc) const {
      return 0;
    }

    bool is_Hyphen(const UCS4 uc) const {
      return 0;
    }

    bool is_Quotation_Mark(const UCS4 uc) const {
      return 0;
    }

    bool is_Terminal_Punctuation(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Math(const UCS4 uc) const {
      return 0;
    }

    bool is_Hex_Digit(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Alphabetic(const UCS4 uc) const {
      return 0;
    }

    bool is_Ideographic(const UCS4 uc) const {
      return 0;
    }

    bool is_Diacritic(const UCS4 uc) const {
      return m_Diacritic.test(uc - m_first_letter);
    }

    bool is_Extender(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Lowercase(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Uppercase(const UCS4 uc) const {
      return 0;
    }

    bool is_Noncharacter_Code_Point(const UCS4 uc) const {
      return 0;
    }


  private:
    // functions
    Greek370(const Greek370 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<144> m_is_defined;
    static const UCS4 m_upper[144];
    static const UCS4 m_lower[144];
    static const UCS4 m_title[144];
    static const unsigned char _cat[144];
    static const unsigned char m_bidir[144];
    static const unsigned char _decomp[144];
    static const UCS2 m_decompStr[144][2];
    static const unsigned char m_lb[144];
    static const unsigned char m_ea[144];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<144> m_Diacritic;

  }; // class Greek370

    const std::bitset<144> Greek370::m_is_defined(std::string("000000000011111111111111111111111111110011111111011111111111111111111111111111111111111111111011111111111111111111010111111100000100010000110000"));

  const UCS4 Greek370::m_upper[] = {
    0x0370, 0x0371, 0x0372, 0x0373, 0x0374, 0x0375, 0x0376, 0x0377, 
    0x0378, 0x0379, 0x037A, 0x037B, 0x037C, 0x037D, 0x037E, 0x037F, 
    0x0380, 0x0381, 0x0382, 0x0383, 0x0384, 0x0385, 0x0386, 0x0387, 
    0x0388, 0x0389, 0x038A, 0x038B, 0x038C, 0x038D, 0x038E, 0x038F, 
    0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 
    0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 
    0x03A0, 0x03A1, 0x03A2, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 
    0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x0386, 0x0388, 0x0389, 0x038A, 
    0x03B0, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 
    0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 
    0x03A0, 0x03A1, 0x03A3, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 
    0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x038C, 0x038E, 0x038F, 0x03CF, 
    0x0392, 0x0398, 0x03D2, 0x03D3, 0x03D4, 0x03A6, 0x03A0, 0x03D7, 
    0x03D8, 0x03D9, 0x03DA, 0x03DA, 0x03DC, 0x03DC, 0x03DE, 0x03DE, 
    0x03E0, 0x03E0, 0x03E2, 0x03E2, 0x03E4, 0x03E4, 0x03E6, 0x03E6, 
    0x03E8, 0x03E8, 0x03EA, 0x03EA, 0x03EC, 0x03EC, 0x03EE, 0x03EE, 
    0x039A, 0x03A1, 0x03A3, 0x03F3, 0x03F4, 0x0395, 0x03F6, 0x03F7, 
    0x03F8, 0x03F9, 0x03FA, 0x03FB, 0x03FC, 0x03FD, 0x03FE, 0x03FF
  };

  const UCS4 Greek370::m_lower[] = {
    0x0370, 0x0371, 0x0372, 0x0373, 0x0374, 0x0375, 0x0376, 0x0377, 
    0x0378, 0x0379, 0x037A, 0x037B, 0x037C, 0x037D, 0x037E, 0x037F, 
    0x0380, 0x0381, 0x0382, 0x0383, 0x0384, 0x0385, 0x03AC, 0x0387, 
    0x03AD, 0x03AE, 0x03AF, 0x038B, 0x03CC, 0x038D, 0x03CD, 0x03CE, 
    0x0390, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 
    0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF, 
    0x03C0, 0x03C1, 0x03A2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7, 
    0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03AC, 0x03AD, 0x03AE, 0x03AF, 
    0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 
    0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF, 
    0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7, 
    0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, 0x03CF, 
    0x03D0, 0x03D1, 0x03D2, 0x03D3, 0x03D4, 0x03D5, 0x03D6, 0x03D7, 
    0x03D8, 0x03D9, 0x03DB, 0x03DB, 0x03DD, 0x03DD, 0x03DF, 0x03DF, 
    0x03E1, 0x03E1, 0x03E3, 0x03E3, 0x03E5, 0x03E5, 0x03E7, 0x03E7, 
    0x03E9, 0x03E9, 0x03EB, 0x03EB, 0x03ED, 0x03ED, 0x03EF, 0x03EF, 
    0x03F0, 0x03F1, 0x03F2, 0x03F3, 0x03B8, 0x03F5, 0x03F6, 0x03F7, 
    0x03F8, 0x03F9, 0x03FA, 0x03FB, 0x03FC, 0x03FD, 0x03FE, 0x03FF
  };

  const UCS4 Greek370::m_title[] = {
    0x0370, 0x0371, 0x0372, 0x0373, 0x0374, 0x0375, 0x0376, 0x0377, 
    0x0378, 0x0379, 0x037A, 0x037B, 0x037C, 0x037D, 0x037E, 0x037F, 
    0x0380, 0x0381, 0x0382, 0x0383, 0x0384, 0x0385, 0x0386, 0x0387, 
    0x0388, 0x0389, 0x038A, 0x038B, 0x038C, 0x038D, 0x038E, 0x038F, 
    0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 
    0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 
    0x03A0, 0x03A1, 0x03A2, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 
    0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x0386, 0x0388, 0x0389, 0x038A, 
    0x03B0, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 
    0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 
    0x03A0, 0x03A1, 0x03A3, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 
    0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x038C, 0x038E, 0x038F, 0x03CF, 
    0x0392, 0x0398, 0x03D2, 0x03D3, 0x03D4, 0x03A6, 0x03A0, 0x03D7, 
    0x03D8, 0x03D9, 0x03DA, 0x03DA, 0x03DC, 0x03DC, 0x03DE, 0x03DE, 
    0x03E0, 0x03E0, 0x03E2, 0x03E2, 0x03E4, 0x03E4, 0x03E6, 0x03E6, 
    0x03E8, 0x03E8, 0x03EA, 0x03EA, 0x03EC, 0x03EC, 0x03EE, 0x03EE, 
    0x039A, 0x03A1, 0x03A3, 0x03F3, 0x03F4, 0x0395, 0x03F6, 0x03F7, 
    0x03F8, 0x03F9, 0x03FA, 0x03FB, 0x03FC, 0x03FD, 0x03FE, 0x03FF
  };

  const unsigned char Greek370::_cat[] = {
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Po, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Lu, CAT_Po, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Sk, CAT_Lu, CAT_Sk, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Sk, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Sk, 
    CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Sk, CAT_Sk, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk
  };

  const unsigned char Greek370::m_bidir[] = {
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON
  };

  const unsigned char Greek370::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Greek370::m_decompStr[][2] = {
    { 0x0370u, 0x0000u }, { 0x0371u, 0x0000u }, { 0x0372u, 0x0000u }, { 0x0373u, 0x0000u }, 
    { 0x02B9u, 0x0000u }, { 0x0375u, 0x0000u }, { 0x0376u, 0x0000u }, { 0x0377u, 0x0000u }, 
    { 0x0378u, 0x0000u }, { 0x0379u, 0x0000u }, { 0x0020u, 0x0345u }, { 0x037Bu, 0x0000u }, 
    { 0x037Cu, 0x0000u }, { 0x037Du, 0x0000u }, { 0x003Bu, 0x0000u }, { 0x037Fu, 0x0000u }, 
    { 0x0380u, 0x0000u }, { 0x0381u, 0x0000u }, { 0x0382u, 0x0000u }, { 0x0383u, 0x0000u }, 
    { 0x0020u, 0x0301u }, { 0x00A8u, 0x0301u }, { 0x0391u, 0x0301u }, { 0x00B7u, 0x0000u }, 
    { 0x0395u, 0x0301u }, { 0x0397u, 0x0301u }, { 0x0399u, 0x0301u }, { 0x038Bu, 0x0000u }, 
    { 0x039Fu, 0x0301u }, { 0x038Du, 0x0000u }, { 0x03A5u, 0x0301u }, { 0x03A9u, 0x0301u }, 
    { 0x03CAu, 0x0301u }, { 0x0391u, 0x0000u }, { 0x0392u, 0x0000u }, { 0x0393u, 0x0000u }, 
    { 0x0394u, 0x0000u }, { 0x0395u, 0x0000u }, { 0x0396u, 0x0000u }, { 0x0397u, 0x0000u }, 
    { 0x0398u, 0x0000u }, { 0x0399u, 0x0000u }, { 0x039Au, 0x0000u }, { 0x039Bu, 0x0000u }, 
    { 0x039Cu, 0x0000u }, { 0x039Du, 0x0000u }, { 0x039Eu, 0x0000u }, { 0x039Fu, 0x0000u }, 
    { 0x03A0u, 0x0000u }, { 0x03A1u, 0x0000u }, { 0x03A2u, 0x0000u }, { 0x03A3u, 0x0000u }, 
    { 0x03A4u, 0x0000u }, { 0x03A5u, 0x0000u }, { 0x03A6u, 0x0000u }, { 0x03A7u, 0x0000u }, 
    { 0x03A8u, 0x0000u }, { 0x03A9u, 0x0000u }, { 0x0399u, 0x0308u }, { 0x03A5u, 0x0308u }, 
    { 0x03B1u, 0x0301u }, { 0x03B5u, 0x0301u }, { 0x03B7u, 0x0301u }, { 0x03B9u, 0x0301u }, 
    { 0x03CBu, 0x0301u }, { 0x03B1u, 0x0000u }, { 0x03B2u, 0x0000u }, { 0x03B3u, 0x0000u }, 
    { 0x03B4u, 0x0000u }, { 0x03B5u, 0x0000u }, { 0x03B6u, 0x0000u }, { 0x03B7u, 0x0000u }, 
    { 0x03B8u, 0x0000u }, { 0x03B9u, 0x0000u }, { 0x03BAu, 0x0000u }, { 0x03BBu, 0x0000u }, 
    { 0x03BCu, 0x0000u }, { 0x03BDu, 0x0000u }, { 0x03BEu, 0x0000u }, { 0x03BFu, 0x0000u }, 
    { 0x03C0u, 0x0000u }, { 0x03C1u, 0x0000u }, { 0x03C2u, 0x0000u }, { 0x03C3u, 0x0000u }, 
    { 0x03C4u, 0x0000u }, { 0x03C5u, 0x0000u }, { 0x03C6u, 0x0000u }, { 0x03C7u, 0x0000u }, 
    { 0x03C8u, 0x0000u }, { 0x03C9u, 0x0000u }, { 0x03B9u, 0x0308u }, { 0x03C5u, 0x0308u }, 
    { 0x03BFu, 0x0301u }, { 0x03C5u, 0x0301u }, { 0x03C9u, 0x0301u }, { 0x03CFu, 0x0000u }, 
    { 0x03B2u, 0x0000u }, { 0x03B8u, 0x0000u }, { 0x03A5u, 0x0000u }, { 0x03D2u, 0x0301u }, 
    { 0x03D2u, 0x0308u }, { 0x03C6u, 0x0000u }, { 0x03C0u, 0x0000u }, { 0x03D7u, 0x0000u }, 
    { 0x03D8u, 0x0000u }, { 0x03D9u, 0x0000u }, { 0x03DAu, 0x0000u }, { 0x03DBu, 0x0000u }, 
    { 0x03DCu, 0x0000u }, { 0x03DDu, 0x0000u }, { 0x03DEu, 0x0000u }, { 0x03DFu, 0x0000u }, 
    { 0x03E0u, 0x0000u }, { 0x03E1u, 0x0000u }, { 0x03E2u, 0x0000u }, { 0x03E3u, 0x0000u }, 
    { 0x03E4u, 0x0000u }, { 0x03E5u, 0x0000u }, { 0x03E6u, 0x0000u }, { 0x03E7u, 0x0000u }, 
    { 0x03E8u, 0x0000u }, { 0x03E9u, 0x0000u }, { 0x03EAu, 0x0000u }, { 0x03EBu, 0x0000u }, 
    { 0x03ECu, 0x0000u }, { 0x03EDu, 0x0000u }, { 0x03EEu, 0x0000u }, { 0x03EFu, 0x0000u }, 
    { 0x03BAu, 0x0000u }, { 0x03C1u, 0x0000u }, { 0x03C2u, 0x0000u }, { 0x03F3u, 0x0000u }, 
    { 0x0398u, 0x0000u }, { 0x03B5u, 0x0000u }, { 0x03F6u, 0x0000u }, { 0x03F7u, 0x0000u }, 
    { 0x03F8u, 0x0000u }, { 0x03F9u, 0x0000u }, { 0x03FAu, 0x0000u }, { 0x03FBu, 0x0000u }, 
    { 0x03FCu, 0x0000u }, { 0x03FDu, 0x0000u }, { 0x03FEu, 0x0000u }, { 0x03FFu, 0x0000u }
  };

  const unsigned char Greek370::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Greek370::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

    const std::bitset<144> Greek370::m_Diacritic(std::string("000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001100000000000000110000"));

}; // namespace Babylon

dload(Babylon::Greek370);
