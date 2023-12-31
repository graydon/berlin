/*$Id: 80-FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:45:57 +0200.
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

  class Latin1_Supplement80 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Latin1_Supplement80() {
      m_first_letter = 0x80;
      m_last_letter  = 0xFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x000000A8, 0x00000300)] = 0x1FED;
      m_composeMap[make_pair(0x000000A8, 0x00000301)] = 0x0385;
      m_composeMap[make_pair(0x000000A8, 0x00000342)] = 0x1FC1;
      m_composeMap[make_pair(0x000000C2, 0x00000300)] = 0x1EA6;
      m_composeMap[make_pair(0x000000C2, 0x00000301)] = 0x1EA4;
      m_composeMap[make_pair(0x000000C2, 0x00000303)] = 0x1EAA;
      m_composeMap[make_pair(0x000000C2, 0x00000309)] = 0x1EA8;
      m_composeMap[make_pair(0x000000C4, 0x00000304)] = 0x01DE;
      m_composeMap[make_pair(0x000000C5, 0x00000301)] = 0x01FA;
      m_composeMap[make_pair(0x000000C6, 0x00000301)] = 0x01FC;
      m_composeMap[make_pair(0x000000C6, 0x00000304)] = 0x01E2;
      m_composeMap[make_pair(0x000000C7, 0x00000301)] = 0x1E08;
      m_composeMap[make_pair(0x000000CA, 0x00000300)] = 0x1EC0;
      m_composeMap[make_pair(0x000000CA, 0x00000301)] = 0x1EBE;
      m_composeMap[make_pair(0x000000CA, 0x00000303)] = 0x1EC4;
      m_composeMap[make_pair(0x000000CA, 0x00000309)] = 0x1EC2;
      m_composeMap[make_pair(0x000000CF, 0x00000301)] = 0x1E2E;
      m_composeMap[make_pair(0x000000D4, 0x00000300)] = 0x1ED2;
      m_composeMap[make_pair(0x000000D4, 0x00000301)] = 0x1ED0;
      m_composeMap[make_pair(0x000000D4, 0x00000303)] = 0x1ED6;
      m_composeMap[make_pair(0x000000D4, 0x00000309)] = 0x1ED4;
      m_composeMap[make_pair(0x000000D5, 0x00000301)] = 0x1E4C;
      m_composeMap[make_pair(0x000000D5, 0x00000304)] = 0x022C;
      m_composeMap[make_pair(0x000000D5, 0x00000308)] = 0x1E4E;
      m_composeMap[make_pair(0x000000D6, 0x00000304)] = 0x022A;
      m_composeMap[make_pair(0x000000D8, 0x00000301)] = 0x01FE;
      m_composeMap[make_pair(0x000000DC, 0x00000300)] = 0x01DB;
      m_composeMap[make_pair(0x000000DC, 0x00000301)] = 0x01D7;
      m_composeMap[make_pair(0x000000DC, 0x00000304)] = 0x01D5;
      m_composeMap[make_pair(0x000000DC, 0x0000030C)] = 0x01D9;
      m_composeMap[make_pair(0x000000E2, 0x00000300)] = 0x1EA7;
      m_composeMap[make_pair(0x000000E2, 0x00000301)] = 0x1EA5;
      m_composeMap[make_pair(0x000000E2, 0x00000303)] = 0x1EAB;
      m_composeMap[make_pair(0x000000E2, 0x00000309)] = 0x1EA9;
      m_composeMap[make_pair(0x000000E4, 0x00000304)] = 0x01DF;
      m_composeMap[make_pair(0x000000E5, 0x00000301)] = 0x01FB;
      m_composeMap[make_pair(0x000000E6, 0x00000301)] = 0x01FD;
      m_composeMap[make_pair(0x000000E6, 0x00000304)] = 0x01E3;
      m_composeMap[make_pair(0x000000E7, 0x00000301)] = 0x1E09;
      m_composeMap[make_pair(0x000000EA, 0x00000300)] = 0x1EC1;
      m_composeMap[make_pair(0x000000EA, 0x00000301)] = 0x1EBF;
      m_composeMap[make_pair(0x000000EA, 0x00000303)] = 0x1EC5;
      m_composeMap[make_pair(0x000000EA, 0x00000309)] = 0x1EC3;
      m_composeMap[make_pair(0x000000EF, 0x00000301)] = 0x1E2F;
      m_composeMap[make_pair(0x000000F4, 0x00000300)] = 0x1ED3;
      m_composeMap[make_pair(0x000000F4, 0x00000301)] = 0x1ED1;
      m_composeMap[make_pair(0x000000F4, 0x00000303)] = 0x1ED7;
      m_composeMap[make_pair(0x000000F4, 0x00000309)] = 0x1ED5;
      m_composeMap[make_pair(0x000000F5, 0x00000301)] = 0x1E4D;
      m_composeMap[make_pair(0x000000F5, 0x00000304)] = 0x022D;
      m_composeMap[make_pair(0x000000F5, 0x00000308)] = 0x1E4F;
      m_composeMap[make_pair(0x000000F6, 0x00000304)] = 0x022B;
      m_composeMap[make_pair(0x000000F8, 0x00000301)] = 0x01FF;
      m_composeMap[make_pair(0x000000FC, 0x00000300)] = 0x01DC;
      m_composeMap[make_pair(0x000000FC, 0x00000301)] = 0x01D8;
      m_composeMap[make_pair(0x000000FC, 0x00000304)] = 0x01D6;
      m_composeMap[make_pair(0x000000FC, 0x0000030C)] = 0x01DA;

    }


    ~Latin1_Supplement80() {
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
      return "Latin-1 Supplement";
    }

    bool is_defined(const UCS4 uc) const {
      return 1;
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Latin1_Supplement80::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Latin1_Supplement80::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Latin1_Supplement80::m_title[uc - m_first_letter];
    }

    int dec_digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x00B2u:
        return 2;
        break;
      case 0x00B3u:
        return 3;
        break;
      case 0x00B9u:
        return 1;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x00B2u:
      case 0x00B3u:
      case 0x00B9u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x00B2u:
        return 2;
        break;
      case 0x00B3u:
        return 3;
        break;
      case 0x00B9u:
        return 1;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x00B2u:
      case 0x00B3u:
      case 0x00B9u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x00B2u:
        return 2.000000;
        break;
      case 0x00B3u:
        return 3.000000;
        break;
      case 0x00B9u:
        return 1.000000;
        break;
      case 0x00BCu:
        return 0.250000;
        break;
      case 0x00BDu:
        return 0.500000;
        break;
      case 0x00BEu:
        return 0.750000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x00B2u:
      case 0x00B3u:
      case 0x00B9u:
      case 0x00BCu:
      case 0x00BDu:
      case 0x00BEu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Latin1_Supplement80::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Latin1_Supplement80::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Latin1_Supplement80::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Latin1_Supplement80::m_decompStr[uc - m_first_letter][0];
      us[1] = Latin1_Supplement80::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0x00BC:
        us.resize(3);
        us[2u] = 0x0034u;
        break;

      case 0x00BD:
        us.resize(3);
        us[2u] = 0x0032u;
        break;

      case 0x00BE:
        us.resize(3);
        us[2u] = 0x0034u;
        break;
      }
      if (us[1] == 0x0000u) {
        us.resize(1);
      }

      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return m_mirror.test(uc - m_first_letter);
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(Latin1_Supplement80::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Latin1_Supplement80::m_ea[uc - m_first_letter]);
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
      return 0;
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
    Latin1_Supplement80(const Latin1_Supplement80 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const UCS4 m_upper[128];
    static const UCS4 m_lower[128];
    static const UCS4 m_title[128];
    static const unsigned char _cat[128];
    static const unsigned char m_bidir[128];
    static const unsigned char _decomp[128];
    static const UCS2 m_decompStr[128][2];
    static const std::bitset<128> m_mirror;
    static const unsigned char m_lb[128];
    static const unsigned char m_ea[128];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Latin1_Supplement80

  const UCS4 Latin1_Supplement80::m_upper[] = {
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, 
    0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
    0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, 
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, 
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, 
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x039C, 0x00B6, 0x00B7, 
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00F7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x0178
  };

  const UCS4 Latin1_Supplement80::m_lower[] = {
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, 
    0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
    0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, 
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, 
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, 
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7, 
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, 
    0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7, 
    0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF, 
    0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00D7, 
    0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00DF, 
    0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7, 
    0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF, 
    0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7, 
    0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF
  };

  const UCS4 Latin1_Supplement80::m_title[] = {
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, 
    0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
    0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, 
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, 
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, 
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x039C, 0x00B6, 0x00B7, 
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00F7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x0178
  };

  const unsigned char Latin1_Supplement80::_cat[] = {
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Zs, CAT_Po, CAT_Sc, CAT_Sc, CAT_Sc, CAT_Sc, CAT_So, CAT_So, 
    CAT_Sk, CAT_So, CAT_Ll, CAT_Pi, CAT_Sm, CAT_Pd, CAT_So, CAT_Sk, 
    CAT_So, CAT_Sm, CAT_No, CAT_No, CAT_Sk, CAT_Ll, CAT_So, CAT_Po, 
    CAT_Sk, CAT_No, CAT_Ll, CAT_Pf, CAT_No, CAT_No, CAT_No, CAT_Po, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Sm, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Sm, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll
  };

  const unsigned char Latin1_Supplement80::m_bidir[] = {
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_B, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_CS, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ET, BIDIR_ET, BIDIR_EN, BIDIR_EN, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_EN, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const unsigned char Latin1_Supplement80::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_NOBREAK, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_SUPER, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_SUPER, DECOMP_SUPER, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_SUPER, DECOMP_SUPER, DECOMP_CANONICAL, DECOMP_FRACTION, DECOMP_FRACTION, DECOMP_FRACTION, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Latin1_Supplement80::m_decompStr[][2] = {
    { 0x0080u, 0x0000u }, { 0x0081u, 0x0000u }, { 0x0082u, 0x0000u }, { 0x0083u, 0x0000u }, 
    { 0x0084u, 0x0000u }, { 0x0085u, 0x0000u }, { 0x0086u, 0x0000u }, { 0x0087u, 0x0000u }, 
    { 0x0088u, 0x0000u }, { 0x0089u, 0x0000u }, { 0x008Au, 0x0000u }, { 0x008Bu, 0x0000u }, 
    { 0x008Cu, 0x0000u }, { 0x008Du, 0x0000u }, { 0x008Eu, 0x0000u }, { 0x008Fu, 0x0000u }, 
    { 0x0090u, 0x0000u }, { 0x0091u, 0x0000u }, { 0x0092u, 0x0000u }, { 0x0093u, 0x0000u }, 
    { 0x0094u, 0x0000u }, { 0x0095u, 0x0000u }, { 0x0096u, 0x0000u }, { 0x0097u, 0x0000u }, 
    { 0x0098u, 0x0000u }, { 0x0099u, 0x0000u }, { 0x009Au, 0x0000u }, { 0x009Bu, 0x0000u }, 
    { 0x009Cu, 0x0000u }, { 0x009Du, 0x0000u }, { 0x009Eu, 0x0000u }, { 0x009Fu, 0x0000u }, 
    { 0x0020u, 0x0000u }, { 0x00A1u, 0x0000u }, { 0x00A2u, 0x0000u }, { 0x00A3u, 0x0000u }, 
    { 0x00A4u, 0x0000u }, { 0x00A5u, 0x0000u }, { 0x00A6u, 0x0000u }, { 0x00A7u, 0x0000u }, 
    { 0x0020u, 0x0308u }, { 0x00A9u, 0x0000u }, { 0x0061u, 0x0000u }, { 0x00ABu, 0x0000u }, 
    { 0x00ACu, 0x0000u }, { 0x00ADu, 0x0000u }, { 0x00AEu, 0x0000u }, { 0x0020u, 0x0304u }, 
    { 0x00B0u, 0x0000u }, { 0x00B1u, 0x0000u }, { 0x0032u, 0x0000u }, { 0x0033u, 0x0000u }, 
    { 0x0020u, 0x0301u }, { 0x03BCu, 0x0000u }, { 0x00B6u, 0x0000u }, { 0x00B7u, 0x0000u }, 
    { 0x0020u, 0x0327u }, { 0x0031u, 0x0000u }, { 0x006Fu, 0x0000u }, { 0x00BBu, 0x0000u }, 
    { 0x0031u, 0x2044u }, { 0x0031u, 0x2044u }, { 0x0033u, 0x2044u }, { 0x00BFu, 0x0000u }, 
    { 0x0041u, 0x0300u }, { 0x0041u, 0x0301u }, { 0x0041u, 0x0302u }, { 0x0041u, 0x0303u }, 
    { 0x0041u, 0x0308u }, { 0x0041u, 0x030Au }, { 0x00C6u, 0x0000u }, { 0x0043u, 0x0327u }, 
    { 0x0045u, 0x0300u }, { 0x0045u, 0x0301u }, { 0x0045u, 0x0302u }, { 0x0045u, 0x0308u }, 
    { 0x0049u, 0x0300u }, { 0x0049u, 0x0301u }, { 0x0049u, 0x0302u }, { 0x0049u, 0x0308u }, 
    { 0x00D0u, 0x0000u }, { 0x004Eu, 0x0303u }, { 0x004Fu, 0x0300u }, { 0x004Fu, 0x0301u }, 
    { 0x004Fu, 0x0302u }, { 0x004Fu, 0x0303u }, { 0x004Fu, 0x0308u }, { 0x00D7u, 0x0000u }, 
    { 0x00D8u, 0x0000u }, { 0x0055u, 0x0300u }, { 0x0055u, 0x0301u }, { 0x0055u, 0x0302u }, 
    { 0x0055u, 0x0308u }, { 0x0059u, 0x0301u }, { 0x00DEu, 0x0000u }, { 0x00DFu, 0x0000u }, 
    { 0x0061u, 0x0300u }, { 0x0061u, 0x0301u }, { 0x0061u, 0x0302u }, { 0x0061u, 0x0303u }, 
    { 0x0061u, 0x0308u }, { 0x0061u, 0x030Au }, { 0x00E6u, 0x0000u }, { 0x0063u, 0x0327u }, 
    { 0x0065u, 0x0300u }, { 0x0065u, 0x0301u }, { 0x0065u, 0x0302u }, { 0x0065u, 0x0308u }, 
    { 0x0069u, 0x0300u }, { 0x0069u, 0x0301u }, { 0x0069u, 0x0302u }, { 0x0069u, 0x0308u }, 
    { 0x00F0u, 0x0000u }, { 0x006Eu, 0x0303u }, { 0x006Fu, 0x0300u }, { 0x006Fu, 0x0301u }, 
    { 0x006Fu, 0x0302u }, { 0x006Fu, 0x0303u }, { 0x006Fu, 0x0308u }, { 0x00F7u, 0x0000u }, 
    { 0x00F8u, 0x0000u }, { 0x0075u, 0x0300u }, { 0x0075u, 0x0301u }, { 0x0075u, 0x0302u }, 
    { 0x0075u, 0x0308u }, { 0x0079u, 0x0301u }, { 0x00FEu, 0x0000u }, { 0x0079u, 0x0308u }
  };

  const std::bitset<128> Latin1_Supplement80::m_mirror(std::string("00000000000000000000000000000000000000000000000000000000000000000000100000000000000010000000000000000000000000000000000000000000"));

  const unsigned char Latin1_Supplement80::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_GL, LB_AI, LB_PO, LB_PR, LB_PR, LB_PR, LB_AL, LB_AI, 
    LB_AI, LB_AL, LB_AI, LB_QU, LB_AL, LB_BA, LB_AL, LB_AL, 
    LB_PO, LB_PR, LB_AI, LB_AI, LB_BB, LB_AL, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_QU, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL
  };

  const unsigned char Latin1_Supplement80::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_A, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_Na, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_Na, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N
  };

}; // namespace Babylon

dload(Babylon::Latin1_Supplement80);
