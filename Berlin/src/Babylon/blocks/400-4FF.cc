/*$Id: 400-4FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:22 +0200.
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

  class Cyrillic400 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Cyrillic400() {
      m_first_letter = 0x400;
      m_last_letter  = 0x4FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000406, 0x00000308)] = 0x0407;
      m_composeMap[make_pair(0x00000410, 0x00000306)] = 0x04D0;
      m_composeMap[make_pair(0x00000410, 0x00000308)] = 0x04D2;
      m_composeMap[make_pair(0x00000413, 0x00000301)] = 0x0403;
      m_composeMap[make_pair(0x00000415, 0x00000300)] = 0x0400;
      m_composeMap[make_pair(0x00000415, 0x00000306)] = 0x04D6;
      m_composeMap[make_pair(0x00000415, 0x00000308)] = 0x0401;
      m_composeMap[make_pair(0x00000416, 0x00000306)] = 0x04C1;
      m_composeMap[make_pair(0x00000416, 0x00000308)] = 0x04DC;
      m_composeMap[make_pair(0x00000417, 0x00000308)] = 0x04DE;
      m_composeMap[make_pair(0x00000418, 0x00000300)] = 0x040D;
      m_composeMap[make_pair(0x00000418, 0x00000304)] = 0x04E2;
      m_composeMap[make_pair(0x00000418, 0x00000306)] = 0x0419;
      m_composeMap[make_pair(0x00000418, 0x00000308)] = 0x04E4;
      m_composeMap[make_pair(0x0000041A, 0x00000301)] = 0x040C;
      m_composeMap[make_pair(0x0000041E, 0x00000308)] = 0x04E6;
      m_composeMap[make_pair(0x00000423, 0x00000304)] = 0x04EE;
      m_composeMap[make_pair(0x00000423, 0x00000306)] = 0x040E;
      m_composeMap[make_pair(0x00000423, 0x00000308)] = 0x04F0;
      m_composeMap[make_pair(0x00000423, 0x0000030B)] = 0x04F2;
      m_composeMap[make_pair(0x00000427, 0x00000308)] = 0x04F4;
      m_composeMap[make_pair(0x0000042B, 0x00000308)] = 0x04F8;
      m_composeMap[make_pair(0x0000042D, 0x00000308)] = 0x04EC;
      m_composeMap[make_pair(0x00000430, 0x00000306)] = 0x04D1;
      m_composeMap[make_pair(0x00000430, 0x00000308)] = 0x04D3;
      m_composeMap[make_pair(0x00000433, 0x00000301)] = 0x0453;
      m_composeMap[make_pair(0x00000435, 0x00000300)] = 0x0450;
      m_composeMap[make_pair(0x00000435, 0x00000306)] = 0x04D7;
      m_composeMap[make_pair(0x00000435, 0x00000308)] = 0x0451;
      m_composeMap[make_pair(0x00000436, 0x00000306)] = 0x04C2;
      m_composeMap[make_pair(0x00000436, 0x00000308)] = 0x04DD;
      m_composeMap[make_pair(0x00000437, 0x00000308)] = 0x04DF;
      m_composeMap[make_pair(0x00000438, 0x00000300)] = 0x045D;
      m_composeMap[make_pair(0x00000438, 0x00000304)] = 0x04E3;
      m_composeMap[make_pair(0x00000438, 0x00000306)] = 0x0439;
      m_composeMap[make_pair(0x00000438, 0x00000308)] = 0x04E5;
      m_composeMap[make_pair(0x0000043A, 0x00000301)] = 0x045C;
      m_composeMap[make_pair(0x0000043E, 0x00000308)] = 0x04E7;
      m_composeMap[make_pair(0x00000443, 0x00000304)] = 0x04EF;
      m_composeMap[make_pair(0x00000443, 0x00000306)] = 0x045E;
      m_composeMap[make_pair(0x00000443, 0x00000308)] = 0x04F1;
      m_composeMap[make_pair(0x00000443, 0x0000030B)] = 0x04F3;
      m_composeMap[make_pair(0x00000447, 0x00000308)] = 0x04F5;
      m_composeMap[make_pair(0x0000044B, 0x00000308)] = 0x04F9;
      m_composeMap[make_pair(0x0000044D, 0x00000308)] = 0x04ED;
      m_composeMap[make_pair(0x00000456, 0x00000308)] = 0x0457;
      m_composeMap[make_pair(0x00000474, 0x0000030F)] = 0x0476;
      m_composeMap[make_pair(0x00000475, 0x0000030F)] = 0x0477;
      m_composeMap[make_pair(0x000004D8, 0x00000308)] = 0x04DA;
      m_composeMap[make_pair(0x000004D9, 0x00000308)] = 0x04DB;
      m_composeMap[make_pair(0x000004E8, 0x00000308)] = 0x04EA;
      m_composeMap[make_pair(0x000004E9, 0x00000308)] = 0x04EB;

    }


    ~Cyrillic400() {
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
      return "Cyrillic";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Cyrillic400::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Cyrillic400::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Cyrillic400::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Cyrillic400::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Cyrillic400::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Cyrillic400::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Cyrillic400::m_decompStr[uc - m_first_letter][0];
      us[1] = Cyrillic400::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Cyrillic400::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Cyrillic400::m_ea[uc - m_first_letter]);
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
    Cyrillic400(const Cyrillic400 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const UCS4 m_upper[256];
    static const UCS4 m_lower[256];
    static const UCS4 m_title[256];
    static const unsigned char _cat[256];
    static const unsigned char _comb_cl[256];
    static const unsigned char m_bidir[256];
    static const UCS2 m_decompStr[256][2];
    static const unsigned char m_lb[256];
    static const unsigned char m_ea[256];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<256> m_Diacritic;

  }; // class Cyrillic400

    const std::bitset<256> Cyrillic400::m_is_defined(std::string("0000001100111111111111111111111111111111111111110001100110011111111111111111111111111111111111111111111111111111111100110111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const UCS4 Cyrillic400::m_upper[] = {
    0x0400, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407, 
    0x0408, 0x0409, 0x040A, 0x040B, 0x040C, 0x040D, 0x040E, 0x040F, 
    0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417, 
    0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F, 
    0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427, 
    0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F, 
    0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417, 
    0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F, 
    0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427, 
    0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F, 
    0x0400, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407, 
    0x0408, 0x0409, 0x040A, 0x040B, 0x040C, 0x040D, 0x040E, 0x040F, 
    0x0460, 0x0460, 0x0462, 0x0462, 0x0464, 0x0464, 0x0466, 0x0466, 
    0x0468, 0x0468, 0x046A, 0x046A, 0x046C, 0x046C, 0x046E, 0x046E, 
    0x0470, 0x0470, 0x0472, 0x0472, 0x0474, 0x0474, 0x0476, 0x0476, 
    0x0478, 0x0478, 0x047A, 0x047A, 0x047C, 0x047C, 0x047E, 0x047E, 
    0x0480, 0x0480, 0x0482, 0x0483, 0x0484, 0x0485, 0x0486, 0x0487, 
    0x0488, 0x0489, 0x048A, 0x048B, 0x048C, 0x048C, 0x048E, 0x048E, 
    0x0490, 0x0490, 0x0492, 0x0492, 0x0494, 0x0494, 0x0496, 0x0496, 
    0x0498, 0x0498, 0x049A, 0x049A, 0x049C, 0x049C, 0x049E, 0x049E, 
    0x04A0, 0x04A0, 0x04A2, 0x04A2, 0x04A4, 0x04A4, 0x04A6, 0x04A6, 
    0x04A8, 0x04A8, 0x04AA, 0x04AA, 0x04AC, 0x04AC, 0x04AE, 0x04AE, 
    0x04B0, 0x04B0, 0x04B2, 0x04B2, 0x04B4, 0x04B4, 0x04B6, 0x04B6, 
    0x04B8, 0x04B8, 0x04BA, 0x04BA, 0x04BC, 0x04BC, 0x04BE, 0x04BE, 
    0x04C0, 0x04C1, 0x04C1, 0x04C3, 0x04C3, 0x04C5, 0x04C6, 0x04C7, 
    0x04C7, 0x04C9, 0x04CA, 0x04CB, 0x04CB, 0x04CD, 0x04CE, 0x04CF, 
    0x04D0, 0x04D0, 0x04D2, 0x04D2, 0x04D4, 0x04D4, 0x04D6, 0x04D6, 
    0x04D8, 0x04D8, 0x04DA, 0x04DA, 0x04DC, 0x04DC, 0x04DE, 0x04DE, 
    0x04E0, 0x04E0, 0x04E2, 0x04E2, 0x04E4, 0x04E4, 0x04E6, 0x04E6, 
    0x04E8, 0x04E8, 0x04EA, 0x04EA, 0x04EC, 0x04EC, 0x04EE, 0x04EE, 
    0x04F0, 0x04F0, 0x04F2, 0x04F2, 0x04F4, 0x04F4, 0x04F6, 0x04F7, 
    0x04F8, 0x04F8, 0x04FA, 0x04FB, 0x04FC, 0x04FD, 0x04FE, 0x04FF
  };

  const UCS4 Cyrillic400::m_lower[] = {
    0x0450, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457, 
    0x0458, 0x0459, 0x045A, 0x045B, 0x045C, 0x045D, 0x045E, 0x045F, 
    0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437, 
    0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F, 
    0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447, 
    0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F, 
    0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437, 
    0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F, 
    0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447, 
    0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F, 
    0x0450, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457, 
    0x0458, 0x0459, 0x045A, 0x045B, 0x045C, 0x045D, 0x045E, 0x045F, 
    0x0461, 0x0461, 0x0463, 0x0463, 0x0465, 0x0465, 0x0467, 0x0467, 
    0x0469, 0x0469, 0x046B, 0x046B, 0x046D, 0x046D, 0x046F, 0x046F, 
    0x0471, 0x0471, 0x0473, 0x0473, 0x0475, 0x0475, 0x0477, 0x0477, 
    0x0479, 0x0479, 0x047B, 0x047B, 0x047D, 0x047D, 0x047F, 0x047F, 
    0x0481, 0x0481, 0x0482, 0x0483, 0x0484, 0x0485, 0x0486, 0x0487, 
    0x0488, 0x0489, 0x048A, 0x048B, 0x048D, 0x048D, 0x048F, 0x048F, 
    0x0491, 0x0491, 0x0493, 0x0493, 0x0495, 0x0495, 0x0497, 0x0497, 
    0x0499, 0x0499, 0x049B, 0x049B, 0x049D, 0x049D, 0x049F, 0x049F, 
    0x04A1, 0x04A1, 0x04A3, 0x04A3, 0x04A5, 0x04A5, 0x04A7, 0x04A7, 
    0x04A9, 0x04A9, 0x04AB, 0x04AB, 0x04AD, 0x04AD, 0x04AF, 0x04AF, 
    0x04B1, 0x04B1, 0x04B3, 0x04B3, 0x04B5, 0x04B5, 0x04B7, 0x04B7, 
    0x04B9, 0x04B9, 0x04BB, 0x04BB, 0x04BD, 0x04BD, 0x04BF, 0x04BF, 
    0x04C0, 0x04C2, 0x04C2, 0x04C4, 0x04C4, 0x04C5, 0x04C6, 0x04C8, 
    0x04C8, 0x04C9, 0x04CA, 0x04CC, 0x04CC, 0x04CD, 0x04CE, 0x04CF, 
    0x04D1, 0x04D1, 0x04D3, 0x04D3, 0x04D5, 0x04D5, 0x04D7, 0x04D7, 
    0x04D9, 0x04D9, 0x04DB, 0x04DB, 0x04DD, 0x04DD, 0x04DF, 0x04DF, 
    0x04E1, 0x04E1, 0x04E3, 0x04E3, 0x04E5, 0x04E5, 0x04E7, 0x04E7, 
    0x04E9, 0x04E9, 0x04EB, 0x04EB, 0x04ED, 0x04ED, 0x04EF, 0x04EF, 
    0x04F1, 0x04F1, 0x04F3, 0x04F3, 0x04F5, 0x04F5, 0x04F6, 0x04F7, 
    0x04F9, 0x04F9, 0x04FA, 0x04FB, 0x04FC, 0x04FD, 0x04FE, 0x04FF
  };

  const UCS4 Cyrillic400::m_title[] = {
    0x0400, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407, 
    0x0408, 0x0409, 0x040A, 0x040B, 0x040C, 0x040D, 0x040E, 0x040F, 
    0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417, 
    0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F, 
    0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427, 
    0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F, 
    0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417, 
    0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F, 
    0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427, 
    0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F, 
    0x0400, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407, 
    0x0408, 0x0409, 0x040A, 0x040B, 0x040C, 0x040D, 0x040E, 0x040F, 
    0x0460, 0x0460, 0x0462, 0x0462, 0x0464, 0x0464, 0x0466, 0x0466, 
    0x0468, 0x0468, 0x046A, 0x046A, 0x046C, 0x046C, 0x046E, 0x046E, 
    0x0470, 0x0470, 0x0472, 0x0472, 0x0474, 0x0474, 0x0476, 0x0476, 
    0x0478, 0x0478, 0x047A, 0x047A, 0x047C, 0x047C, 0x047E, 0x047E, 
    0x0480, 0x0480, 0x0482, 0x0483, 0x0484, 0x0485, 0x0486, 0x0487, 
    0x0488, 0x0489, 0x048A, 0x048B, 0x048C, 0x048C, 0x048E, 0x048E, 
    0x0490, 0x0490, 0x0492, 0x0492, 0x0494, 0x0494, 0x0496, 0x0496, 
    0x0498, 0x0498, 0x049A, 0x049A, 0x049C, 0x049C, 0x049E, 0x049E, 
    0x04A0, 0x04A0, 0x04A2, 0x04A2, 0x04A4, 0x04A4, 0x04A6, 0x04A6, 
    0x04A8, 0x04A8, 0x04AA, 0x04AA, 0x04AC, 0x04AC, 0x04AE, 0x04AE, 
    0x04B0, 0x04B0, 0x04B2, 0x04B2, 0x04B4, 0x04B4, 0x04B6, 0x04B6, 
    0x04B8, 0x04B8, 0x04BA, 0x04BA, 0x04BC, 0x04BC, 0x04BE, 0x04BE, 
    0x04C0, 0x04C1, 0x04C1, 0x04C3, 0x04C3, 0x04C5, 0x04C6, 0x04C7, 
    0x04C7, 0x04C9, 0x04CA, 0x04CB, 0x04CB, 0x04CD, 0x04CE, 0x04CF, 
    0x04D0, 0x04D0, 0x04D2, 0x04D2, 0x04D4, 0x04D4, 0x04D6, 0x04D6, 
    0x04D8, 0x04D8, 0x04DA, 0x04DA, 0x04DC, 0x04DC, 0x04DE, 0x04DE, 
    0x04E0, 0x04E0, 0x04E2, 0x04E2, 0x04E4, 0x04E4, 0x04E6, 0x04E6, 
    0x04E8, 0x04E8, 0x04EA, 0x04EA, 0x04EC, 0x04EC, 0x04EE, 0x04EE, 
    0x04F0, 0x04F0, 0x04F2, 0x04F2, 0x04F4, 0x04F4, 0x04F6, 0x04F7, 
    0x04F8, 0x04F8, 0x04FA, 0x04FB, 0x04FC, 0x04FD, 0x04FE, 0x04FF
  };

  const unsigned char Cyrillic400::_cat[] = {
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_So, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lu, 
    CAT_Me, CAT_Me, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu
  };

  const unsigned char Cyrillic400::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 230, 230, 230, 230, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Cyrillic400::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS2 Cyrillic400::m_decompStr[][2] = {
    { 0x0415u, 0x0300u }, { 0x0415u, 0x0308u }, { 0x0402u, 0x0000u }, { 0x0413u, 0x0301u }, 
    { 0x0404u, 0x0000u }, { 0x0405u, 0x0000u }, { 0x0406u, 0x0000u }, { 0x0406u, 0x0308u }, 
    { 0x0408u, 0x0000u }, { 0x0409u, 0x0000u }, { 0x040Au, 0x0000u }, { 0x040Bu, 0x0000u }, 
    { 0x041Au, 0x0301u }, { 0x0418u, 0x0300u }, { 0x0423u, 0x0306u }, { 0x040Fu, 0x0000u }, 
    { 0x0410u, 0x0000u }, { 0x0411u, 0x0000u }, { 0x0412u, 0x0000u }, { 0x0413u, 0x0000u }, 
    { 0x0414u, 0x0000u }, { 0x0415u, 0x0000u }, { 0x0416u, 0x0000u }, { 0x0417u, 0x0000u }, 
    { 0x0418u, 0x0000u }, { 0x0418u, 0x0306u }, { 0x041Au, 0x0000u }, { 0x041Bu, 0x0000u }, 
    { 0x041Cu, 0x0000u }, { 0x041Du, 0x0000u }, { 0x041Eu, 0x0000u }, { 0x041Fu, 0x0000u }, 
    { 0x0420u, 0x0000u }, { 0x0421u, 0x0000u }, { 0x0422u, 0x0000u }, { 0x0423u, 0x0000u }, 
    { 0x0424u, 0x0000u }, { 0x0425u, 0x0000u }, { 0x0426u, 0x0000u }, { 0x0427u, 0x0000u }, 
    { 0x0428u, 0x0000u }, { 0x0429u, 0x0000u }, { 0x042Au, 0x0000u }, { 0x042Bu, 0x0000u }, 
    { 0x042Cu, 0x0000u }, { 0x042Du, 0x0000u }, { 0x042Eu, 0x0000u }, { 0x042Fu, 0x0000u }, 
    { 0x0430u, 0x0000u }, { 0x0431u, 0x0000u }, { 0x0432u, 0x0000u }, { 0x0433u, 0x0000u }, 
    { 0x0434u, 0x0000u }, { 0x0435u, 0x0000u }, { 0x0436u, 0x0000u }, { 0x0437u, 0x0000u }, 
    { 0x0438u, 0x0000u }, { 0x0438u, 0x0306u }, { 0x043Au, 0x0000u }, { 0x043Bu, 0x0000u }, 
    { 0x043Cu, 0x0000u }, { 0x043Du, 0x0000u }, { 0x043Eu, 0x0000u }, { 0x043Fu, 0x0000u }, 
    { 0x0440u, 0x0000u }, { 0x0441u, 0x0000u }, { 0x0442u, 0x0000u }, { 0x0443u, 0x0000u }, 
    { 0x0444u, 0x0000u }, { 0x0445u, 0x0000u }, { 0x0446u, 0x0000u }, { 0x0447u, 0x0000u }, 
    { 0x0448u, 0x0000u }, { 0x0449u, 0x0000u }, { 0x044Au, 0x0000u }, { 0x044Bu, 0x0000u }, 
    { 0x044Cu, 0x0000u }, { 0x044Du, 0x0000u }, { 0x044Eu, 0x0000u }, { 0x044Fu, 0x0000u }, 
    { 0x0435u, 0x0300u }, { 0x0435u, 0x0308u }, { 0x0452u, 0x0000u }, { 0x0433u, 0x0301u }, 
    { 0x0454u, 0x0000u }, { 0x0455u, 0x0000u }, { 0x0456u, 0x0000u }, { 0x0456u, 0x0308u }, 
    { 0x0458u, 0x0000u }, { 0x0459u, 0x0000u }, { 0x045Au, 0x0000u }, { 0x045Bu, 0x0000u }, 
    { 0x043Au, 0x0301u }, { 0x0438u, 0x0300u }, { 0x0443u, 0x0306u }, { 0x045Fu, 0x0000u }, 
    { 0x0460u, 0x0000u }, { 0x0461u, 0x0000u }, { 0x0462u, 0x0000u }, { 0x0463u, 0x0000u }, 
    { 0x0464u, 0x0000u }, { 0x0465u, 0x0000u }, { 0x0466u, 0x0000u }, { 0x0467u, 0x0000u }, 
    { 0x0468u, 0x0000u }, { 0x0469u, 0x0000u }, { 0x046Au, 0x0000u }, { 0x046Bu, 0x0000u }, 
    { 0x046Cu, 0x0000u }, { 0x046Du, 0x0000u }, { 0x046Eu, 0x0000u }, { 0x046Fu, 0x0000u }, 
    { 0x0470u, 0x0000u }, { 0x0471u, 0x0000u }, { 0x0472u, 0x0000u }, { 0x0473u, 0x0000u }, 
    { 0x0474u, 0x0000u }, { 0x0475u, 0x0000u }, { 0x0474u, 0x030Fu }, { 0x0475u, 0x030Fu }, 
    { 0x0478u, 0x0000u }, { 0x0479u, 0x0000u }, { 0x047Au, 0x0000u }, { 0x047Bu, 0x0000u }, 
    { 0x047Cu, 0x0000u }, { 0x047Du, 0x0000u }, { 0x047Eu, 0x0000u }, { 0x047Fu, 0x0000u }, 
    { 0x0480u, 0x0000u }, { 0x0481u, 0x0000u }, { 0x0482u, 0x0000u }, { 0x0483u, 0x0000u }, 
    { 0x0484u, 0x0000u }, { 0x0485u, 0x0000u }, { 0x0486u, 0x0000u }, { 0x0487u, 0x0000u }, 
    { 0x0488u, 0x0000u }, { 0x0489u, 0x0000u }, { 0x048Au, 0x0000u }, { 0x048Bu, 0x0000u }, 
    { 0x048Cu, 0x0000u }, { 0x048Du, 0x0000u }, { 0x048Eu, 0x0000u }, { 0x048Fu, 0x0000u }, 
    { 0x0490u, 0x0000u }, { 0x0491u, 0x0000u }, { 0x0492u, 0x0000u }, { 0x0493u, 0x0000u }, 
    { 0x0494u, 0x0000u }, { 0x0495u, 0x0000u }, { 0x0496u, 0x0000u }, { 0x0497u, 0x0000u }, 
    { 0x0498u, 0x0000u }, { 0x0499u, 0x0000u }, { 0x049Au, 0x0000u }, { 0x049Bu, 0x0000u }, 
    { 0x049Cu, 0x0000u }, { 0x049Du, 0x0000u }, { 0x049Eu, 0x0000u }, { 0x049Fu, 0x0000u }, 
    { 0x04A0u, 0x0000u }, { 0x04A1u, 0x0000u }, { 0x04A2u, 0x0000u }, { 0x04A3u, 0x0000u }, 
    { 0x04A4u, 0x0000u }, { 0x04A5u, 0x0000u }, { 0x04A6u, 0x0000u }, { 0x04A7u, 0x0000u }, 
    { 0x04A8u, 0x0000u }, { 0x04A9u, 0x0000u }, { 0x04AAu, 0x0000u }, { 0x04ABu, 0x0000u }, 
    { 0x04ACu, 0x0000u }, { 0x04ADu, 0x0000u }, { 0x04AEu, 0x0000u }, { 0x04AFu, 0x0000u }, 
    { 0x04B0u, 0x0000u }, { 0x04B1u, 0x0000u }, { 0x04B2u, 0x0000u }, { 0x04B3u, 0x0000u }, 
    { 0x04B4u, 0x0000u }, { 0x04B5u, 0x0000u }, { 0x04B6u, 0x0000u }, { 0x04B7u, 0x0000u }, 
    { 0x04B8u, 0x0000u }, { 0x04B9u, 0x0000u }, { 0x04BAu, 0x0000u }, { 0x04BBu, 0x0000u }, 
    { 0x04BCu, 0x0000u }, { 0x04BDu, 0x0000u }, { 0x04BEu, 0x0000u }, { 0x04BFu, 0x0000u }, 
    { 0x04C0u, 0x0000u }, { 0x0416u, 0x0306u }, { 0x0436u, 0x0306u }, { 0x04C3u, 0x0000u }, 
    { 0x04C4u, 0x0000u }, { 0x04C5u, 0x0000u }, { 0x04C6u, 0x0000u }, { 0x04C7u, 0x0000u }, 
    { 0x04C8u, 0x0000u }, { 0x04C9u, 0x0000u }, { 0x04CAu, 0x0000u }, { 0x04CBu, 0x0000u }, 
    { 0x04CCu, 0x0000u }, { 0x04CDu, 0x0000u }, { 0x04CEu, 0x0000u }, { 0x04CFu, 0x0000u }, 
    { 0x0410u, 0x0306u }, { 0x0430u, 0x0306u }, { 0x0410u, 0x0308u }, { 0x0430u, 0x0308u }, 
    { 0x04D4u, 0x0000u }, { 0x04D5u, 0x0000u }, { 0x0415u, 0x0306u }, { 0x0435u, 0x0306u }, 
    { 0x04D8u, 0x0000u }, { 0x04D9u, 0x0000u }, { 0x04D8u, 0x0308u }, { 0x04D9u, 0x0308u }, 
    { 0x0416u, 0x0308u }, { 0x0436u, 0x0308u }, { 0x0417u, 0x0308u }, { 0x0437u, 0x0308u }, 
    { 0x04E0u, 0x0000u }, { 0x04E1u, 0x0000u }, { 0x0418u, 0x0304u }, { 0x0438u, 0x0304u }, 
    { 0x0418u, 0x0308u }, { 0x0438u, 0x0308u }, { 0x041Eu, 0x0308u }, { 0x043Eu, 0x0308u }, 
    { 0x04E8u, 0x0000u }, { 0x04E9u, 0x0000u }, { 0x04E8u, 0x0308u }, { 0x04E9u, 0x0308u }, 
    { 0x042Du, 0x0308u }, { 0x044Du, 0x0308u }, { 0x0423u, 0x0304u }, { 0x0443u, 0x0304u }, 
    { 0x0423u, 0x0308u }, { 0x0443u, 0x0308u }, { 0x0423u, 0x030Bu }, { 0x0443u, 0x030Bu }, 
    { 0x0427u, 0x0308u }, { 0x0447u, 0x0308u }, { 0x04F6u, 0x0000u }, { 0x04F7u, 0x0000u }, 
    { 0x042Bu, 0x0308u }, { 0x044Bu, 0x0308u }, { 0x04FAu, 0x0000u }, { 0x04FBu, 0x0000u }, 
    { 0x04FCu, 0x0000u }, { 0x04FDu, 0x0000u }, { 0x04FEu, 0x0000u }, { 0x04FFu, 0x0000u }
  };

  const unsigned char Cyrillic400::m_lb[] = {
    LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, 
    LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Cyrillic400::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

    const std::bitset<256> Cyrillic400::m_Diacritic(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Cyrillic400);
