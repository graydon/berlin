/*$Id: 1F00-1FFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:31 +0200.
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

  class Greek_Extended1F00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Greek_Extended1F00() {
      m_first_letter = 0x1F00;
      m_last_letter  = 0x1FFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00001F00, 0x00000300)] = 0x1F02;
      m_composeMap[make_pair(0x00001F00, 0x00000301)] = 0x1F04;
      m_composeMap[make_pair(0x00001F00, 0x00000342)] = 0x1F06;
      m_composeMap[make_pair(0x00001F00, 0x00000345)] = 0x1F80;
      m_composeMap[make_pair(0x00001F01, 0x00000300)] = 0x1F03;
      m_composeMap[make_pair(0x00001F01, 0x00000301)] = 0x1F05;
      m_composeMap[make_pair(0x00001F01, 0x00000342)] = 0x1F07;
      m_composeMap[make_pair(0x00001F01, 0x00000345)] = 0x1F81;
      m_composeMap[make_pair(0x00001F02, 0x00000345)] = 0x1F82;
      m_composeMap[make_pair(0x00001F03, 0x00000345)] = 0x1F83;
      m_composeMap[make_pair(0x00001F04, 0x00000345)] = 0x1F84;
      m_composeMap[make_pair(0x00001F05, 0x00000345)] = 0x1F85;
      m_composeMap[make_pair(0x00001F06, 0x00000345)] = 0x1F86;
      m_composeMap[make_pair(0x00001F07, 0x00000345)] = 0x1F87;
      m_composeMap[make_pair(0x00001F08, 0x00000300)] = 0x1F0A;
      m_composeMap[make_pair(0x00001F08, 0x00000301)] = 0x1F0C;
      m_composeMap[make_pair(0x00001F08, 0x00000342)] = 0x1F0E;
      m_composeMap[make_pair(0x00001F08, 0x00000345)] = 0x1F88;
      m_composeMap[make_pair(0x00001F09, 0x00000300)] = 0x1F0B;
      m_composeMap[make_pair(0x00001F09, 0x00000301)] = 0x1F0D;
      m_composeMap[make_pair(0x00001F09, 0x00000342)] = 0x1F0F;
      m_composeMap[make_pair(0x00001F09, 0x00000345)] = 0x1F89;
      m_composeMap[make_pair(0x00001F0A, 0x00000345)] = 0x1F8A;
      m_composeMap[make_pair(0x00001F0B, 0x00000345)] = 0x1F8B;
      m_composeMap[make_pair(0x00001F0C, 0x00000345)] = 0x1F8C;
      m_composeMap[make_pair(0x00001F0D, 0x00000345)] = 0x1F8D;
      m_composeMap[make_pair(0x00001F0E, 0x00000345)] = 0x1F8E;
      m_composeMap[make_pair(0x00001F0F, 0x00000345)] = 0x1F8F;
      m_composeMap[make_pair(0x00001F10, 0x00000300)] = 0x1F12;
      m_composeMap[make_pair(0x00001F10, 0x00000301)] = 0x1F14;
      m_composeMap[make_pair(0x00001F11, 0x00000300)] = 0x1F13;
      m_composeMap[make_pair(0x00001F11, 0x00000301)] = 0x1F15;
      m_composeMap[make_pair(0x00001F18, 0x00000300)] = 0x1F1A;
      m_composeMap[make_pair(0x00001F18, 0x00000301)] = 0x1F1C;
      m_composeMap[make_pair(0x00001F19, 0x00000300)] = 0x1F1B;
      m_composeMap[make_pair(0x00001F19, 0x00000301)] = 0x1F1D;
      m_composeMap[make_pair(0x00001F20, 0x00000300)] = 0x1F22;
      m_composeMap[make_pair(0x00001F20, 0x00000301)] = 0x1F24;
      m_composeMap[make_pair(0x00001F20, 0x00000342)] = 0x1F26;
      m_composeMap[make_pair(0x00001F20, 0x00000345)] = 0x1F90;
      m_composeMap[make_pair(0x00001F21, 0x00000300)] = 0x1F23;
      m_composeMap[make_pair(0x00001F21, 0x00000301)] = 0x1F25;
      m_composeMap[make_pair(0x00001F21, 0x00000342)] = 0x1F27;
      m_composeMap[make_pair(0x00001F21, 0x00000345)] = 0x1F91;
      m_composeMap[make_pair(0x00001F22, 0x00000345)] = 0x1F92;
      m_composeMap[make_pair(0x00001F23, 0x00000345)] = 0x1F93;
      m_composeMap[make_pair(0x00001F24, 0x00000345)] = 0x1F94;
      m_composeMap[make_pair(0x00001F25, 0x00000345)] = 0x1F95;
      m_composeMap[make_pair(0x00001F26, 0x00000345)] = 0x1F96;
      m_composeMap[make_pair(0x00001F27, 0x00000345)] = 0x1F97;
      m_composeMap[make_pair(0x00001F28, 0x00000300)] = 0x1F2A;
      m_composeMap[make_pair(0x00001F28, 0x00000301)] = 0x1F2C;
      m_composeMap[make_pair(0x00001F28, 0x00000342)] = 0x1F2E;
      m_composeMap[make_pair(0x00001F28, 0x00000345)] = 0x1F98;
      m_composeMap[make_pair(0x00001F29, 0x00000300)] = 0x1F2B;
      m_composeMap[make_pair(0x00001F29, 0x00000301)] = 0x1F2D;
      m_composeMap[make_pair(0x00001F29, 0x00000342)] = 0x1F2F;
      m_composeMap[make_pair(0x00001F29, 0x00000345)] = 0x1F99;
      m_composeMap[make_pair(0x00001F2A, 0x00000345)] = 0x1F9A;
      m_composeMap[make_pair(0x00001F2B, 0x00000345)] = 0x1F9B;
      m_composeMap[make_pair(0x00001F2C, 0x00000345)] = 0x1F9C;
      m_composeMap[make_pair(0x00001F2D, 0x00000345)] = 0x1F9D;
      m_composeMap[make_pair(0x00001F2E, 0x00000345)] = 0x1F9E;
      m_composeMap[make_pair(0x00001F2F, 0x00000345)] = 0x1F9F;
      m_composeMap[make_pair(0x00001F30, 0x00000300)] = 0x1F32;
      m_composeMap[make_pair(0x00001F30, 0x00000301)] = 0x1F34;
      m_composeMap[make_pair(0x00001F30, 0x00000342)] = 0x1F36;
      m_composeMap[make_pair(0x00001F31, 0x00000300)] = 0x1F33;
      m_composeMap[make_pair(0x00001F31, 0x00000301)] = 0x1F35;
      m_composeMap[make_pair(0x00001F31, 0x00000342)] = 0x1F37;
      m_composeMap[make_pair(0x00001F38, 0x00000300)] = 0x1F3A;
      m_composeMap[make_pair(0x00001F38, 0x00000301)] = 0x1F3C;
      m_composeMap[make_pair(0x00001F38, 0x00000342)] = 0x1F3E;
      m_composeMap[make_pair(0x00001F39, 0x00000300)] = 0x1F3B;
      m_composeMap[make_pair(0x00001F39, 0x00000301)] = 0x1F3D;
      m_composeMap[make_pair(0x00001F39, 0x00000342)] = 0x1F3F;
      m_composeMap[make_pair(0x00001F40, 0x00000300)] = 0x1F42;
      m_composeMap[make_pair(0x00001F40, 0x00000301)] = 0x1F44;
      m_composeMap[make_pair(0x00001F41, 0x00000300)] = 0x1F43;
      m_composeMap[make_pair(0x00001F41, 0x00000301)] = 0x1F45;
      m_composeMap[make_pair(0x00001F48, 0x00000300)] = 0x1F4A;
      m_composeMap[make_pair(0x00001F48, 0x00000301)] = 0x1F4C;
      m_composeMap[make_pair(0x00001F49, 0x00000300)] = 0x1F4B;
      m_composeMap[make_pair(0x00001F49, 0x00000301)] = 0x1F4D;
      m_composeMap[make_pair(0x00001F50, 0x00000300)] = 0x1F52;
      m_composeMap[make_pair(0x00001F50, 0x00000301)] = 0x1F54;
      m_composeMap[make_pair(0x00001F50, 0x00000342)] = 0x1F56;
      m_composeMap[make_pair(0x00001F51, 0x00000300)] = 0x1F53;
      m_composeMap[make_pair(0x00001F51, 0x00000301)] = 0x1F55;
      m_composeMap[make_pair(0x00001F51, 0x00000342)] = 0x1F57;
      m_composeMap[make_pair(0x00001F59, 0x00000300)] = 0x1F5B;
      m_composeMap[make_pair(0x00001F59, 0x00000301)] = 0x1F5D;
      m_composeMap[make_pair(0x00001F59, 0x00000342)] = 0x1F5F;
      m_composeMap[make_pair(0x00001F60, 0x00000300)] = 0x1F62;
      m_composeMap[make_pair(0x00001F60, 0x00000301)] = 0x1F64;
      m_composeMap[make_pair(0x00001F60, 0x00000342)] = 0x1F66;
      m_composeMap[make_pair(0x00001F60, 0x00000345)] = 0x1FA0;
      m_composeMap[make_pair(0x00001F61, 0x00000300)] = 0x1F63;
      m_composeMap[make_pair(0x00001F61, 0x00000301)] = 0x1F65;
      m_composeMap[make_pair(0x00001F61, 0x00000342)] = 0x1F67;
      m_composeMap[make_pair(0x00001F61, 0x00000345)] = 0x1FA1;
      m_composeMap[make_pair(0x00001F62, 0x00000345)] = 0x1FA2;
      m_composeMap[make_pair(0x00001F63, 0x00000345)] = 0x1FA3;
      m_composeMap[make_pair(0x00001F64, 0x00000345)] = 0x1FA4;
      m_composeMap[make_pair(0x00001F65, 0x00000345)] = 0x1FA5;
      m_composeMap[make_pair(0x00001F66, 0x00000345)] = 0x1FA6;
      m_composeMap[make_pair(0x00001F67, 0x00000345)] = 0x1FA7;
      m_composeMap[make_pair(0x00001F68, 0x00000300)] = 0x1F6A;
      m_composeMap[make_pair(0x00001F68, 0x00000301)] = 0x1F6C;
      m_composeMap[make_pair(0x00001F68, 0x00000342)] = 0x1F6E;
      m_composeMap[make_pair(0x00001F68, 0x00000345)] = 0x1FA8;
      m_composeMap[make_pair(0x00001F69, 0x00000300)] = 0x1F6B;
      m_composeMap[make_pair(0x00001F69, 0x00000301)] = 0x1F6D;
      m_composeMap[make_pair(0x00001F69, 0x00000342)] = 0x1F6F;
      m_composeMap[make_pair(0x00001F69, 0x00000345)] = 0x1FA9;
      m_composeMap[make_pair(0x00001F6A, 0x00000345)] = 0x1FAA;
      m_composeMap[make_pair(0x00001F6B, 0x00000345)] = 0x1FAB;
      m_composeMap[make_pair(0x00001F6C, 0x00000345)] = 0x1FAC;
      m_composeMap[make_pair(0x00001F6D, 0x00000345)] = 0x1FAD;
      m_composeMap[make_pair(0x00001F6E, 0x00000345)] = 0x1FAE;
      m_composeMap[make_pair(0x00001F6F, 0x00000345)] = 0x1FAF;
      m_composeMap[make_pair(0x00001F70, 0x00000345)] = 0x1FB2;
      m_composeMap[make_pair(0x00001F74, 0x00000345)] = 0x1FC2;
      m_composeMap[make_pair(0x00001F7C, 0x00000345)] = 0x1FF2;
      m_composeMap[make_pair(0x00001FB6, 0x00000345)] = 0x1FB7;
      m_composeMap[make_pair(0x00001FBF, 0x00000300)] = 0x1FCD;
      m_composeMap[make_pair(0x00001FBF, 0x00000301)] = 0x1FCE;
      m_composeMap[make_pair(0x00001FBF, 0x00000342)] = 0x1FCF;
      m_composeMap[make_pair(0x00001FC6, 0x00000345)] = 0x1FC7;
      m_composeMap[make_pair(0x00001FF6, 0x00000345)] = 0x1FF7;
      m_composeMap[make_pair(0x00001FFE, 0x00000300)] = 0x1FDD;
      m_composeMap[make_pair(0x00001FFE, 0x00000301)] = 0x1FDE;
      m_composeMap[make_pair(0x00001FFE, 0x00000342)] = 0x1FDF;

    }


    ~Greek_Extended1F00() {
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
      return "Greek Extended";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Greek_Extended1F00::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Greek_Extended1F00::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Greek_Extended1F00::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Greek_Extended1F00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Greek_Extended1F00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Greek_Extended1F00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Greek_Extended1F00::m_decompStr[uc - m_first_letter][0];
      us[1] = Greek_Extended1F00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(LB_AL);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_N);
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
    Greek_Extended1F00(const Greek_Extended1F00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const UCS4 m_upper[256];
    static const UCS4 m_lower[256];
    static const UCS4 m_title[256];
    static const unsigned char _cat[256];
    static const unsigned char m_bidir[256];
    static const unsigned char _decomp[256];
    static const UCS2 m_decompStr[256][2];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<256> m_Diacritic;

  }; // class Greek_Extended1F00

    const std::bitset<256> Greek_Extended1F00::m_is_defined(std::string("0111111111011100111111111111111111101111110011111111111111011111111111111101111111111111111111111111111111111111111111111111111100111111111111111111111111111111101010101111111100111111001111111111111111111111111111111111111100111111001111111111111111111111"));

  const UCS4 Greek_Extended1F00::m_upper[] = {
    0x1F08, 0x1F09, 0x1F0A, 0x1F0B, 0x1F0C, 0x1F0D, 0x1F0E, 0x1F0F, 
    0x1F08, 0x1F09, 0x1F0A, 0x1F0B, 0x1F0C, 0x1F0D, 0x1F0E, 0x1F0F, 
    0x1F18, 0x1F19, 0x1F1A, 0x1F1B, 0x1F1C, 0x1F1D, 0x1F16, 0x1F17, 
    0x1F18, 0x1F19, 0x1F1A, 0x1F1B, 0x1F1C, 0x1F1D, 0x1F1E, 0x1F1F, 
    0x1F28, 0x1F29, 0x1F2A, 0x1F2B, 0x1F2C, 0x1F2D, 0x1F2E, 0x1F2F, 
    0x1F28, 0x1F29, 0x1F2A, 0x1F2B, 0x1F2C, 0x1F2D, 0x1F2E, 0x1F2F, 
    0x1F38, 0x1F39, 0x1F3A, 0x1F3B, 0x1F3C, 0x1F3D, 0x1F3E, 0x1F3F, 
    0x1F38, 0x1F39, 0x1F3A, 0x1F3B, 0x1F3C, 0x1F3D, 0x1F3E, 0x1F3F, 
    0x1F48, 0x1F49, 0x1F4A, 0x1F4B, 0x1F4C, 0x1F4D, 0x1F46, 0x1F47, 
    0x1F48, 0x1F49, 0x1F4A, 0x1F4B, 0x1F4C, 0x1F4D, 0x1F4E, 0x1F4F, 
    0x1F50, 0x1F59, 0x1F52, 0x1F5B, 0x1F54, 0x1F5D, 0x1F56, 0x1F5F, 
    0x1F58, 0x1F59, 0x1F5A, 0x1F5B, 0x1F5C, 0x1F5D, 0x1F5E, 0x1F5F, 
    0x1F68, 0x1F69, 0x1F6A, 0x1F6B, 0x1F6C, 0x1F6D, 0x1F6E, 0x1F6F, 
    0x1F68, 0x1F69, 0x1F6A, 0x1F6B, 0x1F6C, 0x1F6D, 0x1F6E, 0x1F6F, 
    0x1FBA, 0x1FBB, 0x1FC8, 0x1FC9, 0x1FCA, 0x1FCB, 0x1FDA, 0x1FDB, 
    0x1FF8, 0x1FF9, 0x1FEA, 0x1FEB, 0x1FFA, 0x1FFB, 0x1F7E, 0x1F7F, 
    0x1F88, 0x1F89, 0x1F8A, 0x1F8B, 0x1F8C, 0x1F8D, 0x1F8E, 0x1F8F, 
    0x1F88, 0x1F89, 0x1F8A, 0x1F8B, 0x1F8C, 0x1F8D, 0x1F8E, 0x1F8F, 
    0x1F98, 0x1F99, 0x1F9A, 0x1F9B, 0x1F9C, 0x1F9D, 0x1F9E, 0x1F9F, 
    0x1F98, 0x1F99, 0x1F9A, 0x1F9B, 0x1F9C, 0x1F9D, 0x1F9E, 0x1F9F, 
    0x1FA8, 0x1FA9, 0x1FAA, 0x1FAB, 0x1FAC, 0x1FAD, 0x1FAE, 0x1FAF, 
    0x1FA8, 0x1FA9, 0x1FAA, 0x1FAB, 0x1FAC, 0x1FAD, 0x1FAE, 0x1FAF, 
    0x1FB8, 0x1FB9, 0x1FB2, 0x1FBC, 0x1FB4, 0x1FB5, 0x1FB6, 0x1FB7, 
    0x1FB8, 0x1FB9, 0x1FBA, 0x1FBB, 0x1FBC, 0x1FBD, 0x0399, 0x1FBF, 
    0x1FC0, 0x1FC1, 0x1FC2, 0x1FCC, 0x1FC4, 0x1FC5, 0x1FC6, 0x1FC7, 
    0x1FC8, 0x1FC9, 0x1FCA, 0x1FCB, 0x1FCC, 0x1FCD, 0x1FCE, 0x1FCF, 
    0x1FD8, 0x1FD9, 0x1FD2, 0x1FD3, 0x1FD4, 0x1FD5, 0x1FD6, 0x1FD7, 
    0x1FD8, 0x1FD9, 0x1FDA, 0x1FDB, 0x1FDC, 0x1FDD, 0x1FDE, 0x1FDF, 
    0x1FE8, 0x1FE9, 0x1FE2, 0x1FE3, 0x1FE4, 0x1FEC, 0x1FE6, 0x1FE7, 
    0x1FE8, 0x1FE9, 0x1FEA, 0x1FEB, 0x1FEC, 0x1FED, 0x1FEE, 0x1FEF, 
    0x1FF0, 0x1FF1, 0x1FF2, 0x1FFC, 0x1FF4, 0x1FF5, 0x1FF6, 0x1FF7, 
    0x1FF8, 0x1FF9, 0x1FFA, 0x1FFB, 0x1FFC, 0x1FFD, 0x1FFE, 0x1FFF
  };

  const UCS4 Greek_Extended1F00::m_lower[] = {
    0x1F00, 0x1F01, 0x1F02, 0x1F03, 0x1F04, 0x1F05, 0x1F06, 0x1F07, 
    0x1F00, 0x1F01, 0x1F02, 0x1F03, 0x1F04, 0x1F05, 0x1F06, 0x1F07, 
    0x1F10, 0x1F11, 0x1F12, 0x1F13, 0x1F14, 0x1F15, 0x1F16, 0x1F17, 
    0x1F10, 0x1F11, 0x1F12, 0x1F13, 0x1F14, 0x1F15, 0x1F1E, 0x1F1F, 
    0x1F20, 0x1F21, 0x1F22, 0x1F23, 0x1F24, 0x1F25, 0x1F26, 0x1F27, 
    0x1F20, 0x1F21, 0x1F22, 0x1F23, 0x1F24, 0x1F25, 0x1F26, 0x1F27, 
    0x1F30, 0x1F31, 0x1F32, 0x1F33, 0x1F34, 0x1F35, 0x1F36, 0x1F37, 
    0x1F30, 0x1F31, 0x1F32, 0x1F33, 0x1F34, 0x1F35, 0x1F36, 0x1F37, 
    0x1F40, 0x1F41, 0x1F42, 0x1F43, 0x1F44, 0x1F45, 0x1F46, 0x1F47, 
    0x1F40, 0x1F41, 0x1F42, 0x1F43, 0x1F44, 0x1F45, 0x1F4E, 0x1F4F, 
    0x1F50, 0x1F51, 0x1F52, 0x1F53, 0x1F54, 0x1F55, 0x1F56, 0x1F57, 
    0x1F58, 0x1F51, 0x1F5A, 0x1F53, 0x1F5C, 0x1F55, 0x1F5E, 0x1F57, 
    0x1F60, 0x1F61, 0x1F62, 0x1F63, 0x1F64, 0x1F65, 0x1F66, 0x1F67, 
    0x1F60, 0x1F61, 0x1F62, 0x1F63, 0x1F64, 0x1F65, 0x1F66, 0x1F67, 
    0x1F70, 0x1F71, 0x1F72, 0x1F73, 0x1F74, 0x1F75, 0x1F76, 0x1F77, 
    0x1F78, 0x1F79, 0x1F7A, 0x1F7B, 0x1F7C, 0x1F7D, 0x1F7E, 0x1F7F, 
    0x1F80, 0x1F81, 0x1F82, 0x1F83, 0x1F84, 0x1F85, 0x1F86, 0x1F87, 
    0x1F80, 0x1F81, 0x1F82, 0x1F83, 0x1F84, 0x1F85, 0x1F86, 0x1F87, 
    0x1F90, 0x1F91, 0x1F92, 0x1F93, 0x1F94, 0x1F95, 0x1F96, 0x1F97, 
    0x1F90, 0x1F91, 0x1F92, 0x1F93, 0x1F94, 0x1F95, 0x1F96, 0x1F97, 
    0x1FA0, 0x1FA1, 0x1FA2, 0x1FA3, 0x1FA4, 0x1FA5, 0x1FA6, 0x1FA7, 
    0x1FA0, 0x1FA1, 0x1FA2, 0x1FA3, 0x1FA4, 0x1FA5, 0x1FA6, 0x1FA7, 
    0x1FB0, 0x1FB1, 0x1FB2, 0x1FB3, 0x1FB4, 0x1FB5, 0x1FB6, 0x1FB7, 
    0x1FB0, 0x1FB1, 0x1F70, 0x1F71, 0x1FB3, 0x1FBD, 0x1FBE, 0x1FBF, 
    0x1FC0, 0x1FC1, 0x1FC2, 0x1FC3, 0x1FC4, 0x1FC5, 0x1FC6, 0x1FC7, 
    0x1F72, 0x1F73, 0x1F74, 0x1F75, 0x1FC3, 0x1FCD, 0x1FCE, 0x1FCF, 
    0x1FD0, 0x1FD1, 0x1FD2, 0x1FD3, 0x1FD4, 0x1FD5, 0x1FD6, 0x1FD7, 
    0x1FD0, 0x1FD1, 0x1F76, 0x1F77, 0x1FDC, 0x1FDD, 0x1FDE, 0x1FDF, 
    0x1FE0, 0x1FE1, 0x1FE2, 0x1FE3, 0x1FE4, 0x1FE5, 0x1FE6, 0x1FE7, 
    0x1FE0, 0x1FE1, 0x1F7A, 0x1F7B, 0x1FE5, 0x1FED, 0x1FEE, 0x1FEF, 
    0x1FF0, 0x1FF1, 0x1FF2, 0x1FF3, 0x1FF4, 0x1FF5, 0x1FF6, 0x1FF7, 
    0x1F78, 0x1F79, 0x1F7C, 0x1F7D, 0x1FF3, 0x1FFD, 0x1FFE, 0x1FFF
  };

  const UCS4 Greek_Extended1F00::m_title[] = {
    0x1F08, 0x1F09, 0x1F0A, 0x1F0B, 0x1F0C, 0x1F0D, 0x1F0E, 0x1F0F, 
    0x1F08, 0x1F09, 0x1F0A, 0x1F0B, 0x1F0C, 0x1F0D, 0x1F0E, 0x1F0F, 
    0x1F18, 0x1F19, 0x1F1A, 0x1F1B, 0x1F1C, 0x1F1D, 0x1F16, 0x1F17, 
    0x1F18, 0x1F19, 0x1F1A, 0x1F1B, 0x1F1C, 0x1F1D, 0x1F1E, 0x1F1F, 
    0x1F28, 0x1F29, 0x1F2A, 0x1F2B, 0x1F2C, 0x1F2D, 0x1F2E, 0x1F2F, 
    0x1F28, 0x1F29, 0x1F2A, 0x1F2B, 0x1F2C, 0x1F2D, 0x1F2E, 0x1F2F, 
    0x1F38, 0x1F39, 0x1F3A, 0x1F3B, 0x1F3C, 0x1F3D, 0x1F3E, 0x1F3F, 
    0x1F38, 0x1F39, 0x1F3A, 0x1F3B, 0x1F3C, 0x1F3D, 0x1F3E, 0x1F3F, 
    0x1F48, 0x1F49, 0x1F4A, 0x1F4B, 0x1F4C, 0x1F4D, 0x1F46, 0x1F47, 
    0x1F48, 0x1F49, 0x1F4A, 0x1F4B, 0x1F4C, 0x1F4D, 0x1F4E, 0x1F4F, 
    0x1F50, 0x1F59, 0x1F52, 0x1F5B, 0x1F54, 0x1F5D, 0x1F56, 0x1F5F, 
    0x1F58, 0x1F59, 0x1F5A, 0x1F5B, 0x1F5C, 0x1F5D, 0x1F5E, 0x1F5F, 
    0x1F68, 0x1F69, 0x1F6A, 0x1F6B, 0x1F6C, 0x1F6D, 0x1F6E, 0x1F6F, 
    0x1F68, 0x1F69, 0x1F6A, 0x1F6B, 0x1F6C, 0x1F6D, 0x1F6E, 0x1F6F, 
    0x1FBA, 0x1FBB, 0x1FC8, 0x1FC9, 0x1FCA, 0x1FCB, 0x1FDA, 0x1FDB, 
    0x1FF8, 0x1FF9, 0x1FEA, 0x1FEB, 0x1FFA, 0x1FFB, 0x1F7E, 0x1F7F, 
    0x1F88, 0x1F89, 0x1F8A, 0x1F8B, 0x1F8C, 0x1F8D, 0x1F8E, 0x1F8F, 
    0x1F88, 0x1F89, 0x1F8A, 0x1F8B, 0x1F8C, 0x1F8D, 0x1F8E, 0x1F8F, 
    0x1F98, 0x1F99, 0x1F9A, 0x1F9B, 0x1F9C, 0x1F9D, 0x1F9E, 0x1F9F, 
    0x1F98, 0x1F99, 0x1F9A, 0x1F9B, 0x1F9C, 0x1F9D, 0x1F9E, 0x1F9F, 
    0x1FA8, 0x1FA9, 0x1FAA, 0x1FAB, 0x1FAC, 0x1FAD, 0x1FAE, 0x1FAF, 
    0x1FA8, 0x1FA9, 0x1FAA, 0x1FAB, 0x1FAC, 0x1FAD, 0x1FAE, 0x1FAF, 
    0x1FB8, 0x1FB9, 0x1FB2, 0x1FBC, 0x1FB4, 0x1FB5, 0x1FB6, 0x1FB7, 
    0x1FB8, 0x1FB9, 0x1FBA, 0x1FBB, 0x1FBC, 0x1FBD, 0x0399, 0x1FBF, 
    0x1FC0, 0x1FC1, 0x1FC2, 0x1FCC, 0x1FC4, 0x1FC5, 0x1FC6, 0x1FC7, 
    0x1FC8, 0x1FC9, 0x1FCA, 0x1FCB, 0x1FCC, 0x1FCD, 0x1FCE, 0x1FCF, 
    0x1FD8, 0x1FD9, 0x1FD2, 0x1FD3, 0x1FD4, 0x1FD5, 0x1FD6, 0x1FD7, 
    0x1FD8, 0x1FD9, 0x1FDA, 0x1FDB, 0x1FDC, 0x1FDD, 0x1FDE, 0x1FDF, 
    0x1FE8, 0x1FE9, 0x1FE2, 0x1FE3, 0x1FE4, 0x1FEC, 0x1FE6, 0x1FE7, 
    0x1FE8, 0x1FE9, 0x1FEA, 0x1FEB, 0x1FEC, 0x1FED, 0x1FEE, 0x1FEF, 
    0x1FF0, 0x1FF1, 0x1FF2, 0x1FFC, 0x1FF4, 0x1FF5, 0x1FF6, 0x1FF7, 
    0x1FF8, 0x1FF9, 0x1FFA, 0x1FFB, 0x1FFC, 0x1FFD, 0x1FFE, 0x1FFF
  };

  const unsigned char Greek_Extended1F00::_cat[] = {
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, CAT_Lt, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lt, CAT_Sk, CAT_Ll, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lt, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lt, CAT_Sk, CAT_Sk, CAT_Ll
  };

  const unsigned char Greek_Extended1F00::m_bidir[] = {
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
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_L
  };

  const unsigned char Greek_Extended1F00::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL
  };

  const UCS2 Greek_Extended1F00::m_decompStr[][2] = {
    { 0x03B1u, 0x0313u }, { 0x03B1u, 0x0314u }, { 0x1F00u, 0x0300u }, { 0x1F01u, 0x0300u }, 
    { 0x1F00u, 0x0301u }, { 0x1F01u, 0x0301u }, { 0x1F00u, 0x0342u }, { 0x1F01u, 0x0342u }, 
    { 0x0391u, 0x0313u }, { 0x0391u, 0x0314u }, { 0x1F08u, 0x0300u }, { 0x1F09u, 0x0300u }, 
    { 0x1F08u, 0x0301u }, { 0x1F09u, 0x0301u }, { 0x1F08u, 0x0342u }, { 0x1F09u, 0x0342u }, 
    { 0x03B5u, 0x0313u }, { 0x03B5u, 0x0314u }, { 0x1F10u, 0x0300u }, { 0x1F11u, 0x0300u }, 
    { 0x1F10u, 0x0301u }, { 0x1F11u, 0x0301u }, { 0x1F16u, 0x0000u }, { 0x1F17u, 0x0000u }, 
    { 0x0395u, 0x0313u }, { 0x0395u, 0x0314u }, { 0x1F18u, 0x0300u }, { 0x1F19u, 0x0300u }, 
    { 0x1F18u, 0x0301u }, { 0x1F19u, 0x0301u }, { 0x1F1Eu, 0x0000u }, { 0x1F1Fu, 0x0000u }, 
    { 0x03B7u, 0x0313u }, { 0x03B7u, 0x0314u }, { 0x1F20u, 0x0300u }, { 0x1F21u, 0x0300u }, 
    { 0x1F20u, 0x0301u }, { 0x1F21u, 0x0301u }, { 0x1F20u, 0x0342u }, { 0x1F21u, 0x0342u }, 
    { 0x0397u, 0x0313u }, { 0x0397u, 0x0314u }, { 0x1F28u, 0x0300u }, { 0x1F29u, 0x0300u }, 
    { 0x1F28u, 0x0301u }, { 0x1F29u, 0x0301u }, { 0x1F28u, 0x0342u }, { 0x1F29u, 0x0342u }, 
    { 0x03B9u, 0x0313u }, { 0x03B9u, 0x0314u }, { 0x1F30u, 0x0300u }, { 0x1F31u, 0x0300u }, 
    { 0x1F30u, 0x0301u }, { 0x1F31u, 0x0301u }, { 0x1F30u, 0x0342u }, { 0x1F31u, 0x0342u }, 
    { 0x0399u, 0x0313u }, { 0x0399u, 0x0314u }, { 0x1F38u, 0x0300u }, { 0x1F39u, 0x0300u }, 
    { 0x1F38u, 0x0301u }, { 0x1F39u, 0x0301u }, { 0x1F38u, 0x0342u }, { 0x1F39u, 0x0342u }, 
    { 0x03BFu, 0x0313u }, { 0x03BFu, 0x0314u }, { 0x1F40u, 0x0300u }, { 0x1F41u, 0x0300u }, 
    { 0x1F40u, 0x0301u }, { 0x1F41u, 0x0301u }, { 0x1F46u, 0x0000u }, { 0x1F47u, 0x0000u }, 
    { 0x039Fu, 0x0313u }, { 0x039Fu, 0x0314u }, { 0x1F48u, 0x0300u }, { 0x1F49u, 0x0300u }, 
    { 0x1F48u, 0x0301u }, { 0x1F49u, 0x0301u }, { 0x1F4Eu, 0x0000u }, { 0x1F4Fu, 0x0000u }, 
    { 0x03C5u, 0x0313u }, { 0x03C5u, 0x0314u }, { 0x1F50u, 0x0300u }, { 0x1F51u, 0x0300u }, 
    { 0x1F50u, 0x0301u }, { 0x1F51u, 0x0301u }, { 0x1F50u, 0x0342u }, { 0x1F51u, 0x0342u }, 
    { 0x1F58u, 0x0000u }, { 0x03A5u, 0x0314u }, { 0x1F5Au, 0x0000u }, { 0x1F59u, 0x0300u }, 
    { 0x1F5Cu, 0x0000u }, { 0x1F59u, 0x0301u }, { 0x1F5Eu, 0x0000u }, { 0x1F59u, 0x0342u }, 
    { 0x03C9u, 0x0313u }, { 0x03C9u, 0x0314u }, { 0x1F60u, 0x0300u }, { 0x1F61u, 0x0300u }, 
    { 0x1F60u, 0x0301u }, { 0x1F61u, 0x0301u }, { 0x1F60u, 0x0342u }, { 0x1F61u, 0x0342u }, 
    { 0x03A9u, 0x0313u }, { 0x03A9u, 0x0314u }, { 0x1F68u, 0x0300u }, { 0x1F69u, 0x0300u }, 
    { 0x1F68u, 0x0301u }, { 0x1F69u, 0x0301u }, { 0x1F68u, 0x0342u }, { 0x1F69u, 0x0342u }, 
    { 0x03B1u, 0x0300u }, { 0x03ACu, 0x0000u }, { 0x03B5u, 0x0300u }, { 0x03ADu, 0x0000u }, 
    { 0x03B7u, 0x0300u }, { 0x03AEu, 0x0000u }, { 0x03B9u, 0x0300u }, { 0x03AFu, 0x0000u }, 
    { 0x03BFu, 0x0300u }, { 0x03CCu, 0x0000u }, { 0x03C5u, 0x0300u }, { 0x03CDu, 0x0000u }, 
    { 0x03C9u, 0x0300u }, { 0x03CEu, 0x0000u }, { 0x1F7Eu, 0x0000u }, { 0x1F7Fu, 0x0000u }, 
    { 0x1F00u, 0x0345u }, { 0x1F01u, 0x0345u }, { 0x1F02u, 0x0345u }, { 0x1F03u, 0x0345u }, 
    { 0x1F04u, 0x0345u }, { 0x1F05u, 0x0345u }, { 0x1F06u, 0x0345u }, { 0x1F07u, 0x0345u }, 
    { 0x1F08u, 0x0345u }, { 0x1F09u, 0x0345u }, { 0x1F0Au, 0x0345u }, { 0x1F0Bu, 0x0345u }, 
    { 0x1F0Cu, 0x0345u }, { 0x1F0Du, 0x0345u }, { 0x1F0Eu, 0x0345u }, { 0x1F0Fu, 0x0345u }, 
    { 0x1F20u, 0x0345u }, { 0x1F21u, 0x0345u }, { 0x1F22u, 0x0345u }, { 0x1F23u, 0x0345u }, 
    { 0x1F24u, 0x0345u }, { 0x1F25u, 0x0345u }, { 0x1F26u, 0x0345u }, { 0x1F27u, 0x0345u }, 
    { 0x1F28u, 0x0345u }, { 0x1F29u, 0x0345u }, { 0x1F2Au, 0x0345u }, { 0x1F2Bu, 0x0345u }, 
    { 0x1F2Cu, 0x0345u }, { 0x1F2Du, 0x0345u }, { 0x1F2Eu, 0x0345u }, { 0x1F2Fu, 0x0345u }, 
    { 0x1F60u, 0x0345u }, { 0x1F61u, 0x0345u }, { 0x1F62u, 0x0345u }, { 0x1F63u, 0x0345u }, 
    { 0x1F64u, 0x0345u }, { 0x1F65u, 0x0345u }, { 0x1F66u, 0x0345u }, { 0x1F67u, 0x0345u }, 
    { 0x1F68u, 0x0345u }, { 0x1F69u, 0x0345u }, { 0x1F6Au, 0x0345u }, { 0x1F6Bu, 0x0345u }, 
    { 0x1F6Cu, 0x0345u }, { 0x1F6Du, 0x0345u }, { 0x1F6Eu, 0x0345u }, { 0x1F6Fu, 0x0345u }, 
    { 0x03B1u, 0x0306u }, { 0x03B1u, 0x0304u }, { 0x1F70u, 0x0345u }, { 0x03B1u, 0x0345u }, 
    { 0x03ACu, 0x0345u }, { 0x1FB5u, 0x0000u }, { 0x03B1u, 0x0342u }, { 0x1FB6u, 0x0345u }, 
    { 0x0391u, 0x0306u }, { 0x0391u, 0x0304u }, { 0x0391u, 0x0300u }, { 0x0386u, 0x0000u }, 
    { 0x0391u, 0x0345u }, { 0x0020u, 0x0313u }, { 0x03B9u, 0x0000u }, { 0x0020u, 0x0313u }, 
    { 0x0020u, 0x0342u }, { 0x00A8u, 0x0342u }, { 0x1F74u, 0x0345u }, { 0x03B7u, 0x0345u }, 
    { 0x03AEu, 0x0345u }, { 0x1FC5u, 0x0000u }, { 0x03B7u, 0x0342u }, { 0x1FC6u, 0x0345u }, 
    { 0x0395u, 0x0300u }, { 0x0388u, 0x0000u }, { 0x0397u, 0x0300u }, { 0x0389u, 0x0000u }, 
    { 0x0397u, 0x0345u }, { 0x1FBFu, 0x0300u }, { 0x1FBFu, 0x0301u }, { 0x1FBFu, 0x0342u }, 
    { 0x03B9u, 0x0306u }, { 0x03B9u, 0x0304u }, { 0x03CAu, 0x0300u }, { 0x0390u, 0x0000u }, 
    { 0x1FD4u, 0x0000u }, { 0x1FD5u, 0x0000u }, { 0x03B9u, 0x0342u }, { 0x03CAu, 0x0342u }, 
    { 0x0399u, 0x0306u }, { 0x0399u, 0x0304u }, { 0x0399u, 0x0300u }, { 0x038Au, 0x0000u }, 
    { 0x1FDCu, 0x0000u }, { 0x1FFEu, 0x0300u }, { 0x1FFEu, 0x0301u }, { 0x1FFEu, 0x0342u }, 
    { 0x03C5u, 0x0306u }, { 0x03C5u, 0x0304u }, { 0x03CBu, 0x0300u }, { 0x03B0u, 0x0000u }, 
    { 0x03C1u, 0x0313u }, { 0x03C1u, 0x0314u }, { 0x03C5u, 0x0342u }, { 0x03CBu, 0x0342u }, 
    { 0x03A5u, 0x0306u }, { 0x03A5u, 0x0304u }, { 0x03A5u, 0x0300u }, { 0x038Eu, 0x0000u }, 
    { 0x03A1u, 0x0314u }, { 0x00A8u, 0x0300u }, { 0x0385u, 0x0000u }, { 0x0060u, 0x0000u }, 
    { 0x1FF0u, 0x0000u }, { 0x1FF1u, 0x0000u }, { 0x1F7Cu, 0x0345u }, { 0x03C9u, 0x0345u }, 
    { 0x03CEu, 0x0345u }, { 0x1FF5u, 0x0000u }, { 0x03C9u, 0x0342u }, { 0x1FF6u, 0x0345u }, 
    { 0x039Fu, 0x0300u }, { 0x038Cu, 0x0000u }, { 0x03A9u, 0x0300u }, { 0x038Fu, 0x0000u }, 
    { 0x03A9u, 0x0345u }, { 0x00B4u, 0x0000u }, { 0x0020u, 0x0314u }, { 0x1FFFu, 0x0000u }
  };

    const std::bitset<256> Greek_Extended1F00::m_Diacritic(std::string("0110000000000000111000000000000011100000000000001110000000000011100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Greek_Extended1F00);
