/*$Id: FB00-FB4F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:04:47 +0200.
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

  class Alphabetic_Presentation_FormsFB00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Alphabetic_Presentation_FormsFB00() {
      m_first_letter = 0xFB00;
      m_last_letter  = 0xFB4F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x0000FB49, 0x000005C1)] = 0xFB2C;
      m_composeMap[make_pair(0x0000FB49, 0x000005C2)] = 0xFB2D;

    }


    ~Alphabetic_Presentation_FormsFB00() {
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
      return "Alphabetic Presentation Forms";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return uc;
    }

    UCS4 lowercase(const UCS4 uc) const {
      return uc;
    }

    UCS4 titlecase(const UCS4 uc) const {
      return uc;
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
      return Babylon::Gen_Cat(Alphabetic_Presentation_FormsFB00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Alphabetic_Presentation_FormsFB00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Alphabetic_Presentation_FormsFB00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Alphabetic_Presentation_FormsFB00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Alphabetic_Presentation_FormsFB00::m_decompStr[uc - m_first_letter][0];
      us[1] = Alphabetic_Presentation_FormsFB00::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0xFB03:
        us.resize(3);
        us[2u] = 0x0069u;
        break;

      case 0xFB04:
        us.resize(3);
        us[2u] = 0x006Cu;
        break;
      }
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
      return Babylon::Line_Break(Alphabetic_Presentation_FormsFB00::m_lb[uc - m_first_letter]);
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
    Alphabetic_Presentation_FormsFB00(const Alphabetic_Presentation_FormsFB00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<80> m_is_defined;
    static const unsigned char _cat[80];
    static const unsigned char _comb_cl[80];
    static const unsigned char m_bidir[80];
    static const unsigned char _decomp[80];
    static const UCS4 m_decompStr[80][2];
    static const unsigned char m_lb[80];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Alphabetic_Presentation_FormsFB00

    const std::bitset<80> Alphabetic_Presentation_FormsFB00::m_is_defined(std::string("11111111110110110101111101111111111111111111111111100000111110000000000001111111"));

  const unsigned char Alphabetic_Presentation_FormsFB00::_cat[] = {
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Lo, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Sm, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Ll, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Ll, CAT_Lo, CAT_Ll, 
    CAT_Lo, CAT_Lo, CAT_Ll, CAT_Lo, CAT_Lo, CAT_Ll, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo
  };

  const unsigned char Alphabetic_Presentation_FormsFB00::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 26, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Alphabetic_Presentation_FormsFB00::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_R, BIDIR_NSM, BIDIR_R, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, 
    BIDIR_R, BIDIR_ET, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_L, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_L, BIDIR_R, BIDIR_L, 
    BIDIR_R, BIDIR_R, BIDIR_L, BIDIR_R, BIDIR_R, BIDIR_L, BIDIR_R, BIDIR_R, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R
  };

  const unsigned char Alphabetic_Presentation_FormsFB00::_decomp[] = {
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, 
    DECOMP_FONT, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT
  };

  const UCS4 Alphabetic_Presentation_FormsFB00::m_decompStr[][2] = {
    { 0x0066u, 0x0066u }, { 0x0066u, 0x0069u }, { 0x0066u, 0x006Cu }, { 0x0066u, 0x0066u }, 
    { 0x0066u, 0x0066u }, { 0x017Fu, 0x0074u }, { 0x0073u, 0x0074u }, { 0xFB07u, 0x0000u }, 
    { 0xFB08u, 0x0000u }, { 0xFB09u, 0x0000u }, { 0xFB0Au, 0x0000u }, { 0xFB0Bu, 0x0000u }, 
    { 0xFB0Cu, 0x0000u }, { 0xFB0Du, 0x0000u }, { 0xFB0Eu, 0x0000u }, { 0xFB0Fu, 0x0000u }, 
    { 0xFB10u, 0x0000u }, { 0xFB11u, 0x0000u }, { 0xFB12u, 0x0000u }, { 0x0574u, 0x0576u }, 
    { 0x0574u, 0x0565u }, { 0x0574u, 0x056Bu }, { 0x057Eu, 0x0576u }, { 0x0574u, 0x056Du }, 
    { 0xFB18u, 0x0000u }, { 0xFB19u, 0x0000u }, { 0xFB1Au, 0x0000u }, { 0xFB1Bu, 0x0000u }, 
    { 0xFB1Cu, 0x0000u }, { 0x05D9u, 0x05B4u }, { 0xFB1Eu, 0x0000u }, { 0x05F2u, 0x05B7u }, 
    { 0x05E2u, 0x0000u }, { 0x05D0u, 0x0000u }, { 0x05D3u, 0x0000u }, { 0x05D4u, 0x0000u }, 
    { 0x05DBu, 0x0000u }, { 0x05DCu, 0x0000u }, { 0x05DDu, 0x0000u }, { 0x05E8u, 0x0000u }, 
    { 0x05EAu, 0x0000u }, { 0x002Bu, 0x0000u }, { 0x05E9u, 0x05C1u }, { 0x05E9u, 0x05C2u }, 
    { 0xFB49u, 0x05C1u }, { 0xFB49u, 0x05C2u }, { 0x05D0u, 0x05B7u }, { 0x05D0u, 0x05B8u }, 
    { 0x05D0u, 0x05BCu }, { 0x05D1u, 0x05BCu }, { 0x05D2u, 0x05BCu }, { 0x05D3u, 0x05BCu }, 
    { 0x05D4u, 0x05BCu }, { 0x05D5u, 0x05BCu }, { 0x05D6u, 0x05BCu }, { 0xFB37u, 0x0000u }, 
    { 0x05D8u, 0x05BCu }, { 0x05D9u, 0x05BCu }, { 0x05DAu, 0x05BCu }, { 0x05DBu, 0x05BCu }, 
    { 0x05DCu, 0x05BCu }, { 0xFB3Du, 0x0000u }, { 0x05DEu, 0x05BCu }, { 0xFB3Fu, 0x0000u }, 
    { 0x05E0u, 0x05BCu }, { 0x05E1u, 0x05BCu }, { 0xFB42u, 0x0000u }, { 0x05E3u, 0x05BCu }, 
    { 0x05E4u, 0x05BCu }, { 0xFB45u, 0x0000u }, { 0x05E6u, 0x05BCu }, { 0x05E7u, 0x05BCu }, 
    { 0x05E8u, 0x05BCu }, { 0x05E9u, 0x05BCu }, { 0x05EAu, 0x05BCu }, { 0x05D5u, 0x05B9u }, 
    { 0x05D1u, 0x05BFu }, { 0x05DBu, 0x05BFu }, { 0x05E4u, 0x05BFu }, { 0x05D0u, 0x05DCu }
  };

  const unsigned char Alphabetic_Presentation_FormsFB00::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

}; // namespace Babylon

dload(Babylon::Alphabetic_Presentation_FormsFB00);
