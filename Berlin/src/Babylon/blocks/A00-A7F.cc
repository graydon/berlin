/*$Id: A00-A7F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:51 +0200.
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

  class GurmukhiA00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    GurmukhiA00() {
      m_first_letter = 0xA00;
      m_last_letter  = 0xA7F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000A16, 0x00000A3C)] = 0x0A59;
      m_composeMap[make_pair(0x00000A17, 0x00000A3C)] = 0x0A5A;
      m_composeMap[make_pair(0x00000A1C, 0x00000A3C)] = 0x0A5B;
      m_composeMap[make_pair(0x00000A2B, 0x00000A3C)] = 0x0A5E;
      m_composeMap[make_pair(0x00000A32, 0x00000A3C)] = 0x0A33;
      m_composeMap[make_pair(0x00000A38, 0x00000A3C)] = 0x0A36;

    }


    ~GurmukhiA00() {
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
      return "Gurmukhi";
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
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0A66u:
        return 0;
        break;
      case 0x0A67u:
        return 1;
        break;
      case 0x0A68u:
        return 2;
        break;
      case 0x0A69u:
        return 3;
        break;
      case 0x0A6Au:
        return 4;
        break;
      case 0x0A6Bu:
        return 5;
        break;
      case 0x0A6Cu:
        return 6;
        break;
      case 0x0A6Du:
        return 7;
        break;
      case 0x0A6Eu:
        return 8;
        break;
      case 0x0A6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0A66u:
      case 0x0A67u:
      case 0x0A68u:
      case 0x0A69u:
      case 0x0A6Au:
      case 0x0A6Bu:
      case 0x0A6Cu:
      case 0x0A6Du:
      case 0x0A6Eu:
      case 0x0A6Fu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0A66u:
        return 0;
        break;
      case 0x0A67u:
        return 1;
        break;
      case 0x0A68u:
        return 2;
        break;
      case 0x0A69u:
        return 3;
        break;
      case 0x0A6Au:
        return 4;
        break;
      case 0x0A6Bu:
        return 5;
        break;
      case 0x0A6Cu:
        return 6;
        break;
      case 0x0A6Du:
        return 7;
        break;
      case 0x0A6Eu:
        return 8;
        break;
      case 0x0A6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0A66u:
      case 0x0A67u:
      case 0x0A68u:
      case 0x0A69u:
      case 0x0A6Au:
      case 0x0A6Bu:
      case 0x0A6Cu:
      case 0x0A6Du:
      case 0x0A6Eu:
      case 0x0A6Fu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0A66u:
        return 0.000000;
        break;
      case 0x0A67u:
        return 1.000000;
        break;
      case 0x0A68u:
        return 2.000000;
        break;
      case 0x0A69u:
        return 3.000000;
        break;
      case 0x0A6Au:
        return 4.000000;
        break;
      case 0x0A6Bu:
        return 5.000000;
        break;
      case 0x0A6Cu:
        return 6.000000;
        break;
      case 0x0A6Du:
        return 7.000000;
        break;
      case 0x0A6Eu:
        return 8.000000;
        break;
      case 0x0A6Fu:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0A66u:
      case 0x0A67u:
      case 0x0A68u:
      case 0x0A69u:
      case 0x0A6Au:
      case 0x0A6Bu:
      case 0x0A6Cu:
      case 0x0A6Du:
      case 0x0A6Eu:
      case 0x0A6Fu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(GurmukhiA00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(GurmukhiA00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(GurmukhiA00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = GurmukhiA00::m_decompStr[uc - m_first_letter][0];
      us[1] = GurmukhiA00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(GurmukhiA00::m_lb[uc - m_first_letter]);
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
      return m_Other_Alphabetic.test(uc - m_first_letter);
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
    GurmukhiA00(const GurmukhiA00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<128> m_is_defined;
    static const unsigned char _cat[128];
    static const unsigned char _comb_cl[128];
    static const unsigned char m_bidir[128];
    static const UCS2 m_decompStr[128][2];
    static const unsigned char m_lb[128];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<128> m_Other_Alphabetic;

  }; // class GurmukhiA00

    const std::bitset<128> GurmukhiA00::m_is_defined(std::string("00000000000111111111111111000000010111100000000000111001100001111101001101101101111111011111111111111111111110011000011111100100"));

  const unsigned char GurmukhiA00::_cat[] = {
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Lo, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn
  };

  const unsigned char GurmukhiA00::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 7, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 9, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char GurmukhiA00::m_bidir[] = {
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM
  };

  const UCS2 GurmukhiA00::m_decompStr[][2] = {
    { 0x0A00u, 0x0000u }, { 0x0A01u, 0x0000u }, { 0x0A02u, 0x0000u }, { 0x0A03u, 0x0000u }, 
    { 0x0A04u, 0x0000u }, { 0x0A05u, 0x0000u }, { 0x0A06u, 0x0000u }, { 0x0A07u, 0x0000u }, 
    { 0x0A08u, 0x0000u }, { 0x0A09u, 0x0000u }, { 0x0A0Au, 0x0000u }, { 0x0A0Bu, 0x0000u }, 
    { 0x0A0Cu, 0x0000u }, { 0x0A0Du, 0x0000u }, { 0x0A0Eu, 0x0000u }, { 0x0A0Fu, 0x0000u }, 
    { 0x0A10u, 0x0000u }, { 0x0A11u, 0x0000u }, { 0x0A12u, 0x0000u }, { 0x0A13u, 0x0000u }, 
    { 0x0A14u, 0x0000u }, { 0x0A15u, 0x0000u }, { 0x0A16u, 0x0000u }, { 0x0A17u, 0x0000u }, 
    { 0x0A18u, 0x0000u }, { 0x0A19u, 0x0000u }, { 0x0A1Au, 0x0000u }, { 0x0A1Bu, 0x0000u }, 
    { 0x0A1Cu, 0x0000u }, { 0x0A1Du, 0x0000u }, { 0x0A1Eu, 0x0000u }, { 0x0A1Fu, 0x0000u }, 
    { 0x0A20u, 0x0000u }, { 0x0A21u, 0x0000u }, { 0x0A22u, 0x0000u }, { 0x0A23u, 0x0000u }, 
    { 0x0A24u, 0x0000u }, { 0x0A25u, 0x0000u }, { 0x0A26u, 0x0000u }, { 0x0A27u, 0x0000u }, 
    { 0x0A28u, 0x0000u }, { 0x0A29u, 0x0000u }, { 0x0A2Au, 0x0000u }, { 0x0A2Bu, 0x0000u }, 
    { 0x0A2Cu, 0x0000u }, { 0x0A2Du, 0x0000u }, { 0x0A2Eu, 0x0000u }, { 0x0A2Fu, 0x0000u }, 
    { 0x0A30u, 0x0000u }, { 0x0A31u, 0x0000u }, { 0x0A32u, 0x0000u }, { 0x0A32u, 0x0A3Cu }, 
    { 0x0A34u, 0x0000u }, { 0x0A35u, 0x0000u }, { 0x0A38u, 0x0A3Cu }, { 0x0A37u, 0x0000u }, 
    { 0x0A38u, 0x0000u }, { 0x0A39u, 0x0000u }, { 0x0A3Au, 0x0000u }, { 0x0A3Bu, 0x0000u }, 
    { 0x0A3Cu, 0x0000u }, { 0x0A3Du, 0x0000u }, { 0x0A3Eu, 0x0000u }, { 0x0A3Fu, 0x0000u }, 
    { 0x0A40u, 0x0000u }, { 0x0A41u, 0x0000u }, { 0x0A42u, 0x0000u }, { 0x0A43u, 0x0000u }, 
    { 0x0A44u, 0x0000u }, { 0x0A45u, 0x0000u }, { 0x0A46u, 0x0000u }, { 0x0A47u, 0x0000u }, 
    { 0x0A48u, 0x0000u }, { 0x0A49u, 0x0000u }, { 0x0A4Au, 0x0000u }, { 0x0A4Bu, 0x0000u }, 
    { 0x0A4Cu, 0x0000u }, { 0x0A4Du, 0x0000u }, { 0x0A4Eu, 0x0000u }, { 0x0A4Fu, 0x0000u }, 
    { 0x0A50u, 0x0000u }, { 0x0A51u, 0x0000u }, { 0x0A52u, 0x0000u }, { 0x0A53u, 0x0000u }, 
    { 0x0A54u, 0x0000u }, { 0x0A55u, 0x0000u }, { 0x0A56u, 0x0000u }, { 0x0A57u, 0x0000u }, 
    { 0x0A58u, 0x0000u }, { 0x0A16u, 0x0A3Cu }, { 0x0A17u, 0x0A3Cu }, { 0x0A1Cu, 0x0A3Cu }, 
    { 0x0A5Cu, 0x0000u }, { 0x0A5Du, 0x0000u }, { 0x0A2Bu, 0x0A3Cu }, { 0x0A5Fu, 0x0000u }, 
    { 0x0A60u, 0x0000u }, { 0x0A61u, 0x0000u }, { 0x0A62u, 0x0000u }, { 0x0A63u, 0x0000u }, 
    { 0x0A64u, 0x0000u }, { 0x0A65u, 0x0000u }, { 0x0A66u, 0x0000u }, { 0x0A67u, 0x0000u }, 
    { 0x0A68u, 0x0000u }, { 0x0A69u, 0x0000u }, { 0x0A6Au, 0x0000u }, { 0x0A6Bu, 0x0000u }, 
    { 0x0A6Cu, 0x0000u }, { 0x0A6Du, 0x0000u }, { 0x0A6Eu, 0x0000u }, { 0x0A6Fu, 0x0000u }, 
    { 0x0A70u, 0x0000u }, { 0x0A71u, 0x0000u }, { 0x0A72u, 0x0000u }, { 0x0A73u, 0x0000u }, 
    { 0x0A74u, 0x0000u }, { 0x0A75u, 0x0000u }, { 0x0A76u, 0x0000u }, { 0x0A77u, 0x0000u }, 
    { 0x0A78u, 0x0000u }, { 0x0A79u, 0x0000u }, { 0x0A7Au, 0x0000u }, { 0x0A7Bu, 0x0000u }, 
    { 0x0A7Cu, 0x0000u }, { 0x0A7Du, 0x0000u }, { 0x0A7Eu, 0x0000u }, { 0x0A7Fu, 0x0000u }
  };

  const unsigned char GurmukhiA00::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, 
    LB_AL, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_CM, LB_AL, LB_AL, LB_CM, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> GurmukhiA00::m_Other_Alphabetic(std::string("00000000000000110000000000000000000000000000000000011001100001111100000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::GurmukhiA00);
