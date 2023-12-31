/*$Id: B00-B7F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:58 +0200.
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

  class OriyaB00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    OriyaB00() {
      m_first_letter = 0xB00;
      m_last_letter  = 0xB7F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000B21, 0x00000B3C)] = 0x0B5C;
      m_composeMap[make_pair(0x00000B22, 0x00000B3C)] = 0x0B5D;
      m_composeMap[make_pair(0x00000B47, 0x00000B3E)] = 0x0B4B;
      m_composeMap[make_pair(0x00000B47, 0x00000B56)] = 0x0B48;
      m_composeMap[make_pair(0x00000B47, 0x00000B57)] = 0x0B4C;

    }


    ~OriyaB00() {
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
      return "Oriya";
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
      case 0x0B66u:
        return 0;
        break;
      case 0x0B67u:
        return 1;
        break;
      case 0x0B68u:
        return 2;
        break;
      case 0x0B69u:
        return 3;
        break;
      case 0x0B6Au:
        return 4;
        break;
      case 0x0B6Bu:
        return 5;
        break;
      case 0x0B6Cu:
        return 6;
        break;
      case 0x0B6Du:
        return 7;
        break;
      case 0x0B6Eu:
        return 8;
        break;
      case 0x0B6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0B66u:
      case 0x0B67u:
      case 0x0B68u:
      case 0x0B69u:
      case 0x0B6Au:
      case 0x0B6Bu:
      case 0x0B6Cu:
      case 0x0B6Du:
      case 0x0B6Eu:
      case 0x0B6Fu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0B66u:
        return 0;
        break;
      case 0x0B67u:
        return 1;
        break;
      case 0x0B68u:
        return 2;
        break;
      case 0x0B69u:
        return 3;
        break;
      case 0x0B6Au:
        return 4;
        break;
      case 0x0B6Bu:
        return 5;
        break;
      case 0x0B6Cu:
        return 6;
        break;
      case 0x0B6Du:
        return 7;
        break;
      case 0x0B6Eu:
        return 8;
        break;
      case 0x0B6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0B66u:
      case 0x0B67u:
      case 0x0B68u:
      case 0x0B69u:
      case 0x0B6Au:
      case 0x0B6Bu:
      case 0x0B6Cu:
      case 0x0B6Du:
      case 0x0B6Eu:
      case 0x0B6Fu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0B66u:
        return 0.000000;
        break;
      case 0x0B67u:
        return 1.000000;
        break;
      case 0x0B68u:
        return 2.000000;
        break;
      case 0x0B69u:
        return 3.000000;
        break;
      case 0x0B6Au:
        return 4.000000;
        break;
      case 0x0B6Bu:
        return 5.000000;
        break;
      case 0x0B6Cu:
        return 6.000000;
        break;
      case 0x0B6Du:
        return 7.000000;
        break;
      case 0x0B6Eu:
        return 8.000000;
        break;
      case 0x0B6Fu:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0B66u:
      case 0x0B67u:
      case 0x0B68u:
      case 0x0B69u:
      case 0x0B6Au:
      case 0x0B6Bu:
      case 0x0B6Cu:
      case 0x0B6Du:
      case 0x0B6Eu:
      case 0x0B6Fu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(OriyaB00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(OriyaB00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(OriyaB00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = OriyaB00::m_decompStr[uc - m_first_letter][0];
      us[1] = OriyaB00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(OriyaB00::m_lb[uc - m_first_letter]);
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
    OriyaB00(const OriyaB00 &) {}

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

  }; // class OriyaB00

    const std::bitset<128> OriyaB00::m_is_defined(std::string("00000000000000011111111111000011101100001100000000111001100011111111001111001101111111011111111111111111111110011001111111101110"));

  const unsigned char OriyaB00::_cat[] = {
    CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Mc, CAT_Mn, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_So, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn
  };

  const unsigned char OriyaB00::_comb_cl[] = {
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

  const unsigned char OriyaB00::m_bidir[] = {
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM
  };

  const UCS2 OriyaB00::m_decompStr[][2] = {
    { 0x0B00u, 0x0000u }, { 0x0B01u, 0x0000u }, { 0x0B02u, 0x0000u }, { 0x0B03u, 0x0000u }, 
    { 0x0B04u, 0x0000u }, { 0x0B05u, 0x0000u }, { 0x0B06u, 0x0000u }, { 0x0B07u, 0x0000u }, 
    { 0x0B08u, 0x0000u }, { 0x0B09u, 0x0000u }, { 0x0B0Au, 0x0000u }, { 0x0B0Bu, 0x0000u }, 
    { 0x0B0Cu, 0x0000u }, { 0x0B0Du, 0x0000u }, { 0x0B0Eu, 0x0000u }, { 0x0B0Fu, 0x0000u }, 
    { 0x0B10u, 0x0000u }, { 0x0B11u, 0x0000u }, { 0x0B12u, 0x0000u }, { 0x0B13u, 0x0000u }, 
    { 0x0B14u, 0x0000u }, { 0x0B15u, 0x0000u }, { 0x0B16u, 0x0000u }, { 0x0B17u, 0x0000u }, 
    { 0x0B18u, 0x0000u }, { 0x0B19u, 0x0000u }, { 0x0B1Au, 0x0000u }, { 0x0B1Bu, 0x0000u }, 
    { 0x0B1Cu, 0x0000u }, { 0x0B1Du, 0x0000u }, { 0x0B1Eu, 0x0000u }, { 0x0B1Fu, 0x0000u }, 
    { 0x0B20u, 0x0000u }, { 0x0B21u, 0x0000u }, { 0x0B22u, 0x0000u }, { 0x0B23u, 0x0000u }, 
    { 0x0B24u, 0x0000u }, { 0x0B25u, 0x0000u }, { 0x0B26u, 0x0000u }, { 0x0B27u, 0x0000u }, 
    { 0x0B28u, 0x0000u }, { 0x0B29u, 0x0000u }, { 0x0B2Au, 0x0000u }, { 0x0B2Bu, 0x0000u }, 
    { 0x0B2Cu, 0x0000u }, { 0x0B2Du, 0x0000u }, { 0x0B2Eu, 0x0000u }, { 0x0B2Fu, 0x0000u }, 
    { 0x0B30u, 0x0000u }, { 0x0B31u, 0x0000u }, { 0x0B32u, 0x0000u }, { 0x0B33u, 0x0000u }, 
    { 0x0B34u, 0x0000u }, { 0x0B35u, 0x0000u }, { 0x0B36u, 0x0000u }, { 0x0B37u, 0x0000u }, 
    { 0x0B38u, 0x0000u }, { 0x0B39u, 0x0000u }, { 0x0B3Au, 0x0000u }, { 0x0B3Bu, 0x0000u }, 
    { 0x0B3Cu, 0x0000u }, { 0x0B3Du, 0x0000u }, { 0x0B3Eu, 0x0000u }, { 0x0B3Fu, 0x0000u }, 
    { 0x0B40u, 0x0000u }, { 0x0B41u, 0x0000u }, { 0x0B42u, 0x0000u }, { 0x0B43u, 0x0000u }, 
    { 0x0B44u, 0x0000u }, { 0x0B45u, 0x0000u }, { 0x0B46u, 0x0000u }, { 0x0B47u, 0x0000u }, 
    { 0x0B47u, 0x0B56u }, { 0x0B49u, 0x0000u }, { 0x0B4Au, 0x0000u }, { 0x0B47u, 0x0B3Eu }, 
    { 0x0B47u, 0x0B57u }, { 0x0B4Du, 0x0000u }, { 0x0B4Eu, 0x0000u }, { 0x0B4Fu, 0x0000u }, 
    { 0x0B50u, 0x0000u }, { 0x0B51u, 0x0000u }, { 0x0B52u, 0x0000u }, { 0x0B53u, 0x0000u }, 
    { 0x0B54u, 0x0000u }, { 0x0B55u, 0x0000u }, { 0x0B56u, 0x0000u }, { 0x0B57u, 0x0000u }, 
    { 0x0B58u, 0x0000u }, { 0x0B59u, 0x0000u }, { 0x0B5Au, 0x0000u }, { 0x0B5Bu, 0x0000u }, 
    { 0x0B21u, 0x0B3Cu }, { 0x0B22u, 0x0B3Cu }, { 0x0B5Eu, 0x0000u }, { 0x0B5Fu, 0x0000u }, 
    { 0x0B60u, 0x0000u }, { 0x0B61u, 0x0000u }, { 0x0B62u, 0x0000u }, { 0x0B63u, 0x0000u }, 
    { 0x0B64u, 0x0000u }, { 0x0B65u, 0x0000u }, { 0x0B66u, 0x0000u }, { 0x0B67u, 0x0000u }, 
    { 0x0B68u, 0x0000u }, { 0x0B69u, 0x0000u }, { 0x0B6Au, 0x0000u }, { 0x0B6Bu, 0x0000u }, 
    { 0x0B6Cu, 0x0000u }, { 0x0B6Du, 0x0000u }, { 0x0B6Eu, 0x0000u }, { 0x0B6Fu, 0x0000u }, 
    { 0x0B70u, 0x0000u }, { 0x0B71u, 0x0000u }, { 0x0B72u, 0x0000u }, { 0x0B73u, 0x0000u }, 
    { 0x0B74u, 0x0000u }, { 0x0B75u, 0x0000u }, { 0x0B76u, 0x0000u }, { 0x0B77u, 0x0000u }, 
    { 0x0B78u, 0x0000u }, { 0x0B79u, 0x0000u }, { 0x0B7Au, 0x0000u }, { 0x0B7Bu, 0x0000u }, 
    { 0x0B7Cu, 0x0000u }, { 0x0B7Du, 0x0000u }, { 0x0B7Eu, 0x0000u }, { 0x0B7Fu, 0x0000u }
  };

  const unsigned char OriyaB00::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_AL, 
    LB_AL, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_CM, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_AL, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_CM, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> OriyaB00::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000000000000011001100011100000000000000000000000000000000000000000000000000000000000001100"));

}; // namespace Babylon

dload(Babylon::OriyaB00);
