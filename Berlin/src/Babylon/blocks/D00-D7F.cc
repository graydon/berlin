/*$Id: D00-D7F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:15 +0200.
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

  class MalayalamD00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    MalayalamD00() {
      m_first_letter = 0xD00;
      m_last_letter  = 0xD7F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000D46, 0x00000D3E)] = 0x0D4A;
      m_composeMap[make_pair(0x00000D46, 0x00000D57)] = 0x0D4C;
      m_composeMap[make_pair(0x00000D47, 0x00000D3E)] = 0x0D4B;

    }


    ~MalayalamD00() {
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
      return "Malayalam";
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
      case 0x0D66u:
        return 0;
        break;
      case 0x0D67u:
        return 1;
        break;
      case 0x0D68u:
        return 2;
        break;
      case 0x0D69u:
        return 3;
        break;
      case 0x0D6Au:
        return 4;
        break;
      case 0x0D6Bu:
        return 5;
        break;
      case 0x0D6Cu:
        return 6;
        break;
      case 0x0D6Du:
        return 7;
        break;
      case 0x0D6Eu:
        return 8;
        break;
      case 0x0D6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0D66u:
      case 0x0D67u:
      case 0x0D68u:
      case 0x0D69u:
      case 0x0D6Au:
      case 0x0D6Bu:
      case 0x0D6Cu:
      case 0x0D6Du:
      case 0x0D6Eu:
      case 0x0D6Fu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0D66u:
        return 0;
        break;
      case 0x0D67u:
        return 1;
        break;
      case 0x0D68u:
        return 2;
        break;
      case 0x0D69u:
        return 3;
        break;
      case 0x0D6Au:
        return 4;
        break;
      case 0x0D6Bu:
        return 5;
        break;
      case 0x0D6Cu:
        return 6;
        break;
      case 0x0D6Du:
        return 7;
        break;
      case 0x0D6Eu:
        return 8;
        break;
      case 0x0D6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0D66u:
      case 0x0D67u:
      case 0x0D68u:
      case 0x0D69u:
      case 0x0D6Au:
      case 0x0D6Bu:
      case 0x0D6Cu:
      case 0x0D6Du:
      case 0x0D6Eu:
      case 0x0D6Fu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0D66u:
        return 0.000000;
        break;
      case 0x0D67u:
        return 1.000000;
        break;
      case 0x0D68u:
        return 2.000000;
        break;
      case 0x0D69u:
        return 3.000000;
        break;
      case 0x0D6Au:
        return 4.000000;
        break;
      case 0x0D6Bu:
        return 5.000000;
        break;
      case 0x0D6Cu:
        return 6.000000;
        break;
      case 0x0D6Du:
        return 7.000000;
        break;
      case 0x0D6Eu:
        return 8.000000;
        break;
      case 0x0D6Fu:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0D66u:
      case 0x0D67u:
      case 0x0D68u:
      case 0x0D69u:
      case 0x0D6Au:
      case 0x0D6Bu:
      case 0x0D6Cu:
      case 0x0D6Du:
      case 0x0D6Eu:
      case 0x0D6Fu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(MalayalamD00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(MalayalamD00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(MalayalamD00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = MalayalamD00::m_decompStr[uc - m_first_letter][0];
      us[1] = MalayalamD00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(MalayalamD00::m_lb[uc - m_first_letter]);
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
    MalayalamD00(const MalayalamD00 &) {}

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

  }; // class MalayalamD00

    const std::bitset<128> MalayalamD00::m_is_defined(std::string("00000000000000001111111111000011000000001000000000111101110011111100001111111111111111011111111111111111111111011101111111101100"));

  const unsigned char MalayalamD00::_cat[] = {
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc
  };

  const unsigned char MalayalamD00::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 9, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char MalayalamD00::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS2 MalayalamD00::m_decompStr[][2] = {
    { 0x0D00u, 0x0000u }, { 0x0D01u, 0x0000u }, { 0x0D02u, 0x0000u }, { 0x0D03u, 0x0000u }, 
    { 0x0D04u, 0x0000u }, { 0x0D05u, 0x0000u }, { 0x0D06u, 0x0000u }, { 0x0D07u, 0x0000u }, 
    { 0x0D08u, 0x0000u }, { 0x0D09u, 0x0000u }, { 0x0D0Au, 0x0000u }, { 0x0D0Bu, 0x0000u }, 
    { 0x0D0Cu, 0x0000u }, { 0x0D0Du, 0x0000u }, { 0x0D0Eu, 0x0000u }, { 0x0D0Fu, 0x0000u }, 
    { 0x0D10u, 0x0000u }, { 0x0D11u, 0x0000u }, { 0x0D12u, 0x0000u }, { 0x0D13u, 0x0000u }, 
    { 0x0D14u, 0x0000u }, { 0x0D15u, 0x0000u }, { 0x0D16u, 0x0000u }, { 0x0D17u, 0x0000u }, 
    { 0x0D18u, 0x0000u }, { 0x0D19u, 0x0000u }, { 0x0D1Au, 0x0000u }, { 0x0D1Bu, 0x0000u }, 
    { 0x0D1Cu, 0x0000u }, { 0x0D1Du, 0x0000u }, { 0x0D1Eu, 0x0000u }, { 0x0D1Fu, 0x0000u }, 
    { 0x0D20u, 0x0000u }, { 0x0D21u, 0x0000u }, { 0x0D22u, 0x0000u }, { 0x0D23u, 0x0000u }, 
    { 0x0D24u, 0x0000u }, { 0x0D25u, 0x0000u }, { 0x0D26u, 0x0000u }, { 0x0D27u, 0x0000u }, 
    { 0x0D28u, 0x0000u }, { 0x0D29u, 0x0000u }, { 0x0D2Au, 0x0000u }, { 0x0D2Bu, 0x0000u }, 
    { 0x0D2Cu, 0x0000u }, { 0x0D2Du, 0x0000u }, { 0x0D2Eu, 0x0000u }, { 0x0D2Fu, 0x0000u }, 
    { 0x0D30u, 0x0000u }, { 0x0D31u, 0x0000u }, { 0x0D32u, 0x0000u }, { 0x0D33u, 0x0000u }, 
    { 0x0D34u, 0x0000u }, { 0x0D35u, 0x0000u }, { 0x0D36u, 0x0000u }, { 0x0D37u, 0x0000u }, 
    { 0x0D38u, 0x0000u }, { 0x0D39u, 0x0000u }, { 0x0D3Au, 0x0000u }, { 0x0D3Bu, 0x0000u }, 
    { 0x0D3Cu, 0x0000u }, { 0x0D3Du, 0x0000u }, { 0x0D3Eu, 0x0000u }, { 0x0D3Fu, 0x0000u }, 
    { 0x0D40u, 0x0000u }, { 0x0D41u, 0x0000u }, { 0x0D42u, 0x0000u }, { 0x0D43u, 0x0000u }, 
    { 0x0D44u, 0x0000u }, { 0x0D45u, 0x0000u }, { 0x0D46u, 0x0000u }, { 0x0D47u, 0x0000u }, 
    { 0x0D48u, 0x0000u }, { 0x0D49u, 0x0000u }, { 0x0D46u, 0x0D3Eu }, { 0x0D47u, 0x0D3Eu }, 
    { 0x0D46u, 0x0D57u }, { 0x0D4Du, 0x0000u }, { 0x0D4Eu, 0x0000u }, { 0x0D4Fu, 0x0000u }, 
    { 0x0D50u, 0x0000u }, { 0x0D51u, 0x0000u }, { 0x0D52u, 0x0000u }, { 0x0D53u, 0x0000u }, 
    { 0x0D54u, 0x0000u }, { 0x0D55u, 0x0000u }, { 0x0D56u, 0x0000u }, { 0x0D57u, 0x0000u }, 
    { 0x0D58u, 0x0000u }, { 0x0D59u, 0x0000u }, { 0x0D5Au, 0x0000u }, { 0x0D5Bu, 0x0000u }, 
    { 0x0D5Cu, 0x0000u }, { 0x0D5Du, 0x0000u }, { 0x0D5Eu, 0x0000u }, { 0x0D5Fu, 0x0000u }, 
    { 0x0D60u, 0x0000u }, { 0x0D61u, 0x0000u }, { 0x0D62u, 0x0000u }, { 0x0D63u, 0x0000u }, 
    { 0x0D64u, 0x0000u }, { 0x0D65u, 0x0000u }, { 0x0D66u, 0x0000u }, { 0x0D67u, 0x0000u }, 
    { 0x0D68u, 0x0000u }, { 0x0D69u, 0x0000u }, { 0x0D6Au, 0x0000u }, { 0x0D6Bu, 0x0000u }, 
    { 0x0D6Cu, 0x0000u }, { 0x0D6Du, 0x0000u }, { 0x0D6Eu, 0x0000u }, { 0x0D6Fu, 0x0000u }, 
    { 0x0D70u, 0x0000u }, { 0x0D71u, 0x0000u }, { 0x0D72u, 0x0000u }, { 0x0D73u, 0x0000u }, 
    { 0x0D74u, 0x0000u }, { 0x0D75u, 0x0000u }, { 0x0D76u, 0x0000u }, { 0x0D77u, 0x0000u }, 
    { 0x0D78u, 0x0000u }, { 0x0D79u, 0x0000u }, { 0x0D7Au, 0x0000u }, { 0x0D7Bu, 0x0000u }, 
    { 0x0D7Cu, 0x0000u }, { 0x0D7Du, 0x0000u }, { 0x0D7Eu, 0x0000u }, { 0x0D7Fu, 0x0000u }
  };

  const unsigned char MalayalamD00::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> MalayalamD00::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000000000000011101110011111100000000000000000000000000000000000000000000000000000000001100"));

}; // namespace Babylon

dload(Babylon::MalayalamD00);
