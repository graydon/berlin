/*$Id: E00-E7F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:21 +0200.
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

namespace Babylon {

  class ThaiE00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    ThaiE00() {
      m_first_letter = 0xE00;
      m_last_letter  = 0xE7F;
      // m_version="3.1" // Not yet supported!

    }


    ~ThaiE00() {
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
      return "Thai";
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
      case 0x0E50u:
        return 0;
        break;
      case 0x0E51u:
        return 1;
        break;
      case 0x0E52u:
        return 2;
        break;
      case 0x0E53u:
        return 3;
        break;
      case 0x0E54u:
        return 4;
        break;
      case 0x0E55u:
        return 5;
        break;
      case 0x0E56u:
        return 6;
        break;
      case 0x0E57u:
        return 7;
        break;
      case 0x0E58u:
        return 8;
        break;
      case 0x0E59u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0E50u:
      case 0x0E51u:
      case 0x0E52u:
      case 0x0E53u:
      case 0x0E54u:
      case 0x0E55u:
      case 0x0E56u:
      case 0x0E57u:
      case 0x0E58u:
      case 0x0E59u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0E50u:
        return 0;
        break;
      case 0x0E51u:
        return 1;
        break;
      case 0x0E52u:
        return 2;
        break;
      case 0x0E53u:
        return 3;
        break;
      case 0x0E54u:
        return 4;
        break;
      case 0x0E55u:
        return 5;
        break;
      case 0x0E56u:
        return 6;
        break;
      case 0x0E57u:
        return 7;
        break;
      case 0x0E58u:
        return 8;
        break;
      case 0x0E59u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0E50u:
      case 0x0E51u:
      case 0x0E52u:
      case 0x0E53u:
      case 0x0E54u:
      case 0x0E55u:
      case 0x0E56u:
      case 0x0E57u:
      case 0x0E58u:
      case 0x0E59u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0E50u:
        return 0.000000;
        break;
      case 0x0E51u:
        return 1.000000;
        break;
      case 0x0E52u:
        return 2.000000;
        break;
      case 0x0E53u:
        return 3.000000;
        break;
      case 0x0E54u:
        return 4.000000;
        break;
      case 0x0E55u:
        return 5.000000;
        break;
      case 0x0E56u:
        return 6.000000;
        break;
      case 0x0E57u:
        return 7.000000;
        break;
      case 0x0E58u:
        return 8.000000;
        break;
      case 0x0E59u:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0E50u:
      case 0x0E51u:
      case 0x0E52u:
      case 0x0E53u:
      case 0x0E54u:
      case 0x0E55u:
      case 0x0E56u:
      case 0x0E57u:
      case 0x0E58u:
      case 0x0E59u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(ThaiE00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(ThaiE00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(ThaiE00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(ThaiE00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = ThaiE00::m_decompStr[uc - m_first_letter][0];
      us[1] = ThaiE00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(ThaiE00::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_N);
    }

    UCS4 compose (const UCS4 start, const UCS4 last) {
      return 0;
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
      return m_Terminal_Punctuation.test(uc - m_first_letter);
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
    ThaiE00(const ThaiE00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<128> m_is_defined;
    static const unsigned char _cat[128];
    static const unsigned char _comb_cl[128];
    static const unsigned char m_bidir[128];
    static const unsigned char _decomp[128];
    static const UCS2 m_decompStr[128][2];
    static const unsigned char m_lb[128];
    static const std::bitset<128> m_Terminal_Punctuation;
    static const std::bitset<128> m_Other_Alphabetic;
    static const std::bitset<128> m_Diacritic;

  }; // class ThaiE00

    const std::bitset<128> ThaiE00::m_is_defined(std::string("00000000000000000000000000000000000011111111111111111111111111111000011111111111111111111111111111111111111111111111111111111110"));

  const unsigned char ThaiE00::_cat[] = {
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Sc, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lm, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Po, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Po, CAT_Po, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo
  };

  const unsigned char ThaiE00::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    103, 103, 9, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    107, 107, 107, 107, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char ThaiE00::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ET, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const unsigned char ThaiE00::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 ThaiE00::m_decompStr[][2] = {
    { 0x0E00u, 0x0000u }, { 0x0E01u, 0x0000u }, { 0x0E02u, 0x0000u }, { 0x0E03u, 0x0000u }, 
    { 0x0E04u, 0x0000u }, { 0x0E05u, 0x0000u }, { 0x0E06u, 0x0000u }, { 0x0E07u, 0x0000u }, 
    { 0x0E08u, 0x0000u }, { 0x0E09u, 0x0000u }, { 0x0E0Au, 0x0000u }, { 0x0E0Bu, 0x0000u }, 
    { 0x0E0Cu, 0x0000u }, { 0x0E0Du, 0x0000u }, { 0x0E0Eu, 0x0000u }, { 0x0E0Fu, 0x0000u }, 
    { 0x0E10u, 0x0000u }, { 0x0E11u, 0x0000u }, { 0x0E12u, 0x0000u }, { 0x0E13u, 0x0000u }, 
    { 0x0E14u, 0x0000u }, { 0x0E15u, 0x0000u }, { 0x0E16u, 0x0000u }, { 0x0E17u, 0x0000u }, 
    { 0x0E18u, 0x0000u }, { 0x0E19u, 0x0000u }, { 0x0E1Au, 0x0000u }, { 0x0E1Bu, 0x0000u }, 
    { 0x0E1Cu, 0x0000u }, { 0x0E1Du, 0x0000u }, { 0x0E1Eu, 0x0000u }, { 0x0E1Fu, 0x0000u }, 
    { 0x0E20u, 0x0000u }, { 0x0E21u, 0x0000u }, { 0x0E22u, 0x0000u }, { 0x0E23u, 0x0000u }, 
    { 0x0E24u, 0x0000u }, { 0x0E25u, 0x0000u }, { 0x0E26u, 0x0000u }, { 0x0E27u, 0x0000u }, 
    { 0x0E28u, 0x0000u }, { 0x0E29u, 0x0000u }, { 0x0E2Au, 0x0000u }, { 0x0E2Bu, 0x0000u }, 
    { 0x0E2Cu, 0x0000u }, { 0x0E2Du, 0x0000u }, { 0x0E2Eu, 0x0000u }, { 0x0E2Fu, 0x0000u }, 
    { 0x0E30u, 0x0000u }, { 0x0E31u, 0x0000u }, { 0x0E32u, 0x0000u }, { 0x0E4Du, 0x0E32u }, 
    { 0x0E34u, 0x0000u }, { 0x0E35u, 0x0000u }, { 0x0E36u, 0x0000u }, { 0x0E37u, 0x0000u }, 
    { 0x0E38u, 0x0000u }, { 0x0E39u, 0x0000u }, { 0x0E3Au, 0x0000u }, { 0x0E3Bu, 0x0000u }, 
    { 0x0E3Cu, 0x0000u }, { 0x0E3Du, 0x0000u }, { 0x0E3Eu, 0x0000u }, { 0x0E3Fu, 0x0000u }, 
    { 0x0E40u, 0x0000u }, { 0x0E41u, 0x0000u }, { 0x0E42u, 0x0000u }, { 0x0E43u, 0x0000u }, 
    { 0x0E44u, 0x0000u }, { 0x0E45u, 0x0000u }, { 0x0E46u, 0x0000u }, { 0x0E47u, 0x0000u }, 
    { 0x0E48u, 0x0000u }, { 0x0E49u, 0x0000u }, { 0x0E4Au, 0x0000u }, { 0x0E4Bu, 0x0000u }, 
    { 0x0E4Cu, 0x0000u }, { 0x0E4Du, 0x0000u }, { 0x0E4Eu, 0x0000u }, { 0x0E4Fu, 0x0000u }, 
    { 0x0E50u, 0x0000u }, { 0x0E51u, 0x0000u }, { 0x0E52u, 0x0000u }, { 0x0E53u, 0x0000u }, 
    { 0x0E54u, 0x0000u }, { 0x0E55u, 0x0000u }, { 0x0E56u, 0x0000u }, { 0x0E57u, 0x0000u }, 
    { 0x0E58u, 0x0000u }, { 0x0E59u, 0x0000u }, { 0x0E5Au, 0x0000u }, { 0x0E5Bu, 0x0000u }, 
    { 0x0E5Cu, 0x0000u }, { 0x0E5Du, 0x0000u }, { 0x0E5Eu, 0x0000u }, { 0x0E5Fu, 0x0000u }, 
    { 0x0E60u, 0x0000u }, { 0x0E61u, 0x0000u }, { 0x0E62u, 0x0000u }, { 0x0E63u, 0x0000u }, 
    { 0x0E64u, 0x0000u }, { 0x0E65u, 0x0000u }, { 0x0E66u, 0x0000u }, { 0x0E67u, 0x0000u }, 
    { 0x0E68u, 0x0000u }, { 0x0E69u, 0x0000u }, { 0x0E6Au, 0x0000u }, { 0x0E6Bu, 0x0000u }, 
    { 0x0E6Cu, 0x0000u }, { 0x0E6Du, 0x0000u }, { 0x0E6Eu, 0x0000u }, { 0x0E6Fu, 0x0000u }, 
    { 0x0E70u, 0x0000u }, { 0x0E71u, 0x0000u }, { 0x0E72u, 0x0000u }, { 0x0E73u, 0x0000u }, 
    { 0x0E74u, 0x0000u }, { 0x0E75u, 0x0000u }, { 0x0E76u, 0x0000u }, { 0x0E77u, 0x0000u }, 
    { 0x0E78u, 0x0000u }, { 0x0E79u, 0x0000u }, { 0x0E7Au, 0x0000u }, { 0x0E7Bu, 0x0000u }, 
    { 0x0E7Cu, 0x0000u }, { 0x0E7Du, 0x0000u }, { 0x0E7Eu, 0x0000u }, { 0x0E7Fu, 0x0000u }
  };

  const unsigned char ThaiE00::m_lb[] = {
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_CM, LB_SA, LB_SA, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_SA, LB_SA, LB_SA, LB_SA, LB_PR, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NS, LB_NS, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA
  };

    const std::bitset<128> ThaiE00::m_Terminal_Punctuation(std::string("00000000000000000000000000000000000011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<128> ThaiE00::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000000000000000000000000000000011111110000000000000000000000000000000000000000000000000000"));

    const std::bitset<128> ThaiE00::m_Diacritic(std::string("00000000000000000000000000000000000000000000000000011111100000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::ThaiE00);
