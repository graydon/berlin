/*$Id: C00-C7F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:08 +0200.
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

  class TeluguC00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    TeluguC00() {
      m_first_letter = 0xC00;
      m_last_letter  = 0xC7F;
      // m_version="3.1" // Not yet supported!

    }


    ~TeluguC00() {
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
      return "Telugu";
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
      case 0x0C66u:
        return 0;
        break;
      case 0x0C67u:
        return 1;
        break;
      case 0x0C68u:
        return 2;
        break;
      case 0x0C69u:
        return 3;
        break;
      case 0x0C6Au:
        return 4;
        break;
      case 0x0C6Bu:
        return 5;
        break;
      case 0x0C6Cu:
        return 6;
        break;
      case 0x0C6Du:
        return 7;
        break;
      case 0x0C6Eu:
        return 8;
        break;
      case 0x0C6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0C66u:
      case 0x0C67u:
      case 0x0C68u:
      case 0x0C69u:
      case 0x0C6Au:
      case 0x0C6Bu:
      case 0x0C6Cu:
      case 0x0C6Du:
      case 0x0C6Eu:
      case 0x0C6Fu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0C66u:
        return 0;
        break;
      case 0x0C67u:
        return 1;
        break;
      case 0x0C68u:
        return 2;
        break;
      case 0x0C69u:
        return 3;
        break;
      case 0x0C6Au:
        return 4;
        break;
      case 0x0C6Bu:
        return 5;
        break;
      case 0x0C6Cu:
        return 6;
        break;
      case 0x0C6Du:
        return 7;
        break;
      case 0x0C6Eu:
        return 8;
        break;
      case 0x0C6Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0C66u:
      case 0x0C67u:
      case 0x0C68u:
      case 0x0C69u:
      case 0x0C6Au:
      case 0x0C6Bu:
      case 0x0C6Cu:
      case 0x0C6Du:
      case 0x0C6Eu:
      case 0x0C6Fu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0C66u:
        return 0.000000;
        break;
      case 0x0C67u:
        return 1.000000;
        break;
      case 0x0C68u:
        return 2.000000;
        break;
      case 0x0C69u:
        return 3.000000;
        break;
      case 0x0C6Au:
        return 4.000000;
        break;
      case 0x0C6Bu:
        return 5.000000;
        break;
      case 0x0C6Cu:
        return 6.000000;
        break;
      case 0x0C6Du:
        return 7.000000;
        break;
      case 0x0C6Eu:
        return 8.000000;
        break;
      case 0x0C6Fu:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0C66u:
      case 0x0C67u:
      case 0x0C68u:
      case 0x0C69u:
      case 0x0C6Au:
      case 0x0C6Bu:
      case 0x0C6Cu:
      case 0x0C6Du:
      case 0x0C6Eu:
      case 0x0C6Fu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(TeluguC00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(TeluguC00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(TeluguC00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = TeluguC00::m_decompStr[uc - m_first_letter][0];
      us[1] = TeluguC00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(TeluguC00::m_lb[uc - m_first_letter]);
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
    TeluguC00(const TeluguC00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<128> m_is_defined;
    static const unsigned char _cat[128];
    static const unsigned char _comb_cl[128];
    static const unsigned char m_bidir[128];
    static const UCS2 m_decompStr[128][2];
    static const unsigned char m_lb[128];
    static const std::bitset<128> m_Other_Alphabetic;

  }; // class TeluguC00

    const std::bitset<128> TeluguC00::m_is_defined(std::string("00000000000000001111111111000011000000000110000000111101110111111100001111101111111111011111111111111111111111011101111111101110"));

  const unsigned char TeluguC00::_cat[] = {
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc
  };

  const unsigned char TeluguC00::_comb_cl[] = {
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
    0, 0, 0, 0, 0, 84, 91, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char TeluguC00::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS2 TeluguC00::m_decompStr[][2] = {
    { 0x0C00u, 0x0000u }, { 0x0C01u, 0x0000u }, { 0x0C02u, 0x0000u }, { 0x0C03u, 0x0000u }, 
    { 0x0C04u, 0x0000u }, { 0x0C05u, 0x0000u }, { 0x0C06u, 0x0000u }, { 0x0C07u, 0x0000u }, 
    { 0x0C08u, 0x0000u }, { 0x0C09u, 0x0000u }, { 0x0C0Au, 0x0000u }, { 0x0C0Bu, 0x0000u }, 
    { 0x0C0Cu, 0x0000u }, { 0x0C0Du, 0x0000u }, { 0x0C0Eu, 0x0000u }, { 0x0C0Fu, 0x0000u }, 
    { 0x0C10u, 0x0000u }, { 0x0C11u, 0x0000u }, { 0x0C12u, 0x0000u }, { 0x0C13u, 0x0000u }, 
    { 0x0C14u, 0x0000u }, { 0x0C15u, 0x0000u }, { 0x0C16u, 0x0000u }, { 0x0C17u, 0x0000u }, 
    { 0x0C18u, 0x0000u }, { 0x0C19u, 0x0000u }, { 0x0C1Au, 0x0000u }, { 0x0C1Bu, 0x0000u }, 
    { 0x0C1Cu, 0x0000u }, { 0x0C1Du, 0x0000u }, { 0x0C1Eu, 0x0000u }, { 0x0C1Fu, 0x0000u }, 
    { 0x0C20u, 0x0000u }, { 0x0C21u, 0x0000u }, { 0x0C22u, 0x0000u }, { 0x0C23u, 0x0000u }, 
    { 0x0C24u, 0x0000u }, { 0x0C25u, 0x0000u }, { 0x0C26u, 0x0000u }, { 0x0C27u, 0x0000u }, 
    { 0x0C28u, 0x0000u }, { 0x0C29u, 0x0000u }, { 0x0C2Au, 0x0000u }, { 0x0C2Bu, 0x0000u }, 
    { 0x0C2Cu, 0x0000u }, { 0x0C2Du, 0x0000u }, { 0x0C2Eu, 0x0000u }, { 0x0C2Fu, 0x0000u }, 
    { 0x0C30u, 0x0000u }, { 0x0C31u, 0x0000u }, { 0x0C32u, 0x0000u }, { 0x0C33u, 0x0000u }, 
    { 0x0C34u, 0x0000u }, { 0x0C35u, 0x0000u }, { 0x0C36u, 0x0000u }, { 0x0C37u, 0x0000u }, 
    { 0x0C38u, 0x0000u }, { 0x0C39u, 0x0000u }, { 0x0C3Au, 0x0000u }, { 0x0C3Bu, 0x0000u }, 
    { 0x0C3Cu, 0x0000u }, { 0x0C3Du, 0x0000u }, { 0x0C3Eu, 0x0000u }, { 0x0C3Fu, 0x0000u }, 
    { 0x0C40u, 0x0000u }, { 0x0C41u, 0x0000u }, { 0x0C42u, 0x0000u }, { 0x0C43u, 0x0000u }, 
    { 0x0C44u, 0x0000u }, { 0x0C45u, 0x0000u }, { 0x0C46u, 0x0000u }, { 0x0C47u, 0x0000u }, 
    { 0x0C46u, 0x0C56u }, { 0x0C49u, 0x0000u }, { 0x0C4Au, 0x0000u }, { 0x0C4Bu, 0x0000u }, 
    { 0x0C4Cu, 0x0000u }, { 0x0C4Du, 0x0000u }, { 0x0C4Eu, 0x0000u }, { 0x0C4Fu, 0x0000u }, 
    { 0x0C50u, 0x0000u }, { 0x0C51u, 0x0000u }, { 0x0C52u, 0x0000u }, { 0x0C53u, 0x0000u }, 
    { 0x0C54u, 0x0000u }, { 0x0C55u, 0x0000u }, { 0x0C56u, 0x0000u }, { 0x0C57u, 0x0000u }, 
    { 0x0C58u, 0x0000u }, { 0x0C59u, 0x0000u }, { 0x0C5Au, 0x0000u }, { 0x0C5Bu, 0x0000u }, 
    { 0x0C5Cu, 0x0000u }, { 0x0C5Du, 0x0000u }, { 0x0C5Eu, 0x0000u }, { 0x0C5Fu, 0x0000u }, 
    { 0x0C60u, 0x0000u }, { 0x0C61u, 0x0000u }, { 0x0C62u, 0x0000u }, { 0x0C63u, 0x0000u }, 
    { 0x0C64u, 0x0000u }, { 0x0C65u, 0x0000u }, { 0x0C66u, 0x0000u }, { 0x0C67u, 0x0000u }, 
    { 0x0C68u, 0x0000u }, { 0x0C69u, 0x0000u }, { 0x0C6Au, 0x0000u }, { 0x0C6Bu, 0x0000u }, 
    { 0x0C6Cu, 0x0000u }, { 0x0C6Du, 0x0000u }, { 0x0C6Eu, 0x0000u }, { 0x0C6Fu, 0x0000u }, 
    { 0x0C70u, 0x0000u }, { 0x0C71u, 0x0000u }, { 0x0C72u, 0x0000u }, { 0x0C73u, 0x0000u }, 
    { 0x0C74u, 0x0000u }, { 0x0C75u, 0x0000u }, { 0x0C76u, 0x0000u }, { 0x0C77u, 0x0000u }, 
    { 0x0C78u, 0x0000u }, { 0x0C79u, 0x0000u }, { 0x0C7Au, 0x0000u }, { 0x0C7Bu, 0x0000u }, 
    { 0x0C7Cu, 0x0000u }, { 0x0C7Du, 0x0000u }, { 0x0C7Eu, 0x0000u }, { 0x0C7Fu, 0x0000u }
  };

  const unsigned char TeluguC00::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, 
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

    const std::bitset<128> TeluguC00::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000110000000011101110111111100000000000000000000000000000000000000000000000000000000001110"));

}; // namespace Babylon

dload(Babylon::TeluguC00);
