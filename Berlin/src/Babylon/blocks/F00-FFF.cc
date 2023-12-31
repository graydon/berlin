/*$Id: F00-FFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:28 +0200.
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

  class TibetanF00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    TibetanF00() {
      m_first_letter = 0xF00;
      m_last_letter  = 0xFFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000F40, 0x00000FB5)] = 0x0F69;
      m_composeMap[make_pair(0x00000F42, 0x00000FB7)] = 0x0F43;
      m_composeMap[make_pair(0x00000F4C, 0x00000FB7)] = 0x0F4D;
      m_composeMap[make_pair(0x00000F51, 0x00000FB7)] = 0x0F52;
      m_composeMap[make_pair(0x00000F56, 0x00000FB7)] = 0x0F57;
      m_composeMap[make_pair(0x00000F5B, 0x00000FB7)] = 0x0F5C;
      m_composeMap[make_pair(0x00000F71, 0x00000F72)] = 0x0F73;
      m_composeMap[make_pair(0x00000F71, 0x00000F74)] = 0x0F75;
      m_composeMap[make_pair(0x00000F71, 0x00000F80)] = 0x0F81;
      m_composeMap[make_pair(0x00000F90, 0x00000FB5)] = 0x0FB9;
      m_composeMap[make_pair(0x00000F92, 0x00000FB7)] = 0x0F93;
      m_composeMap[make_pair(0x00000F9C, 0x00000FB7)] = 0x0F9D;
      m_composeMap[make_pair(0x00000FA1, 0x00000FB7)] = 0x0FA2;
      m_composeMap[make_pair(0x00000FA6, 0x00000FB7)] = 0x0FA7;
      m_composeMap[make_pair(0x00000FAB, 0x00000FB7)] = 0x0FAC;
      m_composeMap[make_pair(0x00000FB2, 0x00000F80)] = 0x0F76;
      m_composeMap[make_pair(0x00000FB3, 0x00000F80)] = 0x0F78;

    }


    ~TibetanF00() {
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
      return "Tibetan";
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
      case 0x0F20u:
        return 0;
        break;
      case 0x0F21u:
        return 1;
        break;
      case 0x0F22u:
        return 2;
        break;
      case 0x0F23u:
        return 3;
        break;
      case 0x0F24u:
        return 4;
        break;
      case 0x0F25u:
        return 5;
        break;
      case 0x0F26u:
        return 6;
        break;
      case 0x0F27u:
        return 7;
        break;
      case 0x0F28u:
        return 8;
        break;
      case 0x0F29u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0F20u:
      case 0x0F21u:
      case 0x0F22u:
      case 0x0F23u:
      case 0x0F24u:
      case 0x0F25u:
      case 0x0F26u:
      case 0x0F27u:
      case 0x0F28u:
      case 0x0F29u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0F20u:
        return 0;
        break;
      case 0x0F21u:
        return 1;
        break;
      case 0x0F22u:
        return 2;
        break;
      case 0x0F23u:
        return 3;
        break;
      case 0x0F24u:
        return 4;
        break;
      case 0x0F25u:
        return 5;
        break;
      case 0x0F26u:
        return 6;
        break;
      case 0x0F27u:
        return 7;
        break;
      case 0x0F28u:
        return 8;
        break;
      case 0x0F29u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0F20u:
      case 0x0F21u:
      case 0x0F22u:
      case 0x0F23u:
      case 0x0F24u:
      case 0x0F25u:
      case 0x0F26u:
      case 0x0F27u:
      case 0x0F28u:
      case 0x0F29u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0F20u:
        return 0.000000;
        break;
      case 0x0F21u:
        return 1.000000;
        break;
      case 0x0F22u:
        return 2.000000;
        break;
      case 0x0F23u:
        return 3.000000;
        break;
      case 0x0F24u:
        return 4.000000;
        break;
      case 0x0F25u:
        return 5.000000;
        break;
      case 0x0F26u:
        return 6.000000;
        break;
      case 0x0F27u:
        return 7.000000;
        break;
      case 0x0F28u:
        return 8.000000;
        break;
      case 0x0F29u:
        return 9.000000;
        break;
      case 0x0F2Au:
        return 0.500000;
        break;
      case 0x0F2Bu:
        return 1.500000;
        break;
      case 0x0F2Cu:
        return 2.500000;
        break;
      case 0x0F2Du:
        return 3.500000;
        break;
      case 0x0F2Eu:
        return 4.500000;
        break;
      case 0x0F2Fu:
        return 5.500000;
        break;
      case 0x0F30u:
        return 6.500000;
        break;
      case 0x0F31u:
        return 7.500000;
        break;
      case 0x0F32u:
        return 8.500000;
        break;
      case 0x0F33u:
        return -0.500000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0F20u:
      case 0x0F21u:
      case 0x0F22u:
      case 0x0F23u:
      case 0x0F24u:
      case 0x0F25u:
      case 0x0F26u:
      case 0x0F27u:
      case 0x0F28u:
      case 0x0F29u:
      case 0x0F2Au:
      case 0x0F2Bu:
      case 0x0F2Cu:
      case 0x0F2Du:
      case 0x0F2Eu:
      case 0x0F2Fu:
      case 0x0F30u:
      case 0x0F31u:
      case 0x0F32u:
      case 0x0F33u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(TibetanF00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(TibetanF00::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(TibetanF00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(TibetanF00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = TibetanF00::m_decompStr[uc - m_first_letter][0];
      us[1] = TibetanF00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(TibetanF00::m_lb[uc - m_first_letter]);
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
    TibetanF00(const TibetanF00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const unsigned char _cat[256];
    static const unsigned char _comb_cl[256];
    static const unsigned char m_bidir[256];
    static const unsigned char _decomp[256];
    static const UCS2 m_decompStr[256][2];
    static const unsigned char m_lb[256];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<256> m_Other_Alphabetic;
    static const std::bitset<256> m_Diacritic;

  }; // class TibetanF00

    const std::bitset<256> TibetanF00::m_is_defined(std::string("0000000000000000000000000000000000000000000000001001111111111111110111111111111111111111111111111111111011111111000011111111111111111111111111100000011111111111111111111111111111111110111111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char TibetanF00::_cat[] = {
    CAT_Lo, CAT_So, CAT_So, CAT_So, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_Mn, CAT_Mn, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_So, CAT_Mn, CAT_So, CAT_Mn, 
    CAT_So, CAT_Mn, CAT_Ps, CAT_Pe, CAT_Ps, CAT_Pe, CAT_Mc, CAT_Mc, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Po, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_Mn, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_Lo, CAT_Lo, CAT_So, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo
  };

  const unsigned char TibetanF00::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    220, 220, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 220, 0, 220, 
    0, 216, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 129, 130, 0, 132, 0, 0, 0, 
    0, 0, 130, 130, 130, 130, 0, 0, 
    130, 0, 230, 230, 9, 0, 230, 230, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 220, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char TibetanF00::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_NSM, 
    BIDIR_L, BIDIR_NSM, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const unsigned char TibetanF00::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_NOBREAK, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, 
    DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 TibetanF00::m_decompStr[][2] = {
    { 0x0F00u, 0x0000u }, { 0x0F01u, 0x0000u }, { 0x0F02u, 0x0000u }, { 0x0F03u, 0x0000u }, 
    { 0x0F04u, 0x0000u }, { 0x0F05u, 0x0000u }, { 0x0F06u, 0x0000u }, { 0x0F07u, 0x0000u }, 
    { 0x0F08u, 0x0000u }, { 0x0F09u, 0x0000u }, { 0x0F0Au, 0x0000u }, { 0x0F0Bu, 0x0000u }, 
    { 0x0F0Bu, 0x0000u }, { 0x0F0Du, 0x0000u }, { 0x0F0Eu, 0x0000u }, { 0x0F0Fu, 0x0000u }, 
    { 0x0F10u, 0x0000u }, { 0x0F11u, 0x0000u }, { 0x0F12u, 0x0000u }, { 0x0F13u, 0x0000u }, 
    { 0x0F14u, 0x0000u }, { 0x0F15u, 0x0000u }, { 0x0F16u, 0x0000u }, { 0x0F17u, 0x0000u }, 
    { 0x0F18u, 0x0000u }, { 0x0F19u, 0x0000u }, { 0x0F1Au, 0x0000u }, { 0x0F1Bu, 0x0000u }, 
    { 0x0F1Cu, 0x0000u }, { 0x0F1Du, 0x0000u }, { 0x0F1Eu, 0x0000u }, { 0x0F1Fu, 0x0000u }, 
    { 0x0F20u, 0x0000u }, { 0x0F21u, 0x0000u }, { 0x0F22u, 0x0000u }, { 0x0F23u, 0x0000u }, 
    { 0x0F24u, 0x0000u }, { 0x0F25u, 0x0000u }, { 0x0F26u, 0x0000u }, { 0x0F27u, 0x0000u }, 
    { 0x0F28u, 0x0000u }, { 0x0F29u, 0x0000u }, { 0x0F2Au, 0x0000u }, { 0x0F2Bu, 0x0000u }, 
    { 0x0F2Cu, 0x0000u }, { 0x0F2Du, 0x0000u }, { 0x0F2Eu, 0x0000u }, { 0x0F2Fu, 0x0000u }, 
    { 0x0F30u, 0x0000u }, { 0x0F31u, 0x0000u }, { 0x0F32u, 0x0000u }, { 0x0F33u, 0x0000u }, 
    { 0x0F34u, 0x0000u }, { 0x0F35u, 0x0000u }, { 0x0F36u, 0x0000u }, { 0x0F37u, 0x0000u }, 
    { 0x0F38u, 0x0000u }, { 0x0F39u, 0x0000u }, { 0x0F3Au, 0x0000u }, { 0x0F3Bu, 0x0000u }, 
    { 0x0F3Cu, 0x0000u }, { 0x0F3Du, 0x0000u }, { 0x0F3Eu, 0x0000u }, { 0x0F3Fu, 0x0000u }, 
    { 0x0F40u, 0x0000u }, { 0x0F41u, 0x0000u }, { 0x0F42u, 0x0000u }, { 0x0F42u, 0x0FB7u }, 
    { 0x0F44u, 0x0000u }, { 0x0F45u, 0x0000u }, { 0x0F46u, 0x0000u }, { 0x0F47u, 0x0000u }, 
    { 0x0F48u, 0x0000u }, { 0x0F49u, 0x0000u }, { 0x0F4Au, 0x0000u }, { 0x0F4Bu, 0x0000u }, 
    { 0x0F4Cu, 0x0000u }, { 0x0F4Cu, 0x0FB7u }, { 0x0F4Eu, 0x0000u }, { 0x0F4Fu, 0x0000u }, 
    { 0x0F50u, 0x0000u }, { 0x0F51u, 0x0000u }, { 0x0F51u, 0x0FB7u }, { 0x0F53u, 0x0000u }, 
    { 0x0F54u, 0x0000u }, { 0x0F55u, 0x0000u }, { 0x0F56u, 0x0000u }, { 0x0F56u, 0x0FB7u }, 
    { 0x0F58u, 0x0000u }, { 0x0F59u, 0x0000u }, { 0x0F5Au, 0x0000u }, { 0x0F5Bu, 0x0000u }, 
    { 0x0F5Bu, 0x0FB7u }, { 0x0F5Du, 0x0000u }, { 0x0F5Eu, 0x0000u }, { 0x0F5Fu, 0x0000u }, 
    { 0x0F60u, 0x0000u }, { 0x0F61u, 0x0000u }, { 0x0F62u, 0x0000u }, { 0x0F63u, 0x0000u }, 
    { 0x0F64u, 0x0000u }, { 0x0F65u, 0x0000u }, { 0x0F66u, 0x0000u }, { 0x0F67u, 0x0000u }, 
    { 0x0F68u, 0x0000u }, { 0x0F40u, 0x0FB5u }, { 0x0F6Au, 0x0000u }, { 0x0F6Bu, 0x0000u }, 
    { 0x0F6Cu, 0x0000u }, { 0x0F6Du, 0x0000u }, { 0x0F6Eu, 0x0000u }, { 0x0F6Fu, 0x0000u }, 
    { 0x0F70u, 0x0000u }, { 0x0F71u, 0x0000u }, { 0x0F72u, 0x0000u }, { 0x0F71u, 0x0F72u }, 
    { 0x0F74u, 0x0000u }, { 0x0F71u, 0x0F74u }, { 0x0FB2u, 0x0F80u }, { 0x0FB2u, 0x0F81u }, 
    { 0x0FB3u, 0x0F80u }, { 0x0FB3u, 0x0F81u }, { 0x0F7Au, 0x0000u }, { 0x0F7Bu, 0x0000u }, 
    { 0x0F7Cu, 0x0000u }, { 0x0F7Du, 0x0000u }, { 0x0F7Eu, 0x0000u }, { 0x0F7Fu, 0x0000u }, 
    { 0x0F80u, 0x0000u }, { 0x0F71u, 0x0F80u }, { 0x0F82u, 0x0000u }, { 0x0F83u, 0x0000u }, 
    { 0x0F84u, 0x0000u }, { 0x0F85u, 0x0000u }, { 0x0F86u, 0x0000u }, { 0x0F87u, 0x0000u }, 
    { 0x0F88u, 0x0000u }, { 0x0F89u, 0x0000u }, { 0x0F8Au, 0x0000u }, { 0x0F8Bu, 0x0000u }, 
    { 0x0F8Cu, 0x0000u }, { 0x0F8Du, 0x0000u }, { 0x0F8Eu, 0x0000u }, { 0x0F8Fu, 0x0000u }, 
    { 0x0F90u, 0x0000u }, { 0x0F91u, 0x0000u }, { 0x0F92u, 0x0000u }, { 0x0F92u, 0x0FB7u }, 
    { 0x0F94u, 0x0000u }, { 0x0F95u, 0x0000u }, { 0x0F96u, 0x0000u }, { 0x0F97u, 0x0000u }, 
    { 0x0F98u, 0x0000u }, { 0x0F99u, 0x0000u }, { 0x0F9Au, 0x0000u }, { 0x0F9Bu, 0x0000u }, 
    { 0x0F9Cu, 0x0000u }, { 0x0F9Cu, 0x0FB7u }, { 0x0F9Eu, 0x0000u }, { 0x0F9Fu, 0x0000u }, 
    { 0x0FA0u, 0x0000u }, { 0x0FA1u, 0x0000u }, { 0x0FA1u, 0x0FB7u }, { 0x0FA3u, 0x0000u }, 
    { 0x0FA4u, 0x0000u }, { 0x0FA5u, 0x0000u }, { 0x0FA6u, 0x0000u }, { 0x0FA6u, 0x0FB7u }, 
    { 0x0FA8u, 0x0000u }, { 0x0FA9u, 0x0000u }, { 0x0FAAu, 0x0000u }, { 0x0FABu, 0x0000u }, 
    { 0x0FABu, 0x0FB7u }, { 0x0FADu, 0x0000u }, { 0x0FAEu, 0x0000u }, { 0x0FAFu, 0x0000u }, 
    { 0x0FB0u, 0x0000u }, { 0x0FB1u, 0x0000u }, { 0x0FB2u, 0x0000u }, { 0x0FB3u, 0x0000u }, 
    { 0x0FB4u, 0x0000u }, { 0x0FB5u, 0x0000u }, { 0x0FB6u, 0x0000u }, { 0x0FB7u, 0x0000u }, 
    { 0x0FB8u, 0x0000u }, { 0x0F90u, 0x0FB5u }, { 0x0FBAu, 0x0000u }, { 0x0FBBu, 0x0000u }, 
    { 0x0FBCu, 0x0000u }, { 0x0FBDu, 0x0000u }, { 0x0FBEu, 0x0000u }, { 0x0FBFu, 0x0000u }, 
    { 0x0FC0u, 0x0000u }, { 0x0FC1u, 0x0000u }, { 0x0FC2u, 0x0000u }, { 0x0FC3u, 0x0000u }, 
    { 0x0FC4u, 0x0000u }, { 0x0FC5u, 0x0000u }, { 0x0FC6u, 0x0000u }, { 0x0FC7u, 0x0000u }, 
    { 0x0FC8u, 0x0000u }, { 0x0FC9u, 0x0000u }, { 0x0FCAu, 0x0000u }, { 0x0FCBu, 0x0000u }, 
    { 0x0FCCu, 0x0000u }, { 0x0FCDu, 0x0000u }, { 0x0FCEu, 0x0000u }, { 0x0FCFu, 0x0000u }, 
    { 0x0FD0u, 0x0000u }, { 0x0FD1u, 0x0000u }, { 0x0FD2u, 0x0000u }, { 0x0FD3u, 0x0000u }, 
    { 0x0FD4u, 0x0000u }, { 0x0FD5u, 0x0000u }, { 0x0FD6u, 0x0000u }, { 0x0FD7u, 0x0000u }, 
    { 0x0FD8u, 0x0000u }, { 0x0FD9u, 0x0000u }, { 0x0FDAu, 0x0000u }, { 0x0FDBu, 0x0000u }, 
    { 0x0FDCu, 0x0000u }, { 0x0FDDu, 0x0000u }, { 0x0FDEu, 0x0000u }, { 0x0FDFu, 0x0000u }, 
    { 0x0FE0u, 0x0000u }, { 0x0FE1u, 0x0000u }, { 0x0FE2u, 0x0000u }, { 0x0FE3u, 0x0000u }, 
    { 0x0FE4u, 0x0000u }, { 0x0FE5u, 0x0000u }, { 0x0FE6u, 0x0000u }, { 0x0FE7u, 0x0000u }, 
    { 0x0FE8u, 0x0000u }, { 0x0FE9u, 0x0000u }, { 0x0FEAu, 0x0000u }, { 0x0FEBu, 0x0000u }, 
    { 0x0FECu, 0x0000u }, { 0x0FEDu, 0x0000u }, { 0x0FEEu, 0x0000u }, { 0x0FEFu, 0x0000u }, 
    { 0x0FF0u, 0x0000u }, { 0x0FF1u, 0x0000u }, { 0x0FF2u, 0x0000u }, { 0x0FF3u, 0x0000u }, 
    { 0x0FF4u, 0x0000u }, { 0x0FF5u, 0x0000u }, { 0x0FF6u, 0x0000u }, { 0x0FF7u, 0x0000u }, 
    { 0x0FF8u, 0x0000u }, { 0x0FF9u, 0x0000u }, { 0x0FFAu, 0x0000u }, { 0x0FFBu, 0x0000u }, 
    { 0x0FFCu, 0x0000u }, { 0x0FFDu, 0x0000u }, { 0x0FFEu, 0x0000u }, { 0x0FFFu, 0x0000u }
  };

  const unsigned char TibetanF00::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_BA, LB_GL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, LB_CM, 
    LB_AL, LB_CM, LB_OP, LB_CL, LB_OP, LB_CL, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

    const std::bitset<256> TibetanF00::m_Other_Alphabetic(std::string("0000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111111111111111011111111000000000000001101111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<256> TibetanF00::m_Diacritic(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001101110000000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000011000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::TibetanF00);
