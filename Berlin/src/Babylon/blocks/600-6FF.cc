/*$Id: 600-6FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:34 +0200.
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

  class Arabic600 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Arabic600() {
      m_first_letter = 0x600;
      m_last_letter  = 0x6FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000627, 0x00000653)] = 0x0622;
      m_composeMap[make_pair(0x00000627, 0x00000654)] = 0x0623;
      m_composeMap[make_pair(0x00000627, 0x00000655)] = 0x0625;
      m_composeMap[make_pair(0x00000648, 0x00000654)] = 0x0624;
      m_composeMap[make_pair(0x0000064A, 0x00000654)] = 0x0626;
      m_composeMap[make_pair(0x000006C1, 0x00000654)] = 0x06C2;
      m_composeMap[make_pair(0x000006D2, 0x00000654)] = 0x06D3;
      m_composeMap[make_pair(0x000006D5, 0x00000654)] = 0x06C0;

    }


    ~Arabic600() {
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
      return "Arabic";
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
      case 0x0660u:
        return 0;
        break;
      case 0x0661u:
        return 1;
        break;
      case 0x0662u:
        return 2;
        break;
      case 0x0663u:
        return 3;
        break;
      case 0x0664u:
        return 4;
        break;
      case 0x0665u:
        return 5;
        break;
      case 0x0666u:
        return 6;
        break;
      case 0x0667u:
        return 7;
        break;
      case 0x0668u:
        return 8;
        break;
      case 0x0669u:
        return 9;
        break;
      case 0x06F0u:
        return 0;
        break;
      case 0x06F1u:
        return 1;
        break;
      case 0x06F2u:
        return 2;
        break;
      case 0x06F3u:
        return 3;
        break;
      case 0x06F4u:
        return 4;
        break;
      case 0x06F5u:
        return 5;
        break;
      case 0x06F6u:
        return 6;
        break;
      case 0x06F7u:
        return 7;
        break;
      case 0x06F8u:
        return 8;
        break;
      case 0x06F9u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0660u:
      case 0x0661u:
      case 0x0662u:
      case 0x0663u:
      case 0x0664u:
      case 0x0665u:
      case 0x0666u:
      case 0x0667u:
      case 0x0668u:
      case 0x0669u:
      case 0x06F0u:
      case 0x06F1u:
      case 0x06F2u:
      case 0x06F3u:
      case 0x06F4u:
      case 0x06F5u:
      case 0x06F6u:
      case 0x06F7u:
      case 0x06F8u:
      case 0x06F9u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0660u:
        return 0;
        break;
      case 0x0661u:
        return 1;
        break;
      case 0x0662u:
        return 2;
        break;
      case 0x0663u:
        return 3;
        break;
      case 0x0664u:
        return 4;
        break;
      case 0x0665u:
        return 5;
        break;
      case 0x0666u:
        return 6;
        break;
      case 0x0667u:
        return 7;
        break;
      case 0x0668u:
        return 8;
        break;
      case 0x0669u:
        return 9;
        break;
      case 0x06F0u:
        return 0;
        break;
      case 0x06F1u:
        return 1;
        break;
      case 0x06F2u:
        return 2;
        break;
      case 0x06F3u:
        return 3;
        break;
      case 0x06F4u:
        return 4;
        break;
      case 0x06F5u:
        return 5;
        break;
      case 0x06F6u:
        return 6;
        break;
      case 0x06F7u:
        return 7;
        break;
      case 0x06F8u:
        return 8;
        break;
      case 0x06F9u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0660u:
      case 0x0661u:
      case 0x0662u:
      case 0x0663u:
      case 0x0664u:
      case 0x0665u:
      case 0x0666u:
      case 0x0667u:
      case 0x0668u:
      case 0x0669u:
      case 0x06F0u:
      case 0x06F1u:
      case 0x06F2u:
      case 0x06F3u:
      case 0x06F4u:
      case 0x06F5u:
      case 0x06F6u:
      case 0x06F7u:
      case 0x06F8u:
      case 0x06F9u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0660u:
        return 0.000000;
        break;
      case 0x0661u:
        return 1.000000;
        break;
      case 0x0662u:
        return 2.000000;
        break;
      case 0x0663u:
        return 3.000000;
        break;
      case 0x0664u:
        return 4.000000;
        break;
      case 0x0665u:
        return 5.000000;
        break;
      case 0x0666u:
        return 6.000000;
        break;
      case 0x0667u:
        return 7.000000;
        break;
      case 0x0668u:
        return 8.000000;
        break;
      case 0x0669u:
        return 9.000000;
        break;
      case 0x06F0u:
        return 0.000000;
        break;
      case 0x06F1u:
        return 1.000000;
        break;
      case 0x06F2u:
        return 2.000000;
        break;
      case 0x06F3u:
        return 3.000000;
        break;
      case 0x06F4u:
        return 4.000000;
        break;
      case 0x06F5u:
        return 5.000000;
        break;
      case 0x06F6u:
        return 6.000000;
        break;
      case 0x06F7u:
        return 7.000000;
        break;
      case 0x06F8u:
        return 8.000000;
        break;
      case 0x06F9u:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0660u:
      case 0x0661u:
      case 0x0662u:
      case 0x0663u:
      case 0x0664u:
      case 0x0665u:
      case 0x0666u:
      case 0x0667u:
      case 0x0668u:
      case 0x0669u:
      case 0x06F0u:
      case 0x06F1u:
      case 0x06F2u:
      case 0x06F3u:
      case 0x06F4u:
      case 0x06F5u:
      case 0x06F6u:
      case 0x06F7u:
      case 0x06F8u:
      case 0x06F9u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Arabic600::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Arabic600::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Arabic600::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Arabic600::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Arabic600::m_decompStr[uc - m_first_letter][0];
      us[1] = Arabic600::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Arabic600::m_lb[uc - m_first_letter]);
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
    Arabic600(const Arabic600 &) {}

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

  }; // class Arabic600

    const std::bitset<256> Arabic600::m_is_defined(std::string("0111111111111111001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110011111111111111000000000011111111111111111111110000011111111111111111111111111010001000000000000001000000000000"));

  const unsigned char Arabic600::_cat[] = {
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Lm, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Po, CAT_Lo, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Me, CAT_Me, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lm, CAT_Lm, CAT_Mn, 
    CAT_Mn, CAT_So, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Po, CAT_Po, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Lo, CAT_Lo, CAT_Lo, CAT_So, CAT_So, CAT_Po
  };

  const unsigned char Arabic600::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 27, 28, 29, 30, 31, 
    32, 33, 34, 230, 230, 220, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    35, 0, 0, 0, 0, 0, 0, 0, 
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
    0, 0, 0, 0, 0, 0, 230, 230, 
    230, 230, 230, 230, 230, 0, 0, 230, 
    230, 230, 230, 220, 230, 0, 0, 230, 
    230, 0, 220, 230, 230, 220, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Arabic600::m_bidir[] = {
    BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, 
    BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, 
    BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, 
    BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_AL, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_AL, 
    BIDIR_CS, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_CS, BIDIR_CS, 
    BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS, 
    BIDIR_AN, BIDIR_AN, BIDIR_AN, BIDIR_AN, BIDIR_AN, BIDIR_AN, BIDIR_AN, BIDIR_AN, 
    BIDIR_AN, BIDIR_AN, BIDIR_ET, BIDIR_AN, BIDIR_AN, BIDIR_AL, BIDIR_CS, BIDIR_CS, 
    BIDIR_NSM, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_AL, BIDIR_AL, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_ON, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_CS, BIDIR_CS, 
    BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, 
    BIDIR_EN, BIDIR_EN, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_CS
  };

  const unsigned char Arabic600::_decomp[] = {
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
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

  const UCS2 Arabic600::m_decompStr[][2] = {
    { 0x0600u, 0x0000u }, { 0x0601u, 0x0000u }, { 0x0602u, 0x0000u }, { 0x0603u, 0x0000u }, 
    { 0x0604u, 0x0000u }, { 0x0605u, 0x0000u }, { 0x0606u, 0x0000u }, { 0x0607u, 0x0000u }, 
    { 0x0608u, 0x0000u }, { 0x0609u, 0x0000u }, { 0x060Au, 0x0000u }, { 0x060Bu, 0x0000u }, 
    { 0x060Cu, 0x0000u }, { 0x060Du, 0x0000u }, { 0x060Eu, 0x0000u }, { 0x060Fu, 0x0000u }, 
    { 0x0610u, 0x0000u }, { 0x0611u, 0x0000u }, { 0x0612u, 0x0000u }, { 0x0613u, 0x0000u }, 
    { 0x0614u, 0x0000u }, { 0x0615u, 0x0000u }, { 0x0616u, 0x0000u }, { 0x0617u, 0x0000u }, 
    { 0x0618u, 0x0000u }, { 0x0619u, 0x0000u }, { 0x061Au, 0x0000u }, { 0x061Bu, 0x0000u }, 
    { 0x061Cu, 0x0000u }, { 0x061Du, 0x0000u }, { 0x061Eu, 0x0000u }, { 0x061Fu, 0x0000u }, 
    { 0x0620u, 0x0000u }, { 0x0621u, 0x0000u }, { 0x0627u, 0x0653u }, { 0x0627u, 0x0654u }, 
    { 0x0648u, 0x0654u }, { 0x0627u, 0x0655u }, { 0x064Au, 0x0654u }, { 0x0627u, 0x0000u }, 
    { 0x0628u, 0x0000u }, { 0x0629u, 0x0000u }, { 0x062Au, 0x0000u }, { 0x062Bu, 0x0000u }, 
    { 0x062Cu, 0x0000u }, { 0x062Du, 0x0000u }, { 0x062Eu, 0x0000u }, { 0x062Fu, 0x0000u }, 
    { 0x0630u, 0x0000u }, { 0x0631u, 0x0000u }, { 0x0632u, 0x0000u }, { 0x0633u, 0x0000u }, 
    { 0x0634u, 0x0000u }, { 0x0635u, 0x0000u }, { 0x0636u, 0x0000u }, { 0x0637u, 0x0000u }, 
    { 0x0638u, 0x0000u }, { 0x0639u, 0x0000u }, { 0x063Au, 0x0000u }, { 0x063Bu, 0x0000u }, 
    { 0x063Cu, 0x0000u }, { 0x063Du, 0x0000u }, { 0x063Eu, 0x0000u }, { 0x063Fu, 0x0000u }, 
    { 0x0640u, 0x0000u }, { 0x0641u, 0x0000u }, { 0x0642u, 0x0000u }, { 0x0643u, 0x0000u }, 
    { 0x0644u, 0x0000u }, { 0x0645u, 0x0000u }, { 0x0646u, 0x0000u }, { 0x0647u, 0x0000u }, 
    { 0x0648u, 0x0000u }, { 0x0649u, 0x0000u }, { 0x064Au, 0x0000u }, { 0x064Bu, 0x0000u }, 
    { 0x064Cu, 0x0000u }, { 0x064Du, 0x0000u }, { 0x064Eu, 0x0000u }, { 0x064Fu, 0x0000u }, 
    { 0x0650u, 0x0000u }, { 0x0651u, 0x0000u }, { 0x0652u, 0x0000u }, { 0x0653u, 0x0000u }, 
    { 0x0654u, 0x0000u }, { 0x0655u, 0x0000u }, { 0x0656u, 0x0000u }, { 0x0657u, 0x0000u }, 
    { 0x0658u, 0x0000u }, { 0x0659u, 0x0000u }, { 0x065Au, 0x0000u }, { 0x065Bu, 0x0000u }, 
    { 0x065Cu, 0x0000u }, { 0x065Du, 0x0000u }, { 0x065Eu, 0x0000u }, { 0x065Fu, 0x0000u }, 
    { 0x0660u, 0x0000u }, { 0x0661u, 0x0000u }, { 0x0662u, 0x0000u }, { 0x0663u, 0x0000u }, 
    { 0x0664u, 0x0000u }, { 0x0665u, 0x0000u }, { 0x0666u, 0x0000u }, { 0x0667u, 0x0000u }, 
    { 0x0668u, 0x0000u }, { 0x0669u, 0x0000u }, { 0x066Au, 0x0000u }, { 0x066Bu, 0x0000u }, 
    { 0x066Cu, 0x0000u }, { 0x066Du, 0x0000u }, { 0x066Eu, 0x0000u }, { 0x066Fu, 0x0000u }, 
    { 0x0670u, 0x0000u }, { 0x0671u, 0x0000u }, { 0x0672u, 0x0000u }, { 0x0673u, 0x0000u }, 
    { 0x0674u, 0x0000u }, { 0x0627u, 0x0674u }, { 0x0648u, 0x0674u }, { 0x06C7u, 0x0674u }, 
    { 0x064Au, 0x0674u }, { 0x0679u, 0x0000u }, { 0x067Au, 0x0000u }, { 0x067Bu, 0x0000u }, 
    { 0x067Cu, 0x0000u }, { 0x067Du, 0x0000u }, { 0x067Eu, 0x0000u }, { 0x067Fu, 0x0000u }, 
    { 0x0680u, 0x0000u }, { 0x0681u, 0x0000u }, { 0x0682u, 0x0000u }, { 0x0683u, 0x0000u }, 
    { 0x0684u, 0x0000u }, { 0x0685u, 0x0000u }, { 0x0686u, 0x0000u }, { 0x0687u, 0x0000u }, 
    { 0x0688u, 0x0000u }, { 0x0689u, 0x0000u }, { 0x068Au, 0x0000u }, { 0x068Bu, 0x0000u }, 
    { 0x068Cu, 0x0000u }, { 0x068Du, 0x0000u }, { 0x068Eu, 0x0000u }, { 0x068Fu, 0x0000u }, 
    { 0x0690u, 0x0000u }, { 0x0691u, 0x0000u }, { 0x0692u, 0x0000u }, { 0x0693u, 0x0000u }, 
    { 0x0694u, 0x0000u }, { 0x0695u, 0x0000u }, { 0x0696u, 0x0000u }, { 0x0697u, 0x0000u }, 
    { 0x0698u, 0x0000u }, { 0x0699u, 0x0000u }, { 0x069Au, 0x0000u }, { 0x069Bu, 0x0000u }, 
    { 0x069Cu, 0x0000u }, { 0x069Du, 0x0000u }, { 0x069Eu, 0x0000u }, { 0x069Fu, 0x0000u }, 
    { 0x06A0u, 0x0000u }, { 0x06A1u, 0x0000u }, { 0x06A2u, 0x0000u }, { 0x06A3u, 0x0000u }, 
    { 0x06A4u, 0x0000u }, { 0x06A5u, 0x0000u }, { 0x06A6u, 0x0000u }, { 0x06A7u, 0x0000u }, 
    { 0x06A8u, 0x0000u }, { 0x06A9u, 0x0000u }, { 0x06AAu, 0x0000u }, { 0x06ABu, 0x0000u }, 
    { 0x06ACu, 0x0000u }, { 0x06ADu, 0x0000u }, { 0x06AEu, 0x0000u }, { 0x06AFu, 0x0000u }, 
    { 0x06B0u, 0x0000u }, { 0x06B1u, 0x0000u }, { 0x06B2u, 0x0000u }, { 0x06B3u, 0x0000u }, 
    { 0x06B4u, 0x0000u }, { 0x06B5u, 0x0000u }, { 0x06B6u, 0x0000u }, { 0x06B7u, 0x0000u }, 
    { 0x06B8u, 0x0000u }, { 0x06B9u, 0x0000u }, { 0x06BAu, 0x0000u }, { 0x06BBu, 0x0000u }, 
    { 0x06BCu, 0x0000u }, { 0x06BDu, 0x0000u }, { 0x06BEu, 0x0000u }, { 0x06BFu, 0x0000u }, 
    { 0x06D5u, 0x0654u }, { 0x06C1u, 0x0000u }, { 0x06C1u, 0x0654u }, { 0x06C3u, 0x0000u }, 
    { 0x06C4u, 0x0000u }, { 0x06C5u, 0x0000u }, { 0x06C6u, 0x0000u }, { 0x06C7u, 0x0000u }, 
    { 0x06C8u, 0x0000u }, { 0x06C9u, 0x0000u }, { 0x06CAu, 0x0000u }, { 0x06CBu, 0x0000u }, 
    { 0x06CCu, 0x0000u }, { 0x06CDu, 0x0000u }, { 0x06CEu, 0x0000u }, { 0x06CFu, 0x0000u }, 
    { 0x06D0u, 0x0000u }, { 0x06D1u, 0x0000u }, { 0x06D2u, 0x0000u }, { 0x06D2u, 0x0654u }, 
    { 0x06D4u, 0x0000u }, { 0x06D5u, 0x0000u }, { 0x06D6u, 0x0000u }, { 0x06D7u, 0x0000u }, 
    { 0x06D8u, 0x0000u }, { 0x06D9u, 0x0000u }, { 0x06DAu, 0x0000u }, { 0x06DBu, 0x0000u }, 
    { 0x06DCu, 0x0000u }, { 0x06DDu, 0x0000u }, { 0x06DEu, 0x0000u }, { 0x06DFu, 0x0000u }, 
    { 0x06E0u, 0x0000u }, { 0x06E1u, 0x0000u }, { 0x06E2u, 0x0000u }, { 0x06E3u, 0x0000u }, 
    { 0x06E4u, 0x0000u }, { 0x06E5u, 0x0000u }, { 0x06E6u, 0x0000u }, { 0x06E7u, 0x0000u }, 
    { 0x06E8u, 0x0000u }, { 0x06E9u, 0x0000u }, { 0x06EAu, 0x0000u }, { 0x06EBu, 0x0000u }, 
    { 0x06ECu, 0x0000u }, { 0x06EDu, 0x0000u }, { 0x06EEu, 0x0000u }, { 0x06EFu, 0x0000u }, 
    { 0x06F0u, 0x0000u }, { 0x06F1u, 0x0000u }, { 0x06F2u, 0x0000u }, { 0x06F3u, 0x0000u }, 
    { 0x06F4u, 0x0000u }, { 0x06F5u, 0x0000u }, { 0x06F6u, 0x0000u }, { 0x06F7u, 0x0000u }, 
    { 0x06F8u, 0x0000u }, { 0x06F9u, 0x0000u }, { 0x06FAu, 0x0000u }, { 0x06FBu, 0x0000u }, 
    { 0x06FCu, 0x0000u }, { 0x06FDu, 0x0000u }, { 0x06FEu, 0x0000u }, { 0x06FFu, 0x0000u }
  };

  const unsigned char Arabic600::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
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
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_CM, 
    LB_CM, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

    const std::bitset<256> Arabic600::m_Other_Alphabetic(std::string("0000000000000000000000011001111000011111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111000000000000000000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<256> Arabic600::m_Diacritic(std::string("0000000000000000000111000110000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Arabic600);
