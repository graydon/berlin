/*$Id: E80-EFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:24 +0200.
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

  class LaoE80 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    LaoE80() {
      m_first_letter = 0xE80;
      m_last_letter  = 0xEFF;
      // m_version="3.1" // Not yet supported!

    }


    ~LaoE80() {
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
      return "Lao";
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
      case 0x0ED0u:
        return 0;
        break;
      case 0x0ED1u:
        return 1;
        break;
      case 0x0ED2u:
        return 2;
        break;
      case 0x0ED3u:
        return 3;
        break;
      case 0x0ED4u:
        return 4;
        break;
      case 0x0ED5u:
        return 5;
        break;
      case 0x0ED6u:
        return 6;
        break;
      case 0x0ED7u:
        return 7;
        break;
      case 0x0ED8u:
        return 8;
        break;
      case 0x0ED9u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0ED0u:
      case 0x0ED1u:
      case 0x0ED2u:
      case 0x0ED3u:
      case 0x0ED4u:
      case 0x0ED5u:
      case 0x0ED6u:
      case 0x0ED7u:
      case 0x0ED8u:
      case 0x0ED9u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0ED0u:
        return 0;
        break;
      case 0x0ED1u:
        return 1;
        break;
      case 0x0ED2u:
        return 2;
        break;
      case 0x0ED3u:
        return 3;
        break;
      case 0x0ED4u:
        return 4;
        break;
      case 0x0ED5u:
        return 5;
        break;
      case 0x0ED6u:
        return 6;
        break;
      case 0x0ED7u:
        return 7;
        break;
      case 0x0ED8u:
        return 8;
        break;
      case 0x0ED9u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0ED0u:
      case 0x0ED1u:
      case 0x0ED2u:
      case 0x0ED3u:
      case 0x0ED4u:
      case 0x0ED5u:
      case 0x0ED6u:
      case 0x0ED7u:
      case 0x0ED8u:
      case 0x0ED9u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0ED0u:
        return 0.000000;
        break;
      case 0x0ED1u:
        return 1.000000;
        break;
      case 0x0ED2u:
        return 2.000000;
        break;
      case 0x0ED3u:
        return 3.000000;
        break;
      case 0x0ED4u:
        return 4.000000;
        break;
      case 0x0ED5u:
        return 5.000000;
        break;
      case 0x0ED6u:
        return 6.000000;
        break;
      case 0x0ED7u:
        return 7.000000;
        break;
      case 0x0ED8u:
        return 8.000000;
        break;
      case 0x0ED9u:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0ED0u:
      case 0x0ED1u:
      case 0x0ED2u:
      case 0x0ED3u:
      case 0x0ED4u:
      case 0x0ED5u:
      case 0x0ED6u:
      case 0x0ED7u:
      case 0x0ED8u:
      case 0x0ED9u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(LaoE80::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(LaoE80::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(LaoE80::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(LaoE80::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = LaoE80::m_decompStr[uc - m_first_letter][0];
      us[1] = LaoE80::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(LaoE80::m_lb[uc - m_first_letter]);
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
    LaoE80(const LaoE80 &) {}

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
    static const std::bitset<128> m_Other_Alphabetic;
    static const std::bitset<128> m_Diacritic;

  }; // class LaoE80

    const std::bitset<128> LaoE80::m_is_defined(std::string("00000000000000000000000000000000001100111111111100111111010111110011101111111111111011001010111011111110111100000010010110010110"));

  const unsigned char LaoE80::_cat[] = {
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lm, CAT_Lo, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo
  };

  const unsigned char LaoE80::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    118, 118, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    122, 122, 122, 122, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char LaoE80::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const unsigned char LaoE80::_decomp[] = {
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 LaoE80::m_decompStr[][2] = {
    { 0x0E80u, 0x0000u }, { 0x0E81u, 0x0000u }, { 0x0E82u, 0x0000u }, { 0x0E83u, 0x0000u }, 
    { 0x0E84u, 0x0000u }, { 0x0E85u, 0x0000u }, { 0x0E86u, 0x0000u }, { 0x0E87u, 0x0000u }, 
    { 0x0E88u, 0x0000u }, { 0x0E89u, 0x0000u }, { 0x0E8Au, 0x0000u }, { 0x0E8Bu, 0x0000u }, 
    { 0x0E8Cu, 0x0000u }, { 0x0E8Du, 0x0000u }, { 0x0E8Eu, 0x0000u }, { 0x0E8Fu, 0x0000u }, 
    { 0x0E90u, 0x0000u }, { 0x0E91u, 0x0000u }, { 0x0E92u, 0x0000u }, { 0x0E93u, 0x0000u }, 
    { 0x0E94u, 0x0000u }, { 0x0E95u, 0x0000u }, { 0x0E96u, 0x0000u }, { 0x0E97u, 0x0000u }, 
    { 0x0E98u, 0x0000u }, { 0x0E99u, 0x0000u }, { 0x0E9Au, 0x0000u }, { 0x0E9Bu, 0x0000u }, 
    { 0x0E9Cu, 0x0000u }, { 0x0E9Du, 0x0000u }, { 0x0E9Eu, 0x0000u }, { 0x0E9Fu, 0x0000u }, 
    { 0x0EA0u, 0x0000u }, { 0x0EA1u, 0x0000u }, { 0x0EA2u, 0x0000u }, { 0x0EA3u, 0x0000u }, 
    { 0x0EA4u, 0x0000u }, { 0x0EA5u, 0x0000u }, { 0x0EA6u, 0x0000u }, { 0x0EA7u, 0x0000u }, 
    { 0x0EA8u, 0x0000u }, { 0x0EA9u, 0x0000u }, { 0x0EAAu, 0x0000u }, { 0x0EABu, 0x0000u }, 
    { 0x0EACu, 0x0000u }, { 0x0EADu, 0x0000u }, { 0x0EAEu, 0x0000u }, { 0x0EAFu, 0x0000u }, 
    { 0x0EB0u, 0x0000u }, { 0x0EB1u, 0x0000u }, { 0x0EB2u, 0x0000u }, { 0x0ECDu, 0x0EB2u }, 
    { 0x0EB4u, 0x0000u }, { 0x0EB5u, 0x0000u }, { 0x0EB6u, 0x0000u }, { 0x0EB7u, 0x0000u }, 
    { 0x0EB8u, 0x0000u }, { 0x0EB9u, 0x0000u }, { 0x0EBAu, 0x0000u }, { 0x0EBBu, 0x0000u }, 
    { 0x0EBCu, 0x0000u }, { 0x0EBDu, 0x0000u }, { 0x0EBEu, 0x0000u }, { 0x0EBFu, 0x0000u }, 
    { 0x0EC0u, 0x0000u }, { 0x0EC1u, 0x0000u }, { 0x0EC2u, 0x0000u }, { 0x0EC3u, 0x0000u }, 
    { 0x0EC4u, 0x0000u }, { 0x0EC5u, 0x0000u }, { 0x0EC6u, 0x0000u }, { 0x0EC7u, 0x0000u }, 
    { 0x0EC8u, 0x0000u }, { 0x0EC9u, 0x0000u }, { 0x0ECAu, 0x0000u }, { 0x0ECBu, 0x0000u }, 
    { 0x0ECCu, 0x0000u }, { 0x0ECDu, 0x0000u }, { 0x0ECEu, 0x0000u }, { 0x0ECFu, 0x0000u }, 
    { 0x0ED0u, 0x0000u }, { 0x0ED1u, 0x0000u }, { 0x0ED2u, 0x0000u }, { 0x0ED3u, 0x0000u }, 
    { 0x0ED4u, 0x0000u }, { 0x0ED5u, 0x0000u }, { 0x0ED6u, 0x0000u }, { 0x0ED7u, 0x0000u }, 
    { 0x0ED8u, 0x0000u }, { 0x0ED9u, 0x0000u }, { 0x0EDAu, 0x0000u }, { 0x0EDBu, 0x0000u }, 
    { 0x0EABu, 0x0E99u }, { 0x0EABu, 0x0EA1u }, { 0x0EDEu, 0x0000u }, { 0x0EDFu, 0x0000u }, 
    { 0x0EE0u, 0x0000u }, { 0x0EE1u, 0x0000u }, { 0x0EE2u, 0x0000u }, { 0x0EE3u, 0x0000u }, 
    { 0x0EE4u, 0x0000u }, { 0x0EE5u, 0x0000u }, { 0x0EE6u, 0x0000u }, { 0x0EE7u, 0x0000u }, 
    { 0x0EE8u, 0x0000u }, { 0x0EE9u, 0x0000u }, { 0x0EEAu, 0x0000u }, { 0x0EEBu, 0x0000u }, 
    { 0x0EECu, 0x0000u }, { 0x0EEDu, 0x0000u }, { 0x0EEEu, 0x0000u }, { 0x0EEFu, 0x0000u }, 
    { 0x0EF0u, 0x0000u }, { 0x0EF1u, 0x0000u }, { 0x0EF2u, 0x0000u }, { 0x0EF3u, 0x0000u }, 
    { 0x0EF4u, 0x0000u }, { 0x0EF5u, 0x0000u }, { 0x0EF6u, 0x0000u }, { 0x0EF7u, 0x0000u }, 
    { 0x0EF8u, 0x0000u }, { 0x0EF9u, 0x0000u }, { 0x0EFAu, 0x0000u }, { 0x0EFBu, 0x0000u }, 
    { 0x0EFCu, 0x0000u }, { 0x0EFDu, 0x0000u }, { 0x0EFEu, 0x0000u }, { 0x0EFFu, 0x0000u }
  };

  const unsigned char LaoE80::m_lb[] = {
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_CM, LB_SA, LB_SA, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_SA, LB_CM, LB_CM, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_SA, LB_SA, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA
  };

    const std::bitset<128> LaoE80::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000000000000000000000000000001101111110000000000000000000000000000000000000000000000000000"));

    const std::bitset<128> LaoE80::m_Diacritic(std::string("00000000000000000000000000000000000000000000000000011111000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::LaoE80);
