/*$Id: C80-CFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:11 +0200.
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

  class KannadaC80 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    KannadaC80() {
      m_first_letter = 0xC80;
      m_last_letter  = 0xCFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000CBF, 0x00000CD5)] = 0x0CC0;
      m_composeMap[make_pair(0x00000CC6, 0x00000CC2)] = 0x0CCA;
      m_composeMap[make_pair(0x00000CC6, 0x00000CD5)] = 0x0CC7;
      m_composeMap[make_pair(0x00000CC6, 0x00000CD6)] = 0x0CC8;
      m_composeMap[make_pair(0x00000CCA, 0x00000CD5)] = 0x0CCB;

    }


    ~KannadaC80() {
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
      return "Kannada";
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
      case 0x0CE6u:
        return 0;
        break;
      case 0x0CE7u:
        return 1;
        break;
      case 0x0CE8u:
        return 2;
        break;
      case 0x0CE9u:
        return 3;
        break;
      case 0x0CEAu:
        return 4;
        break;
      case 0x0CEBu:
        return 5;
        break;
      case 0x0CECu:
        return 6;
        break;
      case 0x0CEDu:
        return 7;
        break;
      case 0x0CEEu:
        return 8;
        break;
      case 0x0CEFu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0CE6u:
      case 0x0CE7u:
      case 0x0CE8u:
      case 0x0CE9u:
      case 0x0CEAu:
      case 0x0CEBu:
      case 0x0CECu:
      case 0x0CEDu:
      case 0x0CEEu:
      case 0x0CEFu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0CE6u:
        return 0;
        break;
      case 0x0CE7u:
        return 1;
        break;
      case 0x0CE8u:
        return 2;
        break;
      case 0x0CE9u:
        return 3;
        break;
      case 0x0CEAu:
        return 4;
        break;
      case 0x0CEBu:
        return 5;
        break;
      case 0x0CECu:
        return 6;
        break;
      case 0x0CEDu:
        return 7;
        break;
      case 0x0CEEu:
        return 8;
        break;
      case 0x0CEFu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0CE6u:
      case 0x0CE7u:
      case 0x0CE8u:
      case 0x0CE9u:
      case 0x0CEAu:
      case 0x0CEBu:
      case 0x0CECu:
      case 0x0CEDu:
      case 0x0CEEu:
      case 0x0CEFu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0CE6u:
        return 0.000000;
        break;
      case 0x0CE7u:
        return 1.000000;
        break;
      case 0x0CE8u:
        return 2.000000;
        break;
      case 0x0CE9u:
        return 3.000000;
        break;
      case 0x0CEAu:
        return 4.000000;
        break;
      case 0x0CEBu:
        return 5.000000;
        break;
      case 0x0CECu:
        return 6.000000;
        break;
      case 0x0CEDu:
        return 7.000000;
        break;
      case 0x0CEEu:
        return 8.000000;
        break;
      case 0x0CEFu:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0CE6u:
      case 0x0CE7u:
      case 0x0CE8u:
      case 0x0CE9u:
      case 0x0CEAu:
      case 0x0CEBu:
      case 0x0CECu:
      case 0x0CEDu:
      case 0x0CEEu:
      case 0x0CEFu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(KannadaC80::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(KannadaC80::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(KannadaC80::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = KannadaC80::m_decompStr[uc - m_first_letter][0];
      us[1] = KannadaC80::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(KannadaC80::m_lb[uc - m_first_letter]);
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
    KannadaC80(const KannadaC80 &) {}

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

  }; // class KannadaC80

    const std::bitset<128> KannadaC80::m_is_defined(std::string("00000000000000001111111111000011010000000110000000111101110111111100001111101111111111011111111111111111111111011101111111101100"));

  const unsigned char KannadaC80::_cat[] = {
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Lo, CAT_Mc, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc
  };

  const unsigned char KannadaC80::_comb_cl[] = {
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

  const unsigned char KannadaC80::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS2 KannadaC80::m_decompStr[][2] = {
    { 0x0C80u, 0x0000u }, { 0x0C81u, 0x0000u }, { 0x0C82u, 0x0000u }, { 0x0C83u, 0x0000u }, 
    { 0x0C84u, 0x0000u }, { 0x0C85u, 0x0000u }, { 0x0C86u, 0x0000u }, { 0x0C87u, 0x0000u }, 
    { 0x0C88u, 0x0000u }, { 0x0C89u, 0x0000u }, { 0x0C8Au, 0x0000u }, { 0x0C8Bu, 0x0000u }, 
    { 0x0C8Cu, 0x0000u }, { 0x0C8Du, 0x0000u }, { 0x0C8Eu, 0x0000u }, { 0x0C8Fu, 0x0000u }, 
    { 0x0C90u, 0x0000u }, { 0x0C91u, 0x0000u }, { 0x0C92u, 0x0000u }, { 0x0C93u, 0x0000u }, 
    { 0x0C94u, 0x0000u }, { 0x0C95u, 0x0000u }, { 0x0C96u, 0x0000u }, { 0x0C97u, 0x0000u }, 
    { 0x0C98u, 0x0000u }, { 0x0C99u, 0x0000u }, { 0x0C9Au, 0x0000u }, { 0x0C9Bu, 0x0000u }, 
    { 0x0C9Cu, 0x0000u }, { 0x0C9Du, 0x0000u }, { 0x0C9Eu, 0x0000u }, { 0x0C9Fu, 0x0000u }, 
    { 0x0CA0u, 0x0000u }, { 0x0CA1u, 0x0000u }, { 0x0CA2u, 0x0000u }, { 0x0CA3u, 0x0000u }, 
    { 0x0CA4u, 0x0000u }, { 0x0CA5u, 0x0000u }, { 0x0CA6u, 0x0000u }, { 0x0CA7u, 0x0000u }, 
    { 0x0CA8u, 0x0000u }, { 0x0CA9u, 0x0000u }, { 0x0CAAu, 0x0000u }, { 0x0CABu, 0x0000u }, 
    { 0x0CACu, 0x0000u }, { 0x0CADu, 0x0000u }, { 0x0CAEu, 0x0000u }, { 0x0CAFu, 0x0000u }, 
    { 0x0CB0u, 0x0000u }, { 0x0CB1u, 0x0000u }, { 0x0CB2u, 0x0000u }, { 0x0CB3u, 0x0000u }, 
    { 0x0CB4u, 0x0000u }, { 0x0CB5u, 0x0000u }, { 0x0CB6u, 0x0000u }, { 0x0CB7u, 0x0000u }, 
    { 0x0CB8u, 0x0000u }, { 0x0CB9u, 0x0000u }, { 0x0CBAu, 0x0000u }, { 0x0CBBu, 0x0000u }, 
    { 0x0CBCu, 0x0000u }, { 0x0CBDu, 0x0000u }, { 0x0CBEu, 0x0000u }, { 0x0CBFu, 0x0000u }, 
    { 0x0CBFu, 0x0CD5u }, { 0x0CC1u, 0x0000u }, { 0x0CC2u, 0x0000u }, { 0x0CC3u, 0x0000u }, 
    { 0x0CC4u, 0x0000u }, { 0x0CC5u, 0x0000u }, { 0x0CC6u, 0x0000u }, { 0x0CC6u, 0x0CD5u }, 
    { 0x0CC6u, 0x0CD6u }, { 0x0CC9u, 0x0000u }, { 0x0CC6u, 0x0CC2u }, { 0x0CCAu, 0x0CD5u }, 
    { 0x0CCCu, 0x0000u }, { 0x0CCDu, 0x0000u }, { 0x0CCEu, 0x0000u }, { 0x0CCFu, 0x0000u }, 
    { 0x0CD0u, 0x0000u }, { 0x0CD1u, 0x0000u }, { 0x0CD2u, 0x0000u }, { 0x0CD3u, 0x0000u }, 
    { 0x0CD4u, 0x0000u }, { 0x0CD5u, 0x0000u }, { 0x0CD6u, 0x0000u }, { 0x0CD7u, 0x0000u }, 
    { 0x0CD8u, 0x0000u }, { 0x0CD9u, 0x0000u }, { 0x0CDAu, 0x0000u }, { 0x0CDBu, 0x0000u }, 
    { 0x0CDCu, 0x0000u }, { 0x0CDDu, 0x0000u }, { 0x0CDEu, 0x0000u }, { 0x0CDFu, 0x0000u }, 
    { 0x0CE0u, 0x0000u }, { 0x0CE1u, 0x0000u }, { 0x0CE2u, 0x0000u }, { 0x0CE3u, 0x0000u }, 
    { 0x0CE4u, 0x0000u }, { 0x0CE5u, 0x0000u }, { 0x0CE6u, 0x0000u }, { 0x0CE7u, 0x0000u }, 
    { 0x0CE8u, 0x0000u }, { 0x0CE9u, 0x0000u }, { 0x0CEAu, 0x0000u }, { 0x0CEBu, 0x0000u }, 
    { 0x0CECu, 0x0000u }, { 0x0CEDu, 0x0000u }, { 0x0CEEu, 0x0000u }, { 0x0CEFu, 0x0000u }, 
    { 0x0CF0u, 0x0000u }, { 0x0CF1u, 0x0000u }, { 0x0CF2u, 0x0000u }, { 0x0CF3u, 0x0000u }, 
    { 0x0CF4u, 0x0000u }, { 0x0CF5u, 0x0000u }, { 0x0CF6u, 0x0000u }, { 0x0CF7u, 0x0000u }, 
    { 0x0CF8u, 0x0000u }, { 0x0CF9u, 0x0000u }, { 0x0CFAu, 0x0000u }, { 0x0CFBu, 0x0000u }, 
    { 0x0CFCu, 0x0000u }, { 0x0CFDu, 0x0000u }, { 0x0CFEu, 0x0000u }, { 0x0CFFu, 0x0000u }
  };

  const unsigned char KannadaC80::m_lb[] = {
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
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_CM, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> KannadaC80::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000110000000001101100111110000000000000000000000000000000000000000000000000000000000001100"));

}; // namespace Babylon

dload(Babylon::KannadaC80);
