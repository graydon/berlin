/*$Id: 980-9FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:48 +0200.
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

  class Bengali980 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Bengali980() {
      m_first_letter = 0x980;
      m_last_letter  = 0x9FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x000009A1, 0x000009BC)] = 0x09DC;
      m_composeMap[make_pair(0x000009A2, 0x000009BC)] = 0x09DD;
      m_composeMap[make_pair(0x000009AF, 0x000009BC)] = 0x09DF;
      m_composeMap[make_pair(0x000009C7, 0x000009BE)] = 0x09CB;
      m_composeMap[make_pair(0x000009C7, 0x000009D7)] = 0x09CC;

    }


    ~Bengali980() {
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
      return "Bengali";
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
      case 0x09E6u:
        return 0;
        break;
      case 0x09E7u:
        return 1;
        break;
      case 0x09E8u:
        return 2;
        break;
      case 0x09E9u:
        return 3;
        break;
      case 0x09EAu:
        return 4;
        break;
      case 0x09EBu:
        return 5;
        break;
      case 0x09ECu:
        return 6;
        break;
      case 0x09EDu:
        return 7;
        break;
      case 0x09EEu:
        return 8;
        break;
      case 0x09EFu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x09E6u:
      case 0x09E7u:
      case 0x09E8u:
      case 0x09E9u:
      case 0x09EAu:
      case 0x09EBu:
      case 0x09ECu:
      case 0x09EDu:
      case 0x09EEu:
      case 0x09EFu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x09E6u:
        return 0;
        break;
      case 0x09E7u:
        return 1;
        break;
      case 0x09E8u:
        return 2;
        break;
      case 0x09E9u:
        return 3;
        break;
      case 0x09EAu:
        return 4;
        break;
      case 0x09EBu:
        return 5;
        break;
      case 0x09ECu:
        return 6;
        break;
      case 0x09EDu:
        return 7;
        break;
      case 0x09EEu:
        return 8;
        break;
      case 0x09EFu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x09E6u:
      case 0x09E7u:
      case 0x09E8u:
      case 0x09E9u:
      case 0x09EAu:
      case 0x09EBu:
      case 0x09ECu:
      case 0x09EDu:
      case 0x09EEu:
      case 0x09EFu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x09E6u:
        return 0.000000;
        break;
      case 0x09E7u:
        return 1.000000;
        break;
      case 0x09E8u:
        return 2.000000;
        break;
      case 0x09E9u:
        return 3.000000;
        break;
      case 0x09EAu:
        return 4.000000;
        break;
      case 0x09EBu:
        return 5.000000;
        break;
      case 0x09ECu:
        return 6.000000;
        break;
      case 0x09EDu:
        return 7.000000;
        break;
      case 0x09EEu:
        return 8.000000;
        break;
      case 0x09EFu:
        return 9.000000;
        break;
      case 0x09F4u:
        return 1.000000;
        break;
      case 0x09F5u:
        return 2.000000;
        break;
      case 0x09F6u:
        return 3.000000;
        break;
      case 0x09F7u:
        return 4.000000;
        break;
      case 0x09F9u:
        return 16.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x09E6u:
      case 0x09E7u:
      case 0x09E8u:
      case 0x09E9u:
      case 0x09EAu:
      case 0x09EBu:
      case 0x09ECu:
      case 0x09EDu:
      case 0x09EEu:
      case 0x09EFu:
      case 0x09F4u:
      case 0x09F5u:
      case 0x09F6u:
      case 0x09F7u:
      case 0x09F9u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Bengali980::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Bengali980::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Bengali980::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Bengali980::m_decompStr[uc - m_first_letter][0];
      us[1] = Bengali980::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Bengali980::m_lb[uc - m_first_letter]);
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
    Bengali980(const Bengali980 &) {}

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

  }; // class Bengali980

    const std::bitset<128> Bengali980::m_is_defined(std::string("00000111111111111111111111001111101100001000000000111001100111111101001111000101111111011111111111111111111110011001111111101110"));

  const unsigned char Bengali980::_cat[] = {
    CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Lo, CAT_Lo, CAT_Sc, CAT_Sc, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_So, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn
  };

  const unsigned char Bengali980::_comb_cl[] = {
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

  const unsigned char Bengali980::m_bidir[] = {
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_ET, BIDIR_ET, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM
  };

  const UCS2 Bengali980::m_decompStr[][2] = {
    { 0x0980u, 0x0000u }, { 0x0981u, 0x0000u }, { 0x0982u, 0x0000u }, { 0x0983u, 0x0000u }, 
    { 0x0984u, 0x0000u }, { 0x0985u, 0x0000u }, { 0x0986u, 0x0000u }, { 0x0987u, 0x0000u }, 
    { 0x0988u, 0x0000u }, { 0x0989u, 0x0000u }, { 0x098Au, 0x0000u }, { 0x098Bu, 0x0000u }, 
    { 0x098Cu, 0x0000u }, { 0x098Du, 0x0000u }, { 0x098Eu, 0x0000u }, { 0x098Fu, 0x0000u }, 
    { 0x0990u, 0x0000u }, { 0x0991u, 0x0000u }, { 0x0992u, 0x0000u }, { 0x0993u, 0x0000u }, 
    { 0x0994u, 0x0000u }, { 0x0995u, 0x0000u }, { 0x0996u, 0x0000u }, { 0x0997u, 0x0000u }, 
    { 0x0998u, 0x0000u }, { 0x0999u, 0x0000u }, { 0x099Au, 0x0000u }, { 0x099Bu, 0x0000u }, 
    { 0x099Cu, 0x0000u }, { 0x099Du, 0x0000u }, { 0x099Eu, 0x0000u }, { 0x099Fu, 0x0000u }, 
    { 0x09A0u, 0x0000u }, { 0x09A1u, 0x0000u }, { 0x09A2u, 0x0000u }, { 0x09A3u, 0x0000u }, 
    { 0x09A4u, 0x0000u }, { 0x09A5u, 0x0000u }, { 0x09A6u, 0x0000u }, { 0x09A7u, 0x0000u }, 
    { 0x09A8u, 0x0000u }, { 0x09A9u, 0x0000u }, { 0x09AAu, 0x0000u }, { 0x09ABu, 0x0000u }, 
    { 0x09ACu, 0x0000u }, { 0x09ADu, 0x0000u }, { 0x09AEu, 0x0000u }, { 0x09AFu, 0x0000u }, 
    { 0x09B0u, 0x0000u }, { 0x09B1u, 0x0000u }, { 0x09B2u, 0x0000u }, { 0x09B3u, 0x0000u }, 
    { 0x09B4u, 0x0000u }, { 0x09B5u, 0x0000u }, { 0x09B6u, 0x0000u }, { 0x09B7u, 0x0000u }, 
    { 0x09B8u, 0x0000u }, { 0x09B9u, 0x0000u }, { 0x09BAu, 0x0000u }, { 0x09BBu, 0x0000u }, 
    { 0x09BCu, 0x0000u }, { 0x09BDu, 0x0000u }, { 0x09BEu, 0x0000u }, { 0x09BFu, 0x0000u }, 
    { 0x09C0u, 0x0000u }, { 0x09C1u, 0x0000u }, { 0x09C2u, 0x0000u }, { 0x09C3u, 0x0000u }, 
    { 0x09C4u, 0x0000u }, { 0x09C5u, 0x0000u }, { 0x09C6u, 0x0000u }, { 0x09C7u, 0x0000u }, 
    { 0x09C8u, 0x0000u }, { 0x09C9u, 0x0000u }, { 0x09CAu, 0x0000u }, { 0x09C7u, 0x09BEu }, 
    { 0x09C7u, 0x09D7u }, { 0x09CDu, 0x0000u }, { 0x09CEu, 0x0000u }, { 0x09CFu, 0x0000u }, 
    { 0x09D0u, 0x0000u }, { 0x09D1u, 0x0000u }, { 0x09D2u, 0x0000u }, { 0x09D3u, 0x0000u }, 
    { 0x09D4u, 0x0000u }, { 0x09D5u, 0x0000u }, { 0x09D6u, 0x0000u }, { 0x09D7u, 0x0000u }, 
    { 0x09D8u, 0x0000u }, { 0x09D9u, 0x0000u }, { 0x09DAu, 0x0000u }, { 0x09DBu, 0x0000u }, 
    { 0x09A1u, 0x09BCu }, { 0x09A2u, 0x09BCu }, { 0x09DEu, 0x0000u }, { 0x09AFu, 0x09BCu }, 
    { 0x09E0u, 0x0000u }, { 0x09E1u, 0x0000u }, { 0x09E2u, 0x0000u }, { 0x09E3u, 0x0000u }, 
    { 0x09E4u, 0x0000u }, { 0x09E5u, 0x0000u }, { 0x09E6u, 0x0000u }, { 0x09E7u, 0x0000u }, 
    { 0x09E8u, 0x0000u }, { 0x09E9u, 0x0000u }, { 0x09EAu, 0x0000u }, { 0x09EBu, 0x0000u }, 
    { 0x09ECu, 0x0000u }, { 0x09EDu, 0x0000u }, { 0x09EEu, 0x0000u }, { 0x09EFu, 0x0000u }, 
    { 0x09F0u, 0x0000u }, { 0x09F1u, 0x0000u }, { 0x09F2u, 0x0000u }, { 0x09F3u, 0x0000u }, 
    { 0x09F4u, 0x0000u }, { 0x09F5u, 0x0000u }, { 0x09F6u, 0x0000u }, { 0x09F7u, 0x0000u }, 
    { 0x09F8u, 0x0000u }, { 0x09F9u, 0x0000u }, { 0x09FAu, 0x0000u }, { 0x09FBu, 0x0000u }, 
    { 0x09FCu, 0x0000u }, { 0x09FDu, 0x0000u }, { 0x09FEu, 0x0000u }, { 0x09FFu, 0x0000u }
  };

  const unsigned char Bengali980::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_AL, 
    LB_AL, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_CM, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_AL, LB_AL, LB_PR, LB_PR, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> Bengali980::m_Other_Alphabetic(std::string("00000000000000000000000000001100000000000000000000011001100111111100000000000000000000000000000000000000000000000000000000001100"));

}; // namespace Babylon

dload(Babylon::Bengali980);
