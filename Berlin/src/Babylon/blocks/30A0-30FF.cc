/*$Id: 30A0-30FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:49:49 +0200.
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

  class Katakana30A0 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Katakana30A0() {
      m_first_letter = 0x30A0;
      m_last_letter  = 0x30FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x000030A6, 0x00003099)] = 0x30F4;
      m_composeMap[make_pair(0x000030AB, 0x00003099)] = 0x30AC;
      m_composeMap[make_pair(0x000030AD, 0x00003099)] = 0x30AE;
      m_composeMap[make_pair(0x000030AF, 0x00003099)] = 0x30B0;
      m_composeMap[make_pair(0x000030B1, 0x00003099)] = 0x30B2;
      m_composeMap[make_pair(0x000030B3, 0x00003099)] = 0x30B4;
      m_composeMap[make_pair(0x000030B5, 0x00003099)] = 0x30B6;
      m_composeMap[make_pair(0x000030B7, 0x00003099)] = 0x30B8;
      m_composeMap[make_pair(0x000030B9, 0x00003099)] = 0x30BA;
      m_composeMap[make_pair(0x000030BB, 0x00003099)] = 0x30BC;
      m_composeMap[make_pair(0x000030BD, 0x00003099)] = 0x30BE;
      m_composeMap[make_pair(0x000030BF, 0x00003099)] = 0x30C0;
      m_composeMap[make_pair(0x000030C1, 0x00003099)] = 0x30C2;
      m_composeMap[make_pair(0x000030C4, 0x00003099)] = 0x30C5;
      m_composeMap[make_pair(0x000030C6, 0x00003099)] = 0x30C7;
      m_composeMap[make_pair(0x000030C8, 0x00003099)] = 0x30C9;
      m_composeMap[make_pair(0x000030CF, 0x00003099)] = 0x30D0;
      m_composeMap[make_pair(0x000030CF, 0x0000309A)] = 0x30D1;
      m_composeMap[make_pair(0x000030D2, 0x00003099)] = 0x30D3;
      m_composeMap[make_pair(0x000030D2, 0x0000309A)] = 0x30D4;
      m_composeMap[make_pair(0x000030D5, 0x00003099)] = 0x30D6;
      m_composeMap[make_pair(0x000030D5, 0x0000309A)] = 0x30D7;
      m_composeMap[make_pair(0x000030D8, 0x00003099)] = 0x30D9;
      m_composeMap[make_pair(0x000030D8, 0x0000309A)] = 0x30DA;
      m_composeMap[make_pair(0x000030DB, 0x00003099)] = 0x30DC;
      m_composeMap[make_pair(0x000030DB, 0x0000309A)] = 0x30DD;
      m_composeMap[make_pair(0x000030EF, 0x00003099)] = 0x30F7;
      m_composeMap[make_pair(0x000030F0, 0x00003099)] = 0x30F8;
      m_composeMap[make_pair(0x000030F1, 0x00003099)] = 0x30F9;
      m_composeMap[make_pair(0x000030F2, 0x00003099)] = 0x30FA;
      m_composeMap[make_pair(0x000030FD, 0x00003099)] = 0x30FE;

    }


    ~Katakana30A0() {
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
      return "Katakana";
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
      return 0;
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      return 0;
    }

    int digit_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Digit(const UCS4 uc) const {
      return 0;
    }

    float numeric_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Numeric(const UCS4 uc) const {
      return 0;
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Katakana30A0::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Katakana30A0::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Katakana30A0::m_decompStr[uc - m_first_letter][0];
      us[1] = Katakana30A0::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Katakana30A0::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_W);
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
      return 0;
    }

    bool is_Ideographic(const UCS4 uc) const {
      return 0;
    }

    bool is_Diacritic(const UCS4 uc) const {
      return 0;
    }

    bool is_Extender(const UCS4 uc) const {
      return m_Extender.test(uc - m_first_letter);
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
    Katakana30A0(const Katakana30A0 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<96> m_is_defined;
    static const unsigned char _cat[96];
    static const unsigned char m_bidir[96];
    static const UCS4 m_decompStr[96][2];
    static const unsigned char m_lb[96];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<96> m_Extender;

  }; // class Katakana30A0

    const std::bitset<96> Katakana30A0::m_is_defined(std::string("011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110"));

  const unsigned char Katakana30A0::_cat[] = {
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
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Pc, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lo
  };

  const unsigned char Katakana30A0::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS4 Katakana30A0::m_decompStr[][2] = {
    { 0x30A0u, 0x0000u }, { 0x30A1u, 0x0000u }, { 0x30A2u, 0x0000u }, { 0x30A3u, 0x0000u }, 
    { 0x30A4u, 0x0000u }, { 0x30A5u, 0x0000u }, { 0x30A6u, 0x0000u }, { 0x30A7u, 0x0000u }, 
    { 0x30A8u, 0x0000u }, { 0x30A9u, 0x0000u }, { 0x30AAu, 0x0000u }, { 0x30ABu, 0x0000u }, 
    { 0x30ABu, 0x3099u }, { 0x30ADu, 0x0000u }, { 0x30ADu, 0x3099u }, { 0x30AFu, 0x0000u }, 
    { 0x30AFu, 0x3099u }, { 0x30B1u, 0x0000u }, { 0x30B1u, 0x3099u }, { 0x30B3u, 0x0000u }, 
    { 0x30B3u, 0x3099u }, { 0x30B5u, 0x0000u }, { 0x30B5u, 0x3099u }, { 0x30B7u, 0x0000u }, 
    { 0x30B7u, 0x3099u }, { 0x30B9u, 0x0000u }, { 0x30B9u, 0x3099u }, { 0x30BBu, 0x0000u }, 
    { 0x30BBu, 0x3099u }, { 0x30BDu, 0x0000u }, { 0x30BDu, 0x3099u }, { 0x30BFu, 0x0000u }, 
    { 0x30BFu, 0x3099u }, { 0x30C1u, 0x0000u }, { 0x30C1u, 0x3099u }, { 0x30C3u, 0x0000u }, 
    { 0x30C4u, 0x0000u }, { 0x30C4u, 0x3099u }, { 0x30C6u, 0x0000u }, { 0x30C6u, 0x3099u }, 
    { 0x30C8u, 0x0000u }, { 0x30C8u, 0x3099u }, { 0x30CAu, 0x0000u }, { 0x30CBu, 0x0000u }, 
    { 0x30CCu, 0x0000u }, { 0x30CDu, 0x0000u }, { 0x30CEu, 0x0000u }, { 0x30CFu, 0x0000u }, 
    { 0x30CFu, 0x3099u }, { 0x30CFu, 0x309Au }, { 0x30D2u, 0x0000u }, { 0x30D2u, 0x3099u }, 
    { 0x30D2u, 0x309Au }, { 0x30D5u, 0x0000u }, { 0x30D5u, 0x3099u }, { 0x30D5u, 0x309Au }, 
    { 0x30D8u, 0x0000u }, { 0x30D8u, 0x3099u }, { 0x30D8u, 0x309Au }, { 0x30DBu, 0x0000u }, 
    { 0x30DBu, 0x3099u }, { 0x30DBu, 0x309Au }, { 0x30DEu, 0x0000u }, { 0x30DFu, 0x0000u }, 
    { 0x30E0u, 0x0000u }, { 0x30E1u, 0x0000u }, { 0x30E2u, 0x0000u }, { 0x30E3u, 0x0000u }, 
    { 0x30E4u, 0x0000u }, { 0x30E5u, 0x0000u }, { 0x30E6u, 0x0000u }, { 0x30E7u, 0x0000u }, 
    { 0x30E8u, 0x0000u }, { 0x30E9u, 0x0000u }, { 0x30EAu, 0x0000u }, { 0x30EBu, 0x0000u }, 
    { 0x30ECu, 0x0000u }, { 0x30EDu, 0x0000u }, { 0x30EEu, 0x0000u }, { 0x30EFu, 0x0000u }, 
    { 0x30F0u, 0x0000u }, { 0x30F1u, 0x0000u }, { 0x30F2u, 0x0000u }, { 0x30F3u, 0x0000u }, 
    { 0x30A6u, 0x3099u }, { 0x30F5u, 0x0000u }, { 0x30F6u, 0x0000u }, { 0x30EFu, 0x3099u }, 
    { 0x30F0u, 0x3099u }, { 0x30F1u, 0x3099u }, { 0x30F2u, 0x3099u }, { 0x30FBu, 0x0000u }, 
    { 0x30FCu, 0x0000u }, { 0x30FDu, 0x0000u }, { 0x30FDu, 0x3099u }, { 0x30FFu, 0x0000u }
  };

  const unsigned char Katakana30A0::m_lb[] = {
    LB_NS, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS, 
    LB_ID, LB_NS, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_NS, LB_NS, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS
  };

    const std::bitset<96> Katakana30A0::m_Extender(std::string("011100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Katakana30A0);
