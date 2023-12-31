/*$Id: 2190-21FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:49 +0200.
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

  class Arrows2190 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Arrows2190() {
      m_first_letter = 0x2190;
      m_last_letter  = 0x21FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00002190, 0x00000338)] = 0x219A;
      m_composeMap[make_pair(0x00002192, 0x00000338)] = 0x219B;
      m_composeMap[make_pair(0x00002194, 0x00000338)] = 0x21AE;
      m_composeMap[make_pair(0x000021D0, 0x00000338)] = 0x21CD;
      m_composeMap[make_pair(0x000021D2, 0x00000338)] = 0x21CF;
      m_composeMap[make_pair(0x000021D4, 0x00000338)] = 0x21CE;

    }


    ~Arrows2190() {
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
      return "Arrows";
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
      return Babylon::Gen_Cat(Arrows2190::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(BIDIR_ON);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Arrows2190::m_decompStr[uc - m_first_letter][0];
      us[1] = Arrows2190::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Arrows2190::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Arrows2190::m_ea[uc - m_first_letter]);
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
    Arrows2190(const Arrows2190 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<112> m_is_defined;
    static const unsigned char _cat[112];
    static const UCS2 m_decompStr[112][2];
    static const unsigned char m_lb[112];
    static const unsigned char m_ea[112];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Arrows2190

    const std::bitset<112> Arrows2190::m_is_defined(std::string("0000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char Arrows2190::_cat[] = {
    CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_Sm, CAT_Sm, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_Sm, CAT_So, CAT_So, CAT_Sm, CAT_So, CAT_So, CAT_Sm, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_Sm, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_Sm, CAT_Sm, 
    CAT_So, CAT_So, CAT_Sm, CAT_So, CAT_Sm, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, 
    CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm
  };

  const UCS2 Arrows2190::m_decompStr[][2] = {
    { 0x2190u, 0x0000u }, { 0x2191u, 0x0000u }, { 0x2192u, 0x0000u }, { 0x2193u, 0x0000u }, 
    { 0x2194u, 0x0000u }, { 0x2195u, 0x0000u }, { 0x2196u, 0x0000u }, { 0x2197u, 0x0000u }, 
    { 0x2198u, 0x0000u }, { 0x2199u, 0x0000u }, { 0x2190u, 0x0338u }, { 0x2192u, 0x0338u }, 
    { 0x219Cu, 0x0000u }, { 0x219Du, 0x0000u }, { 0x219Eu, 0x0000u }, { 0x219Fu, 0x0000u }, 
    { 0x21A0u, 0x0000u }, { 0x21A1u, 0x0000u }, { 0x21A2u, 0x0000u }, { 0x21A3u, 0x0000u }, 
    { 0x21A4u, 0x0000u }, { 0x21A5u, 0x0000u }, { 0x21A6u, 0x0000u }, { 0x21A7u, 0x0000u }, 
    { 0x21A8u, 0x0000u }, { 0x21A9u, 0x0000u }, { 0x21AAu, 0x0000u }, { 0x21ABu, 0x0000u }, 
    { 0x21ACu, 0x0000u }, { 0x21ADu, 0x0000u }, { 0x2194u, 0x0338u }, { 0x21AFu, 0x0000u }, 
    { 0x21B0u, 0x0000u }, { 0x21B1u, 0x0000u }, { 0x21B2u, 0x0000u }, { 0x21B3u, 0x0000u }, 
    { 0x21B4u, 0x0000u }, { 0x21B5u, 0x0000u }, { 0x21B6u, 0x0000u }, { 0x21B7u, 0x0000u }, 
    { 0x21B8u, 0x0000u }, { 0x21B9u, 0x0000u }, { 0x21BAu, 0x0000u }, { 0x21BBu, 0x0000u }, 
    { 0x21BCu, 0x0000u }, { 0x21BDu, 0x0000u }, { 0x21BEu, 0x0000u }, { 0x21BFu, 0x0000u }, 
    { 0x21C0u, 0x0000u }, { 0x21C1u, 0x0000u }, { 0x21C2u, 0x0000u }, { 0x21C3u, 0x0000u }, 
    { 0x21C4u, 0x0000u }, { 0x21C5u, 0x0000u }, { 0x21C6u, 0x0000u }, { 0x21C7u, 0x0000u }, 
    { 0x21C8u, 0x0000u }, { 0x21C9u, 0x0000u }, { 0x21CAu, 0x0000u }, { 0x21CBu, 0x0000u }, 
    { 0x21CCu, 0x0000u }, { 0x21D0u, 0x0338u }, { 0x21D4u, 0x0338u }, { 0x21D2u, 0x0338u }, 
    { 0x21D0u, 0x0000u }, { 0x21D1u, 0x0000u }, { 0x21D2u, 0x0000u }, { 0x21D3u, 0x0000u }, 
    { 0x21D4u, 0x0000u }, { 0x21D5u, 0x0000u }, { 0x21D6u, 0x0000u }, { 0x21D7u, 0x0000u }, 
    { 0x21D8u, 0x0000u }, { 0x21D9u, 0x0000u }, { 0x21DAu, 0x0000u }, { 0x21DBu, 0x0000u }, 
    { 0x21DCu, 0x0000u }, { 0x21DDu, 0x0000u }, { 0x21DEu, 0x0000u }, { 0x21DFu, 0x0000u }, 
    { 0x21E0u, 0x0000u }, { 0x21E1u, 0x0000u }, { 0x21E2u, 0x0000u }, { 0x21E3u, 0x0000u }, 
    { 0x21E4u, 0x0000u }, { 0x21E5u, 0x0000u }, { 0x21E6u, 0x0000u }, { 0x21E7u, 0x0000u }, 
    { 0x21E8u, 0x0000u }, { 0x21E9u, 0x0000u }, { 0x21EAu, 0x0000u }, { 0x21EBu, 0x0000u }, 
    { 0x21ECu, 0x0000u }, { 0x21EDu, 0x0000u }, { 0x21EEu, 0x0000u }, { 0x21EFu, 0x0000u }, 
    { 0x21F0u, 0x0000u }, { 0x21F1u, 0x0000u }, { 0x21F2u, 0x0000u }, { 0x21F3u, 0x0000u }, 
    { 0x21F4u, 0x0000u }, { 0x21F5u, 0x0000u }, { 0x21F6u, 0x0000u }, { 0x21F7u, 0x0000u }, 
    { 0x21F8u, 0x0000u }, { 0x21F9u, 0x0000u }, { 0x21FAu, 0x0000u }, { 0x21FBu, 0x0000u }, 
    { 0x21FCu, 0x0000u }, { 0x21FDu, 0x0000u }, { 0x21FEu, 0x0000u }, { 0x21FFu, 0x0000u }
  };

  const unsigned char Arrows2190::m_lb[] = {
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI
  };

  const unsigned char Arrows2190::m_ea[] = {
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A
  };

}; // namespace Babylon

dload(Babylon::Arrows2190);
