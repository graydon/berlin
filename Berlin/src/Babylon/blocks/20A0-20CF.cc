/*$Id: 20A0-20CF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:42 +0200.
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

  class Currency_Symbols20A0 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Currency_Symbols20A0() {
      m_first_letter = 0x20A0;
      m_last_letter  = 0x20CF;
      // m_version="3.1" // Not yet supported!

    }


    ~Currency_Symbols20A0() {
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
      return "Currency Symbols";
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
      return Babylon::Gen_Cat(CAT_Sc);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(BIDIR_ET);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Currency_Symbols20A0::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Currency_Symbols20A0::m_decompStr[uc - m_first_letter][0];
      us[1] = Currency_Symbols20A0::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Currency_Symbols20A0::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Currency_Symbols20A0::m_ea[uc - m_first_letter]);
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
    Currency_Symbols20A0(const Currency_Symbols20A0 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<48> m_is_defined;
    static const unsigned char _decomp[48];
    static const UCS2 m_decompStr[48][2];
    static const unsigned char m_lb[48];
    static const unsigned char m_ea[48];

  }; // class Currency_Symbols20A0

    const std::bitset<48> Currency_Symbols20A0::m_is_defined(std::string("000000000000000000000000000000001111111111111111"));

  const unsigned char Currency_Symbols20A0::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Currency_Symbols20A0::m_decompStr[][2] = {
    { 0x20A0u, 0x0000u }, { 0x20A1u, 0x0000u }, { 0x20A2u, 0x0000u }, { 0x20A3u, 0x0000u }, 
    { 0x20A4u, 0x0000u }, { 0x20A5u, 0x0000u }, { 0x20A6u, 0x0000u }, { 0x20A7u, 0x0000u }, 
    { 0x0052u, 0x0073u }, { 0x20A9u, 0x0000u }, { 0x20AAu, 0x0000u }, { 0x20ABu, 0x0000u }, 
    { 0x20ACu, 0x0000u }, { 0x20ADu, 0x0000u }, { 0x20AEu, 0x0000u }, { 0x20AFu, 0x0000u }, 
    { 0x20B0u, 0x0000u }, { 0x20B1u, 0x0000u }, { 0x20B2u, 0x0000u }, { 0x20B3u, 0x0000u }, 
    { 0x20B4u, 0x0000u }, { 0x20B5u, 0x0000u }, { 0x20B6u, 0x0000u }, { 0x20B7u, 0x0000u }, 
    { 0x20B8u, 0x0000u }, { 0x20B9u, 0x0000u }, { 0x20BAu, 0x0000u }, { 0x20BBu, 0x0000u }, 
    { 0x20BCu, 0x0000u }, { 0x20BDu, 0x0000u }, { 0x20BEu, 0x0000u }, { 0x20BFu, 0x0000u }, 
    { 0x20C0u, 0x0000u }, { 0x20C1u, 0x0000u }, { 0x20C2u, 0x0000u }, { 0x20C3u, 0x0000u }, 
    { 0x20C4u, 0x0000u }, { 0x20C5u, 0x0000u }, { 0x20C6u, 0x0000u }, { 0x20C7u, 0x0000u }, 
    { 0x20C8u, 0x0000u }, { 0x20C9u, 0x0000u }, { 0x20CAu, 0x0000u }, { 0x20CBu, 0x0000u }, 
    { 0x20CCu, 0x0000u }, { 0x20CDu, 0x0000u }, { 0x20CEu, 0x0000u }, { 0x20CFu, 0x0000u }
  };

  const unsigned char Currency_Symbols20A0::m_lb[] = {
    LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PO, 
    LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, 
    LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, 
    LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, 
    LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, 
    LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR, LB_PR
  };

  const unsigned char Currency_Symbols20A0::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_H, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

}; // namespace Babylon

dload(Babylon::Currency_Symbols20A0);
