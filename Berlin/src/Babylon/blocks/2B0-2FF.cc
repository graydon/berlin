/*$Id: 2B0-2FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:12 +0200.
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

  class Spacing_Modifier_Letters2B0 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Spacing_Modifier_Letters2B0() {
      m_first_letter = 0x2B0;
      m_last_letter  = 0x2FF;
      // m_version="3.1" // Not yet supported!

    }


    ~Spacing_Modifier_Letters2B0() {
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
      return "Spacing Modifier Letters";
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
      return Babylon::Gen_Cat(Spacing_Modifier_Letters2B0::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Spacing_Modifier_Letters2B0::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Spacing_Modifier_Letters2B0::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Spacing_Modifier_Letters2B0::m_decompStr[uc - m_first_letter][0];
      us[1] = Spacing_Modifier_Letters2B0::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Spacing_Modifier_Letters2B0::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Spacing_Modifier_Letters2B0::m_ea[uc - m_first_letter]);
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
      return m_Diacritic.test(uc - m_first_letter);
    }

    bool is_Extender(const UCS4 uc) const {
      return m_Extender.test(uc - m_first_letter);
    }

    bool is_Other_Lowercase(const UCS4 uc) const {
      return m_Other_Lowercase.test(uc - m_first_letter);
    }

    bool is_Other_Uppercase(const UCS4 uc) const {
      return 0;
    }

    bool is_Noncharacter_Code_Point(const UCS4 uc) const {
      return 0;
    }


  private:
    // functions
    Spacing_Modifier_Letters2B0(const Spacing_Modifier_Letters2B0 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<80> m_is_defined;
    static const unsigned char _cat[80];
    static const unsigned char m_bidir[80];
    static const unsigned char _decomp[80];
    static const UCS2 m_decompStr[80][2];
    static const unsigned char m_lb[80];
    static const unsigned char m_ea[80];
    static const std::bitset<80> m_Diacritic;
    static const std::bitset<80> m_Extender;
    static const std::bitset<80> m_Other_Lowercase;

  }; // class Spacing_Modifier_Letters2B0

    const std::bitset<80> Spacing_Modifier_Letters2B0::m_is_defined(std::string("00000000000000000111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char Spacing_Modifier_Letters2B0::_cat[] = {
    CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, 
    CAT_Lm, CAT_Sk, CAT_Sk, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, 
    CAT_Lm, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Lm, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Lm, CAT_Lm, 
    CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, 
    CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm
  };

  const unsigned char Spacing_Modifier_Letters2B0::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const unsigned char Spacing_Modifier_Letters2B0::_decomp[] = {
    DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, 
    DECOMP_SUPER, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Spacing_Modifier_Letters2B0::m_decompStr[][2] = {
    { 0x0068u, 0x0000u }, { 0x0266u, 0x0000u }, { 0x006Au, 0x0000u }, { 0x0072u, 0x0000u }, 
    { 0x0279u, 0x0000u }, { 0x027Bu, 0x0000u }, { 0x0281u, 0x0000u }, { 0x0077u, 0x0000u }, 
    { 0x0079u, 0x0000u }, { 0x02B9u, 0x0000u }, { 0x02BAu, 0x0000u }, { 0x02BBu, 0x0000u }, 
    { 0x02BCu, 0x0000u }, { 0x02BDu, 0x0000u }, { 0x02BEu, 0x0000u }, { 0x02BFu, 0x0000u }, 
    { 0x02C0u, 0x0000u }, { 0x02C1u, 0x0000u }, { 0x02C2u, 0x0000u }, { 0x02C3u, 0x0000u }, 
    { 0x02C4u, 0x0000u }, { 0x02C5u, 0x0000u }, { 0x02C6u, 0x0000u }, { 0x02C7u, 0x0000u }, 
    { 0x02C8u, 0x0000u }, { 0x02C9u, 0x0000u }, { 0x02CAu, 0x0000u }, { 0x02CBu, 0x0000u }, 
    { 0x02CCu, 0x0000u }, { 0x02CDu, 0x0000u }, { 0x02CEu, 0x0000u }, { 0x02CFu, 0x0000u }, 
    { 0x02D0u, 0x0000u }, { 0x02D1u, 0x0000u }, { 0x02D2u, 0x0000u }, { 0x02D3u, 0x0000u }, 
    { 0x02D4u, 0x0000u }, { 0x02D5u, 0x0000u }, { 0x02D6u, 0x0000u }, { 0x02D7u, 0x0000u }, 
    { 0x0020u, 0x0306u }, { 0x0020u, 0x0307u }, { 0x0020u, 0x030Au }, { 0x0020u, 0x0328u }, 
    { 0x0020u, 0x0303u }, { 0x0020u, 0x030Bu }, { 0x02DEu, 0x0000u }, { 0x02DFu, 0x0000u }, 
    { 0x0263u, 0x0000u }, { 0x006Cu, 0x0000u }, { 0x0073u, 0x0000u }, { 0x0078u, 0x0000u }, 
    { 0x0295u, 0x0000u }, { 0x02E5u, 0x0000u }, { 0x02E6u, 0x0000u }, { 0x02E7u, 0x0000u }, 
    { 0x02E8u, 0x0000u }, { 0x02E9u, 0x0000u }, { 0x02EAu, 0x0000u }, { 0x02EBu, 0x0000u }, 
    { 0x02ECu, 0x0000u }, { 0x02EDu, 0x0000u }, { 0x02EEu, 0x0000u }, { 0x02EFu, 0x0000u }, 
    { 0x02F0u, 0x0000u }, { 0x02F1u, 0x0000u }, { 0x02F2u, 0x0000u }, { 0x02F3u, 0x0000u }, 
    { 0x02F4u, 0x0000u }, { 0x02F5u, 0x0000u }, { 0x02F6u, 0x0000u }, { 0x02F7u, 0x0000u }, 
    { 0x02F8u, 0x0000u }, { 0x02F9u, 0x0000u }, { 0x02FAu, 0x0000u }, { 0x02FBu, 0x0000u }, 
    { 0x02FCu, 0x0000u }, { 0x02FDu, 0x0000u }, { 0x02FEu, 0x0000u }, { 0x02FFu, 0x0000u }
  };

  const unsigned char Spacing_Modifier_Letters2B0::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_BB, LB_AI, LB_AI, LB_AI, LB_BB, LB_AI, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Spacing_Modifier_Letters2B0::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

    const std::bitset<80> Spacing_Modifier_Letters2B0::m_Diacritic(std::string("00000000000000000011111111111111111111111111111111111111111111111111111111111111"));

    const std::bitset<80> Spacing_Modifier_Letters2B0::m_Extender(std::string("00000000000000000000000000000000000000000000001100000000000000000000000000000000"));

    const std::bitset<80> Spacing_Modifier_Letters2B0::m_Other_Lowercase(std::string("00000000000000000000000000011111000000000000000000000000000000110000000111111111"));

}; // namespace Babylon

dload(Babylon::Spacing_Modifier_Letters2B0);
