/*$Id: FE30-FE4F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:05:09 +0200.
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

  class CJK_Compatibility_FormsFE30 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    CJK_Compatibility_FormsFE30() {
      m_first_letter = 0xFE30;
      m_last_letter  = 0xFE4F;
      // m_version="3.1" // Not yet supported!

    }


    ~CJK_Compatibility_FormsFE30() {
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
      return "CJK Compatibility Forms";
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
      return Babylon::Gen_Cat(CJK_Compatibility_FormsFE30::_cat[uc - m_first_letter]);
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
      return Babylon::Char_Decomp(CJK_Compatibility_FormsFE30::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(1);
      us[0] = CJK_Compatibility_FormsFE30::m_decompStr[uc - m_first_letter];
      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(CJK_Compatibility_FormsFE30::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_W);
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
      return m_Dash.test(uc - m_first_letter);
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
    CJK_Compatibility_FormsFE30(const CJK_Compatibility_FormsFE30 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<32> m_is_defined;
    static const unsigned char _cat[32];
    static const unsigned char _decomp[32];
    static const UCS4 m_decompStr[32];
    static const unsigned char m_lb[32];
    static const std::bitset<32> m_Dash;

  }; // class CJK_Compatibility_FormsFE30

    const std::bitset<32> CJK_Compatibility_FormsFE30::m_is_defined(std::string("11111110000111111111111111111111"));

  const unsigned char CJK_Compatibility_FormsFE30::_cat[] = {
    CAT_Po, CAT_Pd, CAT_Pd, CAT_Pc, CAT_Pc, CAT_Ps, CAT_Pe, CAT_Ps, 
    CAT_Pe, CAT_Ps, CAT_Pe, CAT_Ps, CAT_Pe, CAT_Ps, CAT_Pe, CAT_Ps, 
    CAT_Pe, CAT_Ps, CAT_Pe, CAT_Ps, CAT_Pe, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Pc, CAT_Pc, CAT_Pc
  };

  const unsigned char CJK_Compatibility_FormsFE30::_decomp[] = {
    DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, 
    DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, 
    DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_VERTICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT
  };

  const UCS4 CJK_Compatibility_FormsFE30::m_decompStr[] = {
    0x2025u, 0x2014u, 0x2013u, 0x005Fu, 
    0x005Fu, 0x0028u, 0x0029u, 0x007Bu, 
    0x007Du, 0x3014u, 0x3015u, 0x3010u, 
    0x3011u, 0x300Au, 0x300Bu, 0x3008u, 
    0x3009u, 0x300Cu, 0x300Du, 0x300Eu, 
    0x300Fu, 0xFE45u, 0xFE46u, 0xFE47u, 
    0xFE48u, 0x203Eu, 0x203Eu, 0x203Eu, 
    0x203Eu, 0x005Fu, 0x005Fu, 0x005Fu
  };

  const unsigned char CJK_Compatibility_FormsFE30::m_lb[] = {
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_OP, LB_CL, LB_OP, 
    LB_CL, LB_OP, LB_CL, LB_OP, LB_CL, LB_OP, LB_CL, LB_OP, 
    LB_CL, LB_OP, LB_CL, LB_OP, LB_CL, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID
  };

    const std::bitset<32> CJK_Compatibility_FormsFE30::m_Dash(std::string("00000000000000000000000000000110"));

}; // namespace Babylon

dload(Babylon::CJK_Compatibility_FormsFE30);
