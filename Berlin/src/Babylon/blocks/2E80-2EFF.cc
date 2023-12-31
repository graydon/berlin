/*$Id: 2E80-2EFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:49:36 +0200.
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

  class CJK_Radicals_Supplement2E80 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    CJK_Radicals_Supplement2E80() {
      m_first_letter = 0x2E80;
      m_last_letter  = 0x2EFF;
      // m_version="3.1" // Not yet supported!

    }


    ~CJK_Radicals_Supplement2E80() {
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
      return "CJK Radicals Supplement";
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
      return Babylon::Gen_Cat(CAT_So);
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
      return Babylon::Char_Decomp(CJK_Radicals_Supplement2E80::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(1);
      us[0] = CJK_Radicals_Supplement2E80::m_decompStr[uc - m_first_letter];
      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(LB_ID);
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
    CJK_Radicals_Supplement2E80(const CJK_Radicals_Supplement2E80 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<128> m_is_defined;
    static const unsigned char _decomp[128];
    static const UCS4 m_decompStr[128];

  }; // class CJK_Radicals_Supplement2E80

    const std::bitset<128> CJK_Radicals_Supplement2E80::m_is_defined(std::string("00000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111011111111111111111111111111"));

  const unsigned char CJK_Radicals_Supplement2E80::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, 
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS4 CJK_Radicals_Supplement2E80::m_decompStr[] = {
    0x2E80u, 0x2E81u, 0x2E82u, 0x2E83u, 
    0x2E84u, 0x2E85u, 0x2E86u, 0x2E87u, 
    0x2E88u, 0x2E89u, 0x2E8Au, 0x2E8Bu, 
    0x2E8Cu, 0x2E8Du, 0x2E8Eu, 0x2E8Fu, 
    0x2E90u, 0x2E91u, 0x2E92u, 0x2E93u, 
    0x2E94u, 0x2E95u, 0x2E96u, 0x2E97u, 
    0x2E98u, 0x2E99u, 0x2E9Au, 0x2E9Bu, 
    0x2E9Cu, 0x2E9Du, 0x2E9Eu, 0x6BCDu, 
    0x2EA0u, 0x2EA1u, 0x2EA2u, 0x2EA3u, 
    0x2EA4u, 0x2EA5u, 0x2EA6u, 0x2EA7u, 
    0x2EA8u, 0x2EA9u, 0x2EAAu, 0x2EABu, 
    0x2EACu, 0x2EADu, 0x2EAEu, 0x2EAFu, 
    0x2EB0u, 0x2EB1u, 0x2EB2u, 0x2EB3u, 
    0x2EB4u, 0x2EB5u, 0x2EB6u, 0x2EB7u, 
    0x2EB8u, 0x2EB9u, 0x2EBAu, 0x2EBBu, 
    0x2EBCu, 0x2EBDu, 0x2EBEu, 0x2EBFu, 
    0x2EC0u, 0x2EC1u, 0x2EC2u, 0x2EC3u, 
    0x2EC4u, 0x2EC5u, 0x2EC6u, 0x2EC7u, 
    0x2EC8u, 0x2EC9u, 0x2ECAu, 0x2ECBu, 
    0x2ECCu, 0x2ECDu, 0x2ECEu, 0x2ECFu, 
    0x2ED0u, 0x2ED1u, 0x2ED2u, 0x2ED3u, 
    0x2ED4u, 0x2ED5u, 0x2ED6u, 0x2ED7u, 
    0x2ED8u, 0x2ED9u, 0x2EDAu, 0x2EDBu, 
    0x2EDCu, 0x2EDDu, 0x2EDEu, 0x2EDFu, 
    0x2EE0u, 0x2EE1u, 0x2EE2u, 0x2EE3u, 
    0x2EE4u, 0x2EE5u, 0x2EE6u, 0x2EE7u, 
    0x2EE8u, 0x2EE9u, 0x2EEAu, 0x2EEBu, 
    0x2EECu, 0x2EEDu, 0x2EEEu, 0x2EEFu, 
    0x2EF0u, 0x2EF1u, 0x2EF2u, 0x9F9Fu, 
    0x2EF4u, 0x2EF5u, 0x2EF6u, 0x2EF7u, 
    0x2EF8u, 0x2EF9u, 0x2EFAu, 0x2EFBu, 
    0x2EFCu, 0x2EFDu, 0x2EFEu, 0x2EFFu
  };

}; // namespace Babylon

dload(Babylon::CJK_Radicals_Supplement2E80);
