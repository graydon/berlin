/*$Id: FE50-FE6F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:05:10 +0200.
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

  class Small_Form_VariantsFE50 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Small_Form_VariantsFE50() {
      m_first_letter = 0xFE50;
      m_last_letter  = 0xFE6F;
      // m_version="3.1" // Not yet supported!

    }


    ~Small_Form_VariantsFE50() {
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
      return "Small Form Variants";
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
      return Babylon::Gen_Cat(Small_Form_VariantsFE50::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Small_Form_VariantsFE50::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Small_Form_VariantsFE50::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(1);
      us[0] = Small_Form_VariantsFE50::m_decompStr[uc - m_first_letter];
      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(Small_Form_VariantsFE50::m_lb[uc - m_first_letter]);
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
      return m_Terminal_Punctuation.test(uc - m_first_letter);
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
    Small_Form_VariantsFE50(const Small_Form_VariantsFE50 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<32> m_is_defined;
    static const unsigned char _cat[32];
    static const unsigned char m_bidir[32];
    static const unsigned char _decomp[32];
    static const UCS4 m_decompStr[32];
    static const unsigned char m_lb[32];
    static const std::bitset<32> m_Terminal_Punctuation;

  }; // class Small_Form_VariantsFE50

    const std::bitset<32> Small_Form_VariantsFE50::m_is_defined(std::string("00001111011111111111111111110111"));

  const unsigned char Small_Form_VariantsFE50::_cat[] = {
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Pd, CAT_Ps, CAT_Pe, CAT_Ps, CAT_Pe, CAT_Ps, CAT_Pe, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Sm, CAT_Pd, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Po, 
    CAT_Po, CAT_Sc, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po
  };

  const unsigned char Small_Form_VariantsFE50::m_bidir[] = {
    BIDIR_CS, BIDIR_ON, BIDIR_CS, BIDIR_CS, BIDIR_ON, BIDIR_CS, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ET, 
    BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_CS, 
    BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_CS, BIDIR_CS, BIDIR_CS, BIDIR_CS
  };

  const unsigned char Small_Form_VariantsFE50::_decomp[] = {
    DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_CANONICAL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, 
    DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, 
    DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_CANONICAL, 
    DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_SMALL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS4 Small_Form_VariantsFE50::m_decompStr[] = {
    0x002Cu, 0x3001u, 0x002Eu, 0xFE53u, 
    0x003Bu, 0x003Au, 0x003Fu, 0x0021u, 
    0x2014u, 0x0028u, 0x0029u, 0x007Bu, 
    0x007Du, 0x3014u, 0x3015u, 0x0023u, 
    0x0026u, 0x002Au, 0x002Bu, 0x002Du, 
    0x003Cu, 0x003Eu, 0x003Du, 0xFE67u, 
    0x005Cu, 0x0024u, 0x0025u, 0x0040u, 
    0xFE6Cu, 0xFE6Du, 0xFE6Eu, 0xFE6Fu
  };

  const unsigned char Small_Form_VariantsFE50::m_lb[] = {
    LB_CL, LB_ID, LB_CL, LB_CL, LB_NS, LB_NS, LB_EX, LB_EX, 
    LB_ID, LB_OP, LB_CL, LB_OP, LB_CL, LB_OP, LB_CL, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_CL, 
    LB_ID, LB_PR, LB_PO, LB_ID, LB_CL, LB_CL, LB_CL, LB_CL
  };

    const std::bitset<32> Small_Form_VariantsFE50::m_Terminal_Punctuation(std::string("00000000000000000000000011110111"));

}; // namespace Babylon

dload(Babylon::Small_Form_VariantsFE50);
