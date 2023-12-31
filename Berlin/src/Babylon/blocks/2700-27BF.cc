/*$Id: 2700-27BF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:49:26 +0200.
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

  class Dingbats2700 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Dingbats2700() {
      m_first_letter = 0x2700;
      m_last_letter  = 0x27BF;
      // m_version="3.1" // Not yet supported!

    }


    ~Dingbats2700() {
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
      return "Dingbats";
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
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x2776u:
        return 1;
        break;
      case 0x2777u:
        return 2;
        break;
      case 0x2778u:
        return 3;
        break;
      case 0x2779u:
        return 4;
        break;
      case 0x277Au:
        return 5;
        break;
      case 0x277Bu:
        return 6;
        break;
      case 0x277Cu:
        return 7;
        break;
      case 0x277Du:
        return 8;
        break;
      case 0x277Eu:
        return 9;
        break;
      case 0x2780u:
        return 1;
        break;
      case 0x2781u:
        return 2;
        break;
      case 0x2782u:
        return 3;
        break;
      case 0x2783u:
        return 4;
        break;
      case 0x2784u:
        return 5;
        break;
      case 0x2785u:
        return 6;
        break;
      case 0x2786u:
        return 7;
        break;
      case 0x2787u:
        return 8;
        break;
      case 0x2788u:
        return 9;
        break;
      case 0x278Au:
        return 1;
        break;
      case 0x278Bu:
        return 2;
        break;
      case 0x278Cu:
        return 3;
        break;
      case 0x278Du:
        return 4;
        break;
      case 0x278Eu:
        return 5;
        break;
      case 0x278Fu:
        return 6;
        break;
      case 0x2790u:
        return 7;
        break;
      case 0x2791u:
        return 8;
        break;
      case 0x2792u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x2776u:
      case 0x2777u:
      case 0x2778u:
      case 0x2779u:
      case 0x277Au:
      case 0x277Bu:
      case 0x277Cu:
      case 0x277Du:
      case 0x277Eu:
      case 0x2780u:
      case 0x2781u:
      case 0x2782u:
      case 0x2783u:
      case 0x2784u:
      case 0x2785u:
      case 0x2786u:
      case 0x2787u:
      case 0x2788u:
      case 0x278Au:
      case 0x278Bu:
      case 0x278Cu:
      case 0x278Du:
      case 0x278Eu:
      case 0x278Fu:
      case 0x2790u:
      case 0x2791u:
      case 0x2792u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x2776u:
        return 1.000000;
        break;
      case 0x2777u:
        return 2.000000;
        break;
      case 0x2778u:
        return 3.000000;
        break;
      case 0x2779u:
        return 4.000000;
        break;
      case 0x277Au:
        return 5.000000;
        break;
      case 0x277Bu:
        return 6.000000;
        break;
      case 0x277Cu:
        return 7.000000;
        break;
      case 0x277Du:
        return 8.000000;
        break;
      case 0x277Eu:
        return 9.000000;
        break;
      case 0x277Fu:
        return 10.000000;
        break;
      case 0x2780u:
        return 1.000000;
        break;
      case 0x2781u:
        return 2.000000;
        break;
      case 0x2782u:
        return 3.000000;
        break;
      case 0x2783u:
        return 4.000000;
        break;
      case 0x2784u:
        return 5.000000;
        break;
      case 0x2785u:
        return 6.000000;
        break;
      case 0x2786u:
        return 7.000000;
        break;
      case 0x2787u:
        return 8.000000;
        break;
      case 0x2788u:
        return 9.000000;
        break;
      case 0x2789u:
        return 10.000000;
        break;
      case 0x278Au:
        return 1.000000;
        break;
      case 0x278Bu:
        return 2.000000;
        break;
      case 0x278Cu:
        return 3.000000;
        break;
      case 0x278Du:
        return 4.000000;
        break;
      case 0x278Eu:
        return 5.000000;
        break;
      case 0x278Fu:
        return 6.000000;
        break;
      case 0x2790u:
        return 7.000000;
        break;
      case 0x2791u:
        return 8.000000;
        break;
      case 0x2792u:
        return 9.000000;
        break;
      case 0x2793u:
        return 10.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x2776u:
      case 0x2777u:
      case 0x2778u:
      case 0x2779u:
      case 0x277Au:
      case 0x277Bu:
      case 0x277Cu:
      case 0x277Du:
      case 0x277Eu:
      case 0x277Fu:
      case 0x2780u:
      case 0x2781u:
      case 0x2782u:
      case 0x2783u:
      case 0x2784u:
      case 0x2785u:
      case 0x2786u:
      case 0x2787u:
      case 0x2788u:
      case 0x2789u:
      case 0x278Au:
      case 0x278Bu:
      case 0x278Cu:
      case 0x278Du:
      case 0x278Eu:
      case 0x278Fu:
      case 0x2790u:
      case 0x2791u:
      case 0x2792u:
      case 0x2793u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Dingbats2700::_cat[uc - m_first_letter]);
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
      UTF32_string us;
      us.resize(1); us[0] = uc;
      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(LB_AL);
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
    Dingbats2700(const Dingbats2700 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<192> m_is_defined;
    static const unsigned char _cat[192];

  }; // class Dingbats2700

    const std::bitset<192> Dingbats2700::m_is_defined(std::string("011111111111111011111111111111111111111100011111111111111111111111111111110000000000000011111110011111110100011110101111111111111111111111111111111111101111111111111111111111111111001111011110"));

  const unsigned char Dingbats2700::_cat[] = {
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So
  };

}; // namespace Babylon

dload(Babylon::Dingbats2700);
