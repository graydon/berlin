/*$Id: 300-36F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:14 +0200.
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

  class Combining_Diacritical_Marks300 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Combining_Diacritical_Marks300() {
      m_first_letter = 0x300;
      m_last_letter  = 0x36F;
      // m_version="3.1" // Not yet supported!

    }


    ~Combining_Diacritical_Marks300() {
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
      return "Combining Diacritical Marks";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Combining_Diacritical_Marks300::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return uc;
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Combining_Diacritical_Marks300::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(CAT_Mn);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Combining_Diacritical_Marks300::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(BIDIR_NSM);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Combining_Diacritical_Marks300::m_decompStr[uc - m_first_letter][0];
      us[1] = Combining_Diacritical_Marks300::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(LB_CM);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_A);
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
    Combining_Diacritical_Marks300(const Combining_Diacritical_Marks300 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<112> m_is_defined;
    static const UCS4 m_upper[112];
    static const UCS4 m_title[112];
    static const unsigned char _comb_cl[112];
    static const UCS2 m_decompStr[112][2];
    static const std::bitset<112> m_Diacritic;

  }; // class Combining_Diacritical_Marks300

    const std::bitset<112> Combining_Diacritical_Marks300::m_is_defined(std::string("0000000000000111000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const UCS4 Combining_Diacritical_Marks300::m_upper[] = {
    0x0300, 0x0301, 0x0302, 0x0303, 0x0304, 0x0305, 0x0306, 0x0307, 
    0x0308, 0x0309, 0x030A, 0x030B, 0x030C, 0x030D, 0x030E, 0x030F, 
    0x0310, 0x0311, 0x0312, 0x0313, 0x0314, 0x0315, 0x0316, 0x0317, 
    0x0318, 0x0319, 0x031A, 0x031B, 0x031C, 0x031D, 0x031E, 0x031F, 
    0x0320, 0x0321, 0x0322, 0x0323, 0x0324, 0x0325, 0x0326, 0x0327, 
    0x0328, 0x0329, 0x032A, 0x032B, 0x032C, 0x032D, 0x032E, 0x032F, 
    0x0330, 0x0331, 0x0332, 0x0333, 0x0334, 0x0335, 0x0336, 0x0337, 
    0x0338, 0x0339, 0x033A, 0x033B, 0x033C, 0x033D, 0x033E, 0x033F, 
    0x0340, 0x0341, 0x0342, 0x0343, 0x0344, 0x0399, 0x0346, 0x0347, 
    0x0348, 0x0349, 0x034A, 0x034B, 0x034C, 0x034D, 0x034E, 0x034F, 
    0x0350, 0x0351, 0x0352, 0x0353, 0x0354, 0x0355, 0x0356, 0x0357, 
    0x0358, 0x0359, 0x035A, 0x035B, 0x035C, 0x035D, 0x035E, 0x035F, 
    0x0360, 0x0361, 0x0362, 0x0363, 0x0364, 0x0365, 0x0366, 0x0367, 
    0x0368, 0x0369, 0x036A, 0x036B, 0x036C, 0x036D, 0x036E, 0x036F
  };

  const UCS4 Combining_Diacritical_Marks300::m_title[] = {
    0x0300, 0x0301, 0x0302, 0x0303, 0x0304, 0x0305, 0x0306, 0x0307, 
    0x0308, 0x0309, 0x030A, 0x030B, 0x030C, 0x030D, 0x030E, 0x030F, 
    0x0310, 0x0311, 0x0312, 0x0313, 0x0314, 0x0315, 0x0316, 0x0317, 
    0x0318, 0x0319, 0x031A, 0x031B, 0x031C, 0x031D, 0x031E, 0x031F, 
    0x0320, 0x0321, 0x0322, 0x0323, 0x0324, 0x0325, 0x0326, 0x0327, 
    0x0328, 0x0329, 0x032A, 0x032B, 0x032C, 0x032D, 0x032E, 0x032F, 
    0x0330, 0x0331, 0x0332, 0x0333, 0x0334, 0x0335, 0x0336, 0x0337, 
    0x0338, 0x0339, 0x033A, 0x033B, 0x033C, 0x033D, 0x033E, 0x033F, 
    0x0340, 0x0341, 0x0342, 0x0343, 0x0344, 0x0399, 0x0346, 0x0347, 
    0x0348, 0x0349, 0x034A, 0x034B, 0x034C, 0x034D, 0x034E, 0x034F, 
    0x0350, 0x0351, 0x0352, 0x0353, 0x0354, 0x0355, 0x0356, 0x0357, 
    0x0358, 0x0359, 0x035A, 0x035B, 0x035C, 0x035D, 0x035E, 0x035F, 
    0x0360, 0x0361, 0x0362, 0x0363, 0x0364, 0x0365, 0x0366, 0x0367, 
    0x0368, 0x0369, 0x036A, 0x036B, 0x036C, 0x036D, 0x036E, 0x036F
  };

  const unsigned char Combining_Diacritical_Marks300::_comb_cl[] = {
    230, 230, 230, 230, 230, 230, 230, 230, 
    230, 230, 230, 230, 230, 230, 230, 230, 
    230, 230, 230, 230, 230, 232, 220, 220, 
    220, 220, 232, 216, 220, 220, 220, 220, 
    220, 202, 202, 220, 220, 220, 220, 202, 
    202, 220, 220, 220, 220, 220, 220, 220, 
    220, 220, 220, 220, 1, 1, 1, 1, 
    1, 220, 220, 220, 220, 230, 230, 230, 
    230, 230, 230, 230, 230, 240, 230, 220, 
    220, 220, 230, 230, 230, 220, 220, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    234, 234, 233, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const UCS2 Combining_Diacritical_Marks300::m_decompStr[][2] = {
    { 0x0300u, 0x0000u }, { 0x0301u, 0x0000u }, { 0x0302u, 0x0000u }, { 0x0303u, 0x0000u }, 
    { 0x0304u, 0x0000u }, { 0x0305u, 0x0000u }, { 0x0306u, 0x0000u }, { 0x0307u, 0x0000u }, 
    { 0x0308u, 0x0000u }, { 0x0309u, 0x0000u }, { 0x030Au, 0x0000u }, { 0x030Bu, 0x0000u }, 
    { 0x030Cu, 0x0000u }, { 0x030Du, 0x0000u }, { 0x030Eu, 0x0000u }, { 0x030Fu, 0x0000u }, 
    { 0x0310u, 0x0000u }, { 0x0311u, 0x0000u }, { 0x0312u, 0x0000u }, { 0x0313u, 0x0000u }, 
    { 0x0314u, 0x0000u }, { 0x0315u, 0x0000u }, { 0x0316u, 0x0000u }, { 0x0317u, 0x0000u }, 
    { 0x0318u, 0x0000u }, { 0x0319u, 0x0000u }, { 0x031Au, 0x0000u }, { 0x031Bu, 0x0000u }, 
    { 0x031Cu, 0x0000u }, { 0x031Du, 0x0000u }, { 0x031Eu, 0x0000u }, { 0x031Fu, 0x0000u }, 
    { 0x0320u, 0x0000u }, { 0x0321u, 0x0000u }, { 0x0322u, 0x0000u }, { 0x0323u, 0x0000u }, 
    { 0x0324u, 0x0000u }, { 0x0325u, 0x0000u }, { 0x0326u, 0x0000u }, { 0x0327u, 0x0000u }, 
    { 0x0328u, 0x0000u }, { 0x0329u, 0x0000u }, { 0x032Au, 0x0000u }, { 0x032Bu, 0x0000u }, 
    { 0x032Cu, 0x0000u }, { 0x032Du, 0x0000u }, { 0x032Eu, 0x0000u }, { 0x032Fu, 0x0000u }, 
    { 0x0330u, 0x0000u }, { 0x0331u, 0x0000u }, { 0x0332u, 0x0000u }, { 0x0333u, 0x0000u }, 
    { 0x0334u, 0x0000u }, { 0x0335u, 0x0000u }, { 0x0336u, 0x0000u }, { 0x0337u, 0x0000u }, 
    { 0x0338u, 0x0000u }, { 0x0339u, 0x0000u }, { 0x033Au, 0x0000u }, { 0x033Bu, 0x0000u }, 
    { 0x033Cu, 0x0000u }, { 0x033Du, 0x0000u }, { 0x033Eu, 0x0000u }, { 0x033Fu, 0x0000u }, 
    { 0x0300u, 0x0000u }, { 0x0301u, 0x0000u }, { 0x0342u, 0x0000u }, { 0x0313u, 0x0000u }, 
    { 0x0308u, 0x0301u }, { 0x0345u, 0x0000u }, { 0x0346u, 0x0000u }, { 0x0347u, 0x0000u }, 
    { 0x0348u, 0x0000u }, { 0x0349u, 0x0000u }, { 0x034Au, 0x0000u }, { 0x034Bu, 0x0000u }, 
    { 0x034Cu, 0x0000u }, { 0x034Du, 0x0000u }, { 0x034Eu, 0x0000u }, { 0x034Fu, 0x0000u }, 
    { 0x0350u, 0x0000u }, { 0x0351u, 0x0000u }, { 0x0352u, 0x0000u }, { 0x0353u, 0x0000u }, 
    { 0x0354u, 0x0000u }, { 0x0355u, 0x0000u }, { 0x0356u, 0x0000u }, { 0x0357u, 0x0000u }, 
    { 0x0358u, 0x0000u }, { 0x0359u, 0x0000u }, { 0x035Au, 0x0000u }, { 0x035Bu, 0x0000u }, 
    { 0x035Cu, 0x0000u }, { 0x035Du, 0x0000u }, { 0x035Eu, 0x0000u }, { 0x035Fu, 0x0000u }, 
    { 0x0360u, 0x0000u }, { 0x0361u, 0x0000u }, { 0x0362u, 0x0000u }, { 0x0363u, 0x0000u }, 
    { 0x0364u, 0x0000u }, { 0x0365u, 0x0000u }, { 0x0366u, 0x0000u }, { 0x0367u, 0x0000u }, 
    { 0x0368u, 0x0000u }, { 0x0369u, 0x0000u }, { 0x036Au, 0x0000u }, { 0x036Bu, 0x0000u }, 
    { 0x036Cu, 0x0000u }, { 0x036Du, 0x0000u }, { 0x036Eu, 0x0000u }, { 0x036Fu, 0x0000u }
  };

    const std::bitset<112> Combining_Diacritical_Marks300::m_Diacritic(std::string("0000000000000111000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111"));

}; // namespace Babylon

dload(Babylon::Combining_Diacritical_Marks300);
