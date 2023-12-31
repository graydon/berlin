/*$Id: 10400-1044F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:05:25 +0200.
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

  class Deseret10400 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Deseret10400() {
      m_first_letter = 0x10400;
      m_last_letter  = 0x1044F;
      // m_version="3.1" // Not yet supported!

    }


    ~Deseret10400() {
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
      return "Deseret";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Deseret10400::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Deseret10400::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Deseret10400::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Deseret10400::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(BIDIR_L);
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
    Deseret10400(const Deseret10400 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<80> m_is_defined;
    static const UCS4 m_upper[80];
    static const UCS4 m_lower[80];
    static const UCS4 m_title[80];
    static const unsigned char _cat[80];

  }; // class Deseret10400

    const std::bitset<80> Deseret10400::m_is_defined(std::string("00111111111111111111111111111111111111110011111111111111111111111111111111111111"));

  const UCS4 Deseret10400::m_upper[] = {
    0x10400, 0x10401, 0x10402, 0x10403, 0x10404, 0x10405, 0x10406, 0x10407, 
    0x10408, 0x10409, 0x1040A, 0x1040B, 0x1040C, 0x1040D, 0x1040E, 0x1040F, 
    0x10410, 0x10411, 0x10412, 0x10413, 0x10414, 0x10415, 0x10416, 0x10417, 
    0x10418, 0x10419, 0x1041A, 0x1041B, 0x1041C, 0x1041D, 0x1041E, 0x1041F, 
    0x10420, 0x10421, 0x10422, 0x10423, 0x10424, 0x10425, 0x10426, 0x10427, 
    0x10400, 0x10401, 0x10402, 0x10403, 0x10404, 0x10405, 0x10406, 0x10407, 
    0x10408, 0x10409, 0x1040A, 0x1040B, 0x1040C, 0x1040D, 0x1040E, 0x1040F, 
    0x10410, 0x10411, 0x10412, 0x10413, 0x10414, 0x10415, 0x10416, 0x10417, 
    0x10418, 0x10419, 0x1041A, 0x1041B, 0x1041C, 0x1041D, 0x1041E, 0x1041F, 
    0x10420, 0x10421, 0x10422, 0x10423, 0x10424, 0x10425, 0x1044E, 0x1044F
  };

  const UCS4 Deseret10400::m_lower[] = {
    0x10428, 0x10429, 0x1042A, 0x1042B, 0x1042C, 0x1042D, 0x1042E, 0x1042F, 
    0x10430, 0x10431, 0x10432, 0x10433, 0x10434, 0x10435, 0x10436, 0x10437, 
    0x10438, 0x10439, 0x1043A, 0x1043B, 0x1043C, 0x1043D, 0x1043E, 0x1043F, 
    0x10440, 0x10441, 0x10442, 0x10443, 0x10444, 0x10445, 0x10446, 0x10447, 
    0x10448, 0x10449, 0x1044A, 0x1044B, 0x1044C, 0x1044D, 0x10426, 0x10427, 
    0x10428, 0x10429, 0x1042A, 0x1042B, 0x1042C, 0x1042D, 0x1042E, 0x1042F, 
    0x10430, 0x10431, 0x10432, 0x10433, 0x10434, 0x10435, 0x10436, 0x10437, 
    0x10438, 0x10439, 0x1043A, 0x1043B, 0x1043C, 0x1043D, 0x1043E, 0x1043F, 
    0x10440, 0x10441, 0x10442, 0x10443, 0x10444, 0x10445, 0x10446, 0x10447, 
    0x10448, 0x10449, 0x1044A, 0x1044B, 0x1044C, 0x1044D, 0x1044E, 0x1044F
  };

  const UCS4 Deseret10400::m_title[] = {
    0x10400, 0x10401, 0x10402, 0x10403, 0x10404, 0x10405, 0x10406, 0x10407, 
    0x10408, 0x10409, 0x1040A, 0x1040B, 0x1040C, 0x1040D, 0x1040E, 0x1040F, 
    0x10410, 0x10411, 0x10412, 0x10413, 0x10414, 0x10415, 0x10416, 0x10417, 
    0x10418, 0x10419, 0x1041A, 0x1041B, 0x1041C, 0x1041D, 0x1041E, 0x1041F, 
    0x10420, 0x10421, 0x10422, 0x10423, 0x10424, 0x10425, 0x10426, 0x10427, 
    0x10400, 0x10401, 0x10402, 0x10403, 0x10404, 0x10405, 0x10406, 0x10407, 
    0x10408, 0x10409, 0x1040A, 0x1040B, 0x1040C, 0x1040D, 0x1040E, 0x1040F, 
    0x10410, 0x10411, 0x10412, 0x10413, 0x10414, 0x10415, 0x10416, 0x10417, 
    0x10418, 0x10419, 0x1041A, 0x1041B, 0x1041C, 0x1041D, 0x1041E, 0x1041F, 
    0x10420, 0x10421, 0x10422, 0x10423, 0x10424, 0x10425, 0x1044E, 0x1044F
  };

  const unsigned char Deseret10400::_cat[] = {
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu
  };

}; // namespace Babylon

dload(Babylon::Deseret10400);
