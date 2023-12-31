/*$Id: 590-5FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:31 +0200.
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

  class Hebrew590 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Hebrew590() {
      m_first_letter = 0x590;
      m_last_letter  = 0x5FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x000005D0, 0x000005B7)] = 0xFB2E;
      m_composeMap[make_pair(0x000005D0, 0x000005B8)] = 0xFB2F;
      m_composeMap[make_pair(0x000005D0, 0x000005BC)] = 0xFB30;
      m_composeMap[make_pair(0x000005D1, 0x000005BC)] = 0xFB31;
      m_composeMap[make_pair(0x000005D1, 0x000005BF)] = 0xFB4C;
      m_composeMap[make_pair(0x000005D2, 0x000005BC)] = 0xFB32;
      m_composeMap[make_pair(0x000005D3, 0x000005BC)] = 0xFB33;
      m_composeMap[make_pair(0x000005D4, 0x000005BC)] = 0xFB34;
      m_composeMap[make_pair(0x000005D5, 0x000005B9)] = 0xFB4B;
      m_composeMap[make_pair(0x000005D5, 0x000005BC)] = 0xFB35;
      m_composeMap[make_pair(0x000005D6, 0x000005BC)] = 0xFB36;
      m_composeMap[make_pair(0x000005D8, 0x000005BC)] = 0xFB38;
      m_composeMap[make_pair(0x000005D9, 0x000005B4)] = 0xFB1D;
      m_composeMap[make_pair(0x000005D9, 0x000005BC)] = 0xFB39;
      m_composeMap[make_pair(0x000005DA, 0x000005BC)] = 0xFB3A;
      m_composeMap[make_pair(0x000005DB, 0x000005BC)] = 0xFB3B;
      m_composeMap[make_pair(0x000005DB, 0x000005BF)] = 0xFB4D;
      m_composeMap[make_pair(0x000005DC, 0x000005BC)] = 0xFB3C;
      m_composeMap[make_pair(0x000005DE, 0x000005BC)] = 0xFB3E;
      m_composeMap[make_pair(0x000005E0, 0x000005BC)] = 0xFB40;
      m_composeMap[make_pair(0x000005E1, 0x000005BC)] = 0xFB41;
      m_composeMap[make_pair(0x000005E3, 0x000005BC)] = 0xFB43;
      m_composeMap[make_pair(0x000005E4, 0x000005BC)] = 0xFB44;
      m_composeMap[make_pair(0x000005E4, 0x000005BF)] = 0xFB4E;
      m_composeMap[make_pair(0x000005E6, 0x000005BC)] = 0xFB46;
      m_composeMap[make_pair(0x000005E7, 0x000005BC)] = 0xFB47;
      m_composeMap[make_pair(0x000005E8, 0x000005BC)] = 0xFB48;
      m_composeMap[make_pair(0x000005E9, 0x000005BC)] = 0xFB49;
      m_composeMap[make_pair(0x000005E9, 0x000005C1)] = 0xFB2A;
      m_composeMap[make_pair(0x000005E9, 0x000005C2)] = 0xFB2B;
      m_composeMap[make_pair(0x000005EA, 0x000005BC)] = 0xFB4A;
      m_composeMap[make_pair(0x000005F2, 0x000005B7)] = 0xFB1F;

    }


    ~Hebrew590() {
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
      return "Hebrew";
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
      return Babylon::Gen_Cat(Hebrew590::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Hebrew590::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Hebrew590::m_bidir[uc - m_first_letter]);
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
      return Babylon::Line_Break(Hebrew590::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_N);
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
      return m_Other_Alphabetic.test(uc - m_first_letter);
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
    Hebrew590(const Hebrew590 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<112> m_is_defined;
    static const unsigned char _cat[112];
    static const unsigned char _comb_cl[112];
    static const unsigned char m_bidir[112];
    static const unsigned char m_lb[112];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<112> m_Other_Alphabetic;
    static const std::bitset<112> m_Diacritic;

  }; // class Hebrew590

    const std::bitset<112> Hebrew590::m_is_defined(std::string("0000000000011111000001111111111111111111111111110000000000011111111110111111111111111111111110111111111111111110"));

  const unsigned char Hebrew590::_cat[] = {
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Po, CAT_Mn, 
    CAT_Po, CAT_Mn, CAT_Mn, CAT_Po, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Po, CAT_Po, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn
  };

  const unsigned char Hebrew590::_comb_cl[] = {
    0, 220, 230, 230, 230, 230, 220, 230, 
    230, 230, 222, 220, 230, 230, 230, 230, 
    230, 230, 0, 220, 220, 220, 220, 220, 
    230, 230, 220, 230, 230, 222, 228, 230, 
    10, 11, 12, 13, 14, 15, 16, 17, 
    18, 19, 0, 20, 21, 22, 0, 23, 
    0, 24, 25, 0, 230, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Hebrew590::m_bidir[] = {
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_R, BIDIR_NSM, 
    BIDIR_R, BIDIR_NSM, BIDIR_NSM, BIDIR_R, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_R, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM
  };

  const unsigned char Hebrew590::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_CM, 
    LB_AL, LB_CM, LB_CM, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<112> Hebrew590::m_Other_Alphabetic(std::string("0000000000000000000000000000000000000000000000000000000000000110001110111111111100000000000000000000000000000000"));

    const std::bitset<112> Hebrew590::m_Diacritic(std::string("0000000000000000000000000000000000000000000000000000000000000110001110111111111111111111111110111111111111111110"));

}; // namespace Babylon

dload(Babylon::Hebrew590);
