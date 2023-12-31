/*$Id: 3200-32FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:49:57 +0200.
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

  class Enclosed_CJK_Letters_and_Months3200 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Enclosed_CJK_Letters_and_Months3200() {
      m_first_letter = 0x3200;
      m_last_letter  = 0x32FF;
      // m_version="3.1" // Not yet supported!

    }


    ~Enclosed_CJK_Letters_and_Months3200() {
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
      return "Enclosed CJK Letters and Months";
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
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x3220u:
        return 1.000000;
        break;
      case 0x3221u:
        return 2.000000;
        break;
      case 0x3222u:
        return 3.000000;
        break;
      case 0x3223u:
        return 4.000000;
        break;
      case 0x3224u:
        return 5.000000;
        break;
      case 0x3225u:
        return 6.000000;
        break;
      case 0x3226u:
        return 7.000000;
        break;
      case 0x3227u:
        return 8.000000;
        break;
      case 0x3228u:
        return 9.000000;
        break;
      case 0x3229u:
        return 10.000000;
        break;
      case 0x3280u:
        return 1.000000;
        break;
      case 0x3281u:
        return 2.000000;
        break;
      case 0x3282u:
        return 3.000000;
        break;
      case 0x3283u:
        return 4.000000;
        break;
      case 0x3284u:
        return 5.000000;
        break;
      case 0x3285u:
        return 6.000000;
        break;
      case 0x3286u:
        return 7.000000;
        break;
      case 0x3287u:
        return 8.000000;
        break;
      case 0x3288u:
        return 9.000000;
        break;
      case 0x3289u:
        return 10.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x3220u:
      case 0x3221u:
      case 0x3222u:
      case 0x3223u:
      case 0x3224u:
      case 0x3225u:
      case 0x3226u:
      case 0x3227u:
      case 0x3228u:
      case 0x3229u:
      case 0x3280u:
      case 0x3281u:
      case 0x3282u:
      case 0x3283u:
      case 0x3284u:
      case 0x3285u:
      case 0x3286u:
      case 0x3287u:
      case 0x3288u:
      case 0x3289u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Enclosed_CJK_Letters_and_Months3200::_cat[uc - m_first_letter]);
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
      return Babylon::Char_Decomp(Enclosed_CJK_Letters_and_Months3200::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Enclosed_CJK_Letters_and_Months3200::m_decompStr[uc - m_first_letter][0];
      us[1] = Enclosed_CJK_Letters_and_Months3200::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0x3200:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3201:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3202:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3203:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3204:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3205:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3206:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3207:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3208:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3209:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x320A:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x320B:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x320C:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x320D:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x320E:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x320F:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3210:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3211:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3212:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3213:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3214:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3215:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3216:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3217:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3218:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x3219:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x321A:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x321B:
        us.resize(4);
        us[2u] = 0x1161u;
        us[3u] = 0x0029u;
        break;

      case 0x321C:
        us.resize(4);
        us[2u] = 0x116Eu;
        us[3u] = 0x0029u;
        break;

      case 0x3220:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3221:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3222:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3223:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3224:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3225:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3226:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3227:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3228:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3229:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x322A:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x322B:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x322C:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x322D:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x322E:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x322F:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3230:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3231:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3232:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3233:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3234:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3235:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3236:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3237:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3238:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3239:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x323A:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x323B:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x323C:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x323D:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x323E:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x323F:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3240:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3241:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3242:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x3243:
        us.resize(3);
        us[2u] = 0x0029u;
        break;

      case 0x32C9:
        us.resize(3);
        us[2u] = 0x6708u;
        break;

      case 0x32CA:
        us.resize(3);
        us[2u] = 0x6708u;
        break;

      case 0x32CB:
        us.resize(3);
        us[2u] = 0x6708u;
        break;
      }
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
    Enclosed_CJK_Letters_and_Months3200(const Enclosed_CJK_Letters_and_Months3200 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const unsigned char _cat[256];
    static const unsigned char _decomp[256];
    static const UCS4 m_decompStr[256][2];

  }; // class Enclosed_CJK_Letters_and_Months3200

    const std::bitset<256> Enclosed_CJK_Letters_and_Months3200::m_is_defined(std::string("0111111111111111111111111111111111111111111111110000111111111111000000000000000111111111111111111111111111111111111111111111111110001111111111111111111111111111000000000000000000000000000011111111111111111111111111111111111100011111111111111111111111111111"));

  const unsigned char Enclosed_CJK_Letters_and_Months3200::_cat[] = {
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
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
    CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, CAT_No, 
    CAT_No, CAT_No, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
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
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So
  };

  const unsigned char Enclosed_CJK_Letters_and_Months3200::_decomp[] = {
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, 
    DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CIRCLE, DECOMP_CANONICAL
  };

  const UCS4 Enclosed_CJK_Letters_and_Months3200::m_decompStr[][2] = {
    { 0x0028u, 0x1100u }, { 0x0028u, 0x1102u }, { 0x0028u, 0x1103u }, { 0x0028u, 0x1105u }, 
    { 0x0028u, 0x1106u }, { 0x0028u, 0x1107u }, { 0x0028u, 0x1109u }, { 0x0028u, 0x110Bu }, 
    { 0x0028u, 0x110Cu }, { 0x0028u, 0x110Eu }, { 0x0028u, 0x110Fu }, { 0x0028u, 0x1110u }, 
    { 0x0028u, 0x1111u }, { 0x0028u, 0x1112u }, { 0x0028u, 0x1100u }, { 0x0028u, 0x1102u }, 
    { 0x0028u, 0x1103u }, { 0x0028u, 0x1105u }, { 0x0028u, 0x1106u }, { 0x0028u, 0x1107u }, 
    { 0x0028u, 0x1109u }, { 0x0028u, 0x110Bu }, { 0x0028u, 0x110Cu }, { 0x0028u, 0x110Eu }, 
    { 0x0028u, 0x110Fu }, { 0x0028u, 0x1110u }, { 0x0028u, 0x1111u }, { 0x0028u, 0x1112u }, 
    { 0x0028u, 0x110Cu }, { 0x321Du, 0x0000u }, { 0x321Eu, 0x0000u }, { 0x321Fu, 0x0000u }, 
    { 0x0028u, 0x4E00u }, { 0x0028u, 0x4E8Cu }, { 0x0028u, 0x4E09u }, { 0x0028u, 0x56DBu }, 
    { 0x0028u, 0x4E94u }, { 0x0028u, 0x516Du }, { 0x0028u, 0x4E03u }, { 0x0028u, 0x516Bu }, 
    { 0x0028u, 0x4E5Du }, { 0x0028u, 0x5341u }, { 0x0028u, 0x6708u }, { 0x0028u, 0x706Bu }, 
    { 0x0028u, 0x6C34u }, { 0x0028u, 0x6728u }, { 0x0028u, 0x91D1u }, { 0x0028u, 0x571Fu }, 
    { 0x0028u, 0x65E5u }, { 0x0028u, 0x682Au }, { 0x0028u, 0x6709u }, { 0x0028u, 0x793Eu }, 
    { 0x0028u, 0x540Du }, { 0x0028u, 0x7279u }, { 0x0028u, 0x8CA1u }, { 0x0028u, 0x795Du }, 
    { 0x0028u, 0x52B4u }, { 0x0028u, 0x4EE3u }, { 0x0028u, 0x547Cu }, { 0x0028u, 0x5B66u }, 
    { 0x0028u, 0x76E3u }, { 0x0028u, 0x4F01u }, { 0x0028u, 0x8CC7u }, { 0x0028u, 0x5354u }, 
    { 0x0028u, 0x796Du }, { 0x0028u, 0x4F11u }, { 0x0028u, 0x81EAu }, { 0x0028u, 0x81F3u }, 
    { 0x3244u, 0x0000u }, { 0x3245u, 0x0000u }, { 0x3246u, 0x0000u }, { 0x3247u, 0x0000u }, 
    { 0x3248u, 0x0000u }, { 0x3249u, 0x0000u }, { 0x324Au, 0x0000u }, { 0x324Bu, 0x0000u }, 
    { 0x324Cu, 0x0000u }, { 0x324Du, 0x0000u }, { 0x324Eu, 0x0000u }, { 0x324Fu, 0x0000u }, 
    { 0x3250u, 0x0000u }, { 0x3251u, 0x0000u }, { 0x3252u, 0x0000u }, { 0x3253u, 0x0000u }, 
    { 0x3254u, 0x0000u }, { 0x3255u, 0x0000u }, { 0x3256u, 0x0000u }, { 0x3257u, 0x0000u }, 
    { 0x3258u, 0x0000u }, { 0x3259u, 0x0000u }, { 0x325Au, 0x0000u }, { 0x325Bu, 0x0000u }, 
    { 0x325Cu, 0x0000u }, { 0x325Du, 0x0000u }, { 0x325Eu, 0x0000u }, { 0x325Fu, 0x0000u }, 
    { 0x1100u, 0x0000u }, { 0x1102u, 0x0000u }, { 0x1103u, 0x0000u }, { 0x1105u, 0x0000u }, 
    { 0x1106u, 0x0000u }, { 0x1107u, 0x0000u }, { 0x1109u, 0x0000u }, { 0x110Bu, 0x0000u }, 
    { 0x110Cu, 0x0000u }, { 0x110Eu, 0x0000u }, { 0x110Fu, 0x0000u }, { 0x1110u, 0x0000u }, 
    { 0x1111u, 0x0000u }, { 0x1112u, 0x0000u }, { 0x1100u, 0x1161u }, { 0x1102u, 0x1161u }, 
    { 0x1103u, 0x1161u }, { 0x1105u, 0x1161u }, { 0x1106u, 0x1161u }, { 0x1107u, 0x1161u }, 
    { 0x1109u, 0x1161u }, { 0x110Bu, 0x1161u }, { 0x110Cu, 0x1161u }, { 0x110Eu, 0x1161u }, 
    { 0x110Fu, 0x1161u }, { 0x1110u, 0x1161u }, { 0x1111u, 0x1161u }, { 0x1112u, 0x1161u }, 
    { 0x327Cu, 0x0000u }, { 0x327Du, 0x0000u }, { 0x327Eu, 0x0000u }, { 0x327Fu, 0x0000u }, 
    { 0x4E00u, 0x0000u }, { 0x4E8Cu, 0x0000u }, { 0x4E09u, 0x0000u }, { 0x56DBu, 0x0000u }, 
    { 0x4E94u, 0x0000u }, { 0x516Du, 0x0000u }, { 0x4E03u, 0x0000u }, { 0x516Bu, 0x0000u }, 
    { 0x4E5Du, 0x0000u }, { 0x5341u, 0x0000u }, { 0x6708u, 0x0000u }, { 0x706Bu, 0x0000u }, 
    { 0x6C34u, 0x0000u }, { 0x6728u, 0x0000u }, { 0x91D1u, 0x0000u }, { 0x571Fu, 0x0000u }, 
    { 0x65E5u, 0x0000u }, { 0x682Au, 0x0000u }, { 0x6709u, 0x0000u }, { 0x793Eu, 0x0000u }, 
    { 0x540Du, 0x0000u }, { 0x7279u, 0x0000u }, { 0x8CA1u, 0x0000u }, { 0x795Du, 0x0000u }, 
    { 0x52B4u, 0x0000u }, { 0x79D8u, 0x0000u }, { 0x7537u, 0x0000u }, { 0x5973u, 0x0000u }, 
    { 0x9069u, 0x0000u }, { 0x512Au, 0x0000u }, { 0x5370u, 0x0000u }, { 0x6CE8u, 0x0000u }, 
    { 0x9805u, 0x0000u }, { 0x4F11u, 0x0000u }, { 0x5199u, 0x0000u }, { 0x6B63u, 0x0000u }, 
    { 0x4E0Au, 0x0000u }, { 0x4E2Du, 0x0000u }, { 0x4E0Bu, 0x0000u }, { 0x5DE6u, 0x0000u }, 
    { 0x53F3u, 0x0000u }, { 0x533Bu, 0x0000u }, { 0x5B97u, 0x0000u }, { 0x5B66u, 0x0000u }, 
    { 0x76E3u, 0x0000u }, { 0x4F01u, 0x0000u }, { 0x8CC7u, 0x0000u }, { 0x5354u, 0x0000u }, 
    { 0x591Cu, 0x0000u }, { 0x32B1u, 0x0000u }, { 0x32B2u, 0x0000u }, { 0x32B3u, 0x0000u }, 
    { 0x32B4u, 0x0000u }, { 0x32B5u, 0x0000u }, { 0x32B6u, 0x0000u }, { 0x32B7u, 0x0000u }, 
    { 0x32B8u, 0x0000u }, { 0x32B9u, 0x0000u }, { 0x32BAu, 0x0000u }, { 0x32BBu, 0x0000u }, 
    { 0x32BCu, 0x0000u }, { 0x32BDu, 0x0000u }, { 0x32BEu, 0x0000u }, { 0x32BFu, 0x0000u }, 
    { 0x0031u, 0x6708u }, { 0x0032u, 0x6708u }, { 0x0033u, 0x6708u }, { 0x0034u, 0x6708u }, 
    { 0x0035u, 0x6708u }, { 0x0036u, 0x6708u }, { 0x0037u, 0x6708u }, { 0x0038u, 0x6708u }, 
    { 0x0039u, 0x6708u }, { 0x0031u, 0x0030u }, { 0x0031u, 0x0031u }, { 0x0031u, 0x0032u }, 
    { 0x32CCu, 0x0000u }, { 0x32CDu, 0x0000u }, { 0x32CEu, 0x0000u }, { 0x32CFu, 0x0000u }, 
    { 0x30A2u, 0x0000u }, { 0x30A4u, 0x0000u }, { 0x30A6u, 0x0000u }, { 0x30A8u, 0x0000u }, 
    { 0x30AAu, 0x0000u }, { 0x30ABu, 0x0000u }, { 0x30ADu, 0x0000u }, { 0x30AFu, 0x0000u }, 
    { 0x30B1u, 0x0000u }, { 0x30B3u, 0x0000u }, { 0x30B5u, 0x0000u }, { 0x30B7u, 0x0000u }, 
    { 0x30B9u, 0x0000u }, { 0x30BBu, 0x0000u }, { 0x30BDu, 0x0000u }, { 0x30BFu, 0x0000u }, 
    { 0x30C1u, 0x0000u }, { 0x30C4u, 0x0000u }, { 0x30C6u, 0x0000u }, { 0x30C8u, 0x0000u }, 
    { 0x30CAu, 0x0000u }, { 0x30CBu, 0x0000u }, { 0x30CCu, 0x0000u }, { 0x30CDu, 0x0000u }, 
    { 0x30CEu, 0x0000u }, { 0x30CFu, 0x0000u }, { 0x30D2u, 0x0000u }, { 0x30D5u, 0x0000u }, 
    { 0x30D8u, 0x0000u }, { 0x30DBu, 0x0000u }, { 0x30DEu, 0x0000u }, { 0x30DFu, 0x0000u }, 
    { 0x30E0u, 0x0000u }, { 0x30E1u, 0x0000u }, { 0x30E2u, 0x0000u }, { 0x30E4u, 0x0000u }, 
    { 0x30E6u, 0x0000u }, { 0x30E8u, 0x0000u }, { 0x30E9u, 0x0000u }, { 0x30EAu, 0x0000u }, 
    { 0x30EBu, 0x0000u }, { 0x30ECu, 0x0000u }, { 0x30EDu, 0x0000u }, { 0x30EFu, 0x0000u }, 
    { 0x30F0u, 0x0000u }, { 0x30F1u, 0x0000u }, { 0x30F2u, 0x0000u }, { 0x32FFu, 0x0000u }
  };

}; // namespace Babylon

dload(Babylon::Enclosed_CJK_Letters_and_Months3200);
