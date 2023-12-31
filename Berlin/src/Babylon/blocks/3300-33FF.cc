/*$Id: 3300-33FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:50:04 +0200.
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

  class CJK_Compatibility3300 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    CJK_Compatibility3300() {
      m_first_letter = 0x3300;
      m_last_letter  = 0x33FF;
      // m_version="3.1" // Not yet supported!

    }


    ~CJK_Compatibility3300() {
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
      return "CJK Compatibility";
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
      return Babylon::Bidir_Props(BIDIR_L);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(CJK_Compatibility3300::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = CJK_Compatibility3300::m_decompStr[uc - m_first_letter][0];
      us[1] = CJK_Compatibility3300::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0x3300:
        us.resize(4);
        us[2u] = 0x30FCu;
        us[3u] = 0x30C8u;
        break;

      case 0x3301:
        us.resize(4);
        us[2u] = 0x30D5u;
        us[3u] = 0x30A1u;
        break;

      case 0x3302:
        us.resize(4);
        us[2u] = 0x30DAu;
        us[3u] = 0x30A2u;
        break;

      case 0x3303:
        us.resize(3);
        us[2u] = 0x30EBu;
        break;

      case 0x3304:
        us.resize(4);
        us[2u] = 0x30F3u;
        us[3u] = 0x30B0u;
        break;

      case 0x3305:
        us.resize(3);
        us[2u] = 0x30C1u;
        break;

      case 0x3306:
        us.resize(3);
        us[2u] = 0x30F3u;
        break;

      case 0x3307:
        us.resize(5);
        us[2u] = 0x30AFu;
        us[3u] = 0x30FCu;
        us[4u] = 0x30C9u;
        break;

      case 0x3308:
        us.resize(4);
        us[2u] = 0x30ABu;
        us[3u] = 0x30FCu;
        break;

      case 0x3309:
        us.resize(3);
        us[2u] = 0x30B9u;
        break;

      case 0x330A:
        us.resize(3);
        us[2u] = 0x30E0u;
        break;

      case 0x330B:
        us.resize(3);
        us[2u] = 0x30EAu;
        break;

      case 0x330C:
        us.resize(4);
        us[2u] = 0x30C3u;
        us[3u] = 0x30C8u;
        break;

      case 0x330D:
        us.resize(4);
        us[2u] = 0x30EAu;
        us[3u] = 0x30FCu;
        break;

      case 0x330E:
        us.resize(3);
        us[2u] = 0x30F3u;
        break;

      case 0x330F:
        us.resize(3);
        us[2u] = 0x30DEu;
        break;

      case 0x3311:
        us.resize(3);
        us[2u] = 0x30FCu;
        break;

      case 0x3312:
        us.resize(4);
        us[2u] = 0x30EAu;
        us[3u] = 0x30FCu;
        break;

      case 0x3313:
        us.resize(4);
        us[2u] = 0x30C0u;
        us[3u] = 0x30FCu;
        break;

      case 0x3315:
        us.resize(5);
        us[2u] = 0x30B0u;
        us[3u] = 0x30E9u;
        us[4u] = 0x30E0u;
        break;

      case 0x3316:
        us.resize(6);
        us[2u] = 0x30E1u;
        us[3u] = 0x30FCu;
        us[4u] = 0x30C8u;
        us[5u] = 0x30EBu;
        break;

      case 0x3317:
        us.resize(5);
        us[2u] = 0x30EFu;
        us[3u] = 0x30C3u;
        us[4u] = 0x30C8u;
        break;

      case 0x3318:
        us.resize(3);
        us[2u] = 0x30E0u;
        break;

      case 0x3319:
        us.resize(5);
        us[2u] = 0x30E0u;
        us[3u] = 0x30C8u;
        us[4u] = 0x30F3u;
        break;

      case 0x331A:
        us.resize(5);
        us[2u] = 0x30BCu;
        us[3u] = 0x30A4u;
        us[4u] = 0x30EDu;
        break;

      case 0x331B:
        us.resize(4);
        us[2u] = 0x30FCu;
        us[3u] = 0x30CDu;
        break;

      case 0x331C:
        us.resize(3);
        us[2u] = 0x30B9u;
        break;

      case 0x331D:
        us.resize(3);
        us[2u] = 0x30CAu;
        break;

      case 0x331E:
        us.resize(3);
        us[2u] = 0x30DDu;
        break;

      case 0x331F:
        us.resize(4);
        us[2u] = 0x30AFu;
        us[3u] = 0x30EBu;
        break;

      case 0x3320:
        us.resize(5);
        us[2u] = 0x30C1u;
        us[3u] = 0x30FCu;
        us[4u] = 0x30E0u;
        break;

      case 0x3321:
        us.resize(4);
        us[2u] = 0x30F3u;
        us[3u] = 0x30B0u;
        break;

      case 0x3322:
        us.resize(3);
        us[2u] = 0x30C1u;
        break;

      case 0x3323:
        us.resize(3);
        us[2u] = 0x30C8u;
        break;

      case 0x3324:
        us.resize(3);
        us[2u] = 0x30B9u;
        break;

      case 0x3329:
        us.resize(3);
        us[2u] = 0x30C8u;
        break;

      case 0x332A:
        us.resize(3);
        us[2u] = 0x30C4u;
        break;

      case 0x332B:
        us.resize(5);
        us[2u] = 0x30BBu;
        us[3u] = 0x30F3u;
        us[4u] = 0x30C8u;
        break;

      case 0x332C:
        us.resize(3);
        us[2u] = 0x30C4u;
        break;

      case 0x332D:
        us.resize(4);
        us[2u] = 0x30ECu;
        us[3u] = 0x30EBu;
        break;

      case 0x332E:
        us.resize(5);
        us[2u] = 0x30B9u;
        us[3u] = 0x30C8u;
        us[4u] = 0x30EBu;
        break;

      case 0x332F:
        us.resize(3);
        us[2u] = 0x30EBu;
        break;

      case 0x3332:
        us.resize(5);
        us[2u] = 0x30E9u;
        us[3u] = 0x30C3u;
        us[4u] = 0x30C9u;
        break;

      case 0x3333:
        us.resize(4);
        us[2u] = 0x30FCu;
        us[3u] = 0x30C8u;
        break;

      case 0x3334:
        us.resize(5);
        us[2u] = 0x30B7u;
        us[3u] = 0x30A7u;
        us[4u] = 0x30EBu;
        break;

      case 0x3335:
        us.resize(3);
        us[2u] = 0x30F3u;
        break;

      case 0x3336:
        us.resize(5);
        us[2u] = 0x30BFu;
        us[3u] = 0x30FCu;
        us[4u] = 0x30EBu;
        break;

      case 0x3338:
        us.resize(3);
        us[2u] = 0x30D2u;
        break;

      case 0x3339:
        us.resize(3);
        us[2u] = 0x30C4u;
        break;

      case 0x333A:
        us.resize(3);
        us[2u] = 0x30B9u;
        break;

      case 0x333B:
        us.resize(3);
        us[2u] = 0x30B8u;
        break;

      case 0x333C:
        us.resize(3);
        us[2u] = 0x30BFu;
        break;

      case 0x333D:
        us.resize(4);
        us[2u] = 0x30F3u;
        us[3u] = 0x30C8u;
        break;

      case 0x333E:
        us.resize(3);
        us[2u] = 0x30C8u;
        break;

      case 0x3340:
        us.resize(3);
        us[2u] = 0x30C9u;
        break;

      case 0x3341:
        us.resize(3);
        us[2u] = 0x30EBu;
        break;

      case 0x3342:
        us.resize(3);
        us[2u] = 0x30F3u;
        break;

      case 0x3343:
        us.resize(4);
        us[2u] = 0x30AFu;
        us[3u] = 0x30EDu;
        break;

      case 0x3344:
        us.resize(3);
        us[2u] = 0x30EBu;
        break;

      case 0x3345:
        us.resize(3);
        us[2u] = 0x30CFu;
        break;

      case 0x3346:
        us.resize(3);
        us[2u] = 0x30AFu;
        break;

      case 0x3347:
        us.resize(5);
        us[2u] = 0x30B7u;
        us[3u] = 0x30E7u;
        us[4u] = 0x30F3u;
        break;

      case 0x3348:
        us.resize(4);
        us[2u] = 0x30EDu;
        us[3u] = 0x30F3u;
        break;

      case 0x334A:
        us.resize(5);
        us[2u] = 0x30D0u;
        us[3u] = 0x30FCu;
        us[4u] = 0x30EBu;
        break;

      case 0x334C:
        us.resize(4);
        us[2u] = 0x30C8u;
        us[3u] = 0x30F3u;
        break;

      case 0x334D:
        us.resize(4);
        us[2u] = 0x30C8u;
        us[3u] = 0x30EBu;
        break;

      case 0x334E:
        us.resize(3);
        us[2u] = 0x30C9u;
        break;

      case 0x334F:
        us.resize(3);
        us[2u] = 0x30EBu;
        break;

      case 0x3350:
        us.resize(3);
        us[2u] = 0x30F3u;
        break;

      case 0x3351:
        us.resize(4);
        us[2u] = 0x30C8u;
        us[3u] = 0x30EBu;
        break;

      case 0x3353:
        us.resize(3);
        us[2u] = 0x30FCu;
        break;

      case 0x3354:
        us.resize(4);
        us[2u] = 0x30D6u;
        us[3u] = 0x30EBu;
        break;

      case 0x3356:
        us.resize(5);
        us[2u] = 0x30C8u;
        us[3u] = 0x30B2u;
        us[4u] = 0x30F3u;
        break;

      case 0x3357:
        us.resize(3);
        us[2u] = 0x30C8u;
        break;

      case 0x3362:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3363:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3364:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3365:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3366:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3367:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3368:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3369:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x336A:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x336B:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x336C:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x336D:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x336E:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x336F:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3370:
        us.resize(3);
        us[2u] = 0x70B9u;
        break;

      case 0x3371:
        us.resize(3);
        us[2u] = 0x0061u;
        break;

      case 0x3374:
        us.resize(3);
        us[2u] = 0x0072u;
        break;

      case 0x337F:
        us.resize(4);
        us[2u] = 0x4F1Au;
        us[3u] = 0x793Eu;
        break;

      case 0x3388:
        us.resize(3);
        us[2u] = 0x006Cu;
        break;

      case 0x3389:
        us.resize(4);
        us[2u] = 0x0061u;
        us[3u] = 0x006Cu;
        break;

      case 0x3391:
        us.resize(3);
        us[2u] = 0x007Au;
        break;

      case 0x3392:
        us.resize(3);
        us[2u] = 0x007Au;
        break;

      case 0x3393:
        us.resize(3);
        us[2u] = 0x007Au;
        break;

      case 0x3394:
        us.resize(3);
        us[2u] = 0x007Au;
        break;

      case 0x339F:
        us.resize(3);
        us[2u] = 0x00B2u;
        break;

      case 0x33A0:
        us.resize(3);
        us[2u] = 0x00B2u;
        break;

      case 0x33A2:
        us.resize(3);
        us[2u] = 0x00B2u;
        break;

      case 0x33A3:
        us.resize(3);
        us[2u] = 0x00B3u;
        break;

      case 0x33A4:
        us.resize(3);
        us[2u] = 0x00B3u;
        break;

      case 0x33A6:
        us.resize(3);
        us[2u] = 0x00B3u;
        break;

      case 0x33A7:
        us.resize(3);
        us[2u] = 0x0073u;
        break;

      case 0x33A8:
        us.resize(4);
        us[2u] = 0x0073u;
        us[3u] = 0x00B2u;
        break;

      case 0x33AA:
        us.resize(3);
        us[2u] = 0x0061u;
        break;

      case 0x33AB:
        us.resize(3);
        us[2u] = 0x0061u;
        break;

      case 0x33AC:
        us.resize(3);
        us[2u] = 0x0061u;
        break;

      case 0x33AD:
        us.resize(3);
        us[2u] = 0x0064u;
        break;

      case 0x33AE:
        us.resize(5);
        us[2u] = 0x0064u;
        us[3u] = 0x2215u;
        us[4u] = 0x0073u;
        break;

      case 0x33AF:
        us.resize(6);
        us[2u] = 0x0064u;
        us[3u] = 0x2215u;
        us[4u] = 0x0073u;
        us[5u] = 0x00B2u;
        break;

      case 0x33C2:
        us.resize(4);
        us[2u] = 0x006Du;
        us[3u] = 0x002Eu;
        break;

      case 0x33C6:
        us.resize(4);
        us[2u] = 0x006Bu;
        us[3u] = 0x0067u;
        break;

      case 0x33C7:
        us.resize(3);
        us[2u] = 0x002Eu;
        break;

      case 0x33D2:
        us.resize(3);
        us[2u] = 0x0067u;
        break;

      case 0x33D5:
        us.resize(3);
        us[2u] = 0x006Cu;
        break;

      case 0x33D6:
        us.resize(3);
        us[2u] = 0x006Cu;
        break;

      case 0x33D8:
        us.resize(4);
        us[2u] = 0x006Du;
        us[3u] = 0x002Eu;
        break;

      case 0x33D9:
        us.resize(3);
        us[2u] = 0x004Du;
        break;

      case 0x33E9:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33EA:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33EB:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33EC:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33ED:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33EE:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33EF:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F0:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F1:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F2:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F3:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F4:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F5:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F6:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F7:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F8:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33F9:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33FA:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33FB:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33FC:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33FD:
        us.resize(3);
        us[2u] = 0x65E5u;
        break;

      case 0x33FE:
        us.resize(3);
        us[2u] = 0x65E5u;
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
    CJK_Compatibility3300(const CJK_Compatibility3300 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const unsigned char _decomp[256];
    static const UCS4 m_decompStr[256][2];

  }; // class CJK_Compatibility3300

    const std::bitset<256> CJK_Compatibility3300::m_is_defined(std::string("0111111111111111111111111111111100111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char CJK_Compatibility3300::_decomp[] = {
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, 
    DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_SQUARE, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL
  };

  const UCS4 CJK_Compatibility3300::m_decompStr[][2] = {
    { 0x30A2u, 0x30D1u }, { 0x30A2u, 0x30EBu }, { 0x30A2u, 0x30F3u }, { 0x30A2u, 0x30FCu }, 
    { 0x30A4u, 0x30CBu }, { 0x30A4u, 0x30F3u }, { 0x30A6u, 0x30A9u }, { 0x30A8u, 0x30B9u }, 
    { 0x30A8u, 0x30FCu }, { 0x30AAu, 0x30F3u }, { 0x30AAu, 0x30FCu }, { 0x30ABu, 0x30A4u }, 
    { 0x30ABu, 0x30E9u }, { 0x30ABu, 0x30EDu }, { 0x30ACu, 0x30EDu }, { 0x30ACu, 0x30F3u }, 
    { 0x30AEu, 0x30ACu }, { 0x30AEu, 0x30CBu }, { 0x30ADu, 0x30E5u }, { 0x30AEu, 0x30EBu }, 
    { 0x30ADu, 0x30EDu }, { 0x30ADu, 0x30EDu }, { 0x30ADu, 0x30EDu }, { 0x30ADu, 0x30EDu }, 
    { 0x30B0u, 0x30E9u }, { 0x30B0u, 0x30E9u }, { 0x30AFu, 0x30EBu }, { 0x30AFu, 0x30EDu }, 
    { 0x30B1u, 0x30FCu }, { 0x30B3u, 0x30EBu }, { 0x30B3u, 0x30FCu }, { 0x30B5u, 0x30A4u }, 
    { 0x30B5u, 0x30F3u }, { 0x30B7u, 0x30EAu }, { 0x30BBu, 0x30F3u }, { 0x30BBu, 0x30F3u }, 
    { 0x30C0u, 0x30FCu }, { 0x30C7u, 0x30B7u }, { 0x30C9u, 0x30EBu }, { 0x30C8u, 0x30F3u }, 
    { 0x30CAu, 0x30CEu }, { 0x30CEu, 0x30C3u }, { 0x30CFu, 0x30A4u }, { 0x30D1u, 0x30FCu }, 
    { 0x30D1u, 0x30FCu }, { 0x30D0u, 0x30FCu }, { 0x30D4u, 0x30A2u }, { 0x30D4u, 0x30AFu }, 
    { 0x30D4u, 0x30B3u }, { 0x30D3u, 0x30EBu }, { 0x30D5u, 0x30A1u }, { 0x30D5u, 0x30A3u }, 
    { 0x30D6u, 0x30C3u }, { 0x30D5u, 0x30E9u }, { 0x30D8u, 0x30AFu }, { 0x30DAu, 0x30BDu }, 
    { 0x30DAu, 0x30CBu }, { 0x30D8u, 0x30EBu }, { 0x30DAu, 0x30F3u }, { 0x30DAu, 0x30FCu }, 
    { 0x30D9u, 0x30FCu }, { 0x30DDu, 0x30A4u }, { 0x30DCu, 0x30EBu }, { 0x30DBu, 0x30F3u }, 
    { 0x30DDu, 0x30F3u }, { 0x30DBu, 0x30FCu }, { 0x30DBu, 0x30FCu }, { 0x30DEu, 0x30A4u }, 
    { 0x30DEu, 0x30A4u }, { 0x30DEu, 0x30C3u }, { 0x30DEu, 0x30EBu }, { 0x30DEu, 0x30F3u }, 
    { 0x30DFu, 0x30AFu }, { 0x30DFu, 0x30EAu }, { 0x30DFu, 0x30EAu }, { 0x30E1u, 0x30ACu }, 
    { 0x30E1u, 0x30ACu }, { 0x30E1u, 0x30FCu }, { 0x30E4u, 0x30FCu }, { 0x30E4u, 0x30FCu }, 
    { 0x30E6u, 0x30A2u }, { 0x30EAu, 0x30C3u }, { 0x30EAu, 0x30E9u }, { 0x30EBu, 0x30D4u }, 
    { 0x30EBu, 0x30FCu }, { 0x30ECu, 0x30E0u }, { 0x30ECu, 0x30F3u }, { 0x30EFu, 0x30C3u }, 
    { 0x0030u, 0x70B9u }, { 0x0031u, 0x70B9u }, { 0x0032u, 0x70B9u }, { 0x0033u, 0x70B9u }, 
    { 0x0034u, 0x70B9u }, { 0x0035u, 0x70B9u }, { 0x0036u, 0x70B9u }, { 0x0037u, 0x70B9u }, 
    { 0x0038u, 0x70B9u }, { 0x0039u, 0x70B9u }, { 0x0031u, 0x0030u }, { 0x0031u, 0x0031u }, 
    { 0x0031u, 0x0032u }, { 0x0031u, 0x0033u }, { 0x0031u, 0x0034u }, { 0x0031u, 0x0035u }, 
    { 0x0031u, 0x0036u }, { 0x0031u, 0x0037u }, { 0x0031u, 0x0038u }, { 0x0031u, 0x0039u }, 
    { 0x0032u, 0x0030u }, { 0x0032u, 0x0031u }, { 0x0032u, 0x0032u }, { 0x0032u, 0x0033u }, 
    { 0x0032u, 0x0034u }, { 0x0068u, 0x0050u }, { 0x0064u, 0x0061u }, { 0x0041u, 0x0055u }, 
    { 0x0062u, 0x0061u }, { 0x006Fu, 0x0056u }, { 0x0070u, 0x0063u }, { 0x3377u, 0x0000u }, 
    { 0x3378u, 0x0000u }, { 0x3379u, 0x0000u }, { 0x337Au, 0x0000u }, { 0x5E73u, 0x6210u }, 
    { 0x662Du, 0x548Cu }, { 0x5927u, 0x6B63u }, { 0x660Eu, 0x6CBBu }, { 0x682Au, 0x5F0Fu }, 
    { 0x0070u, 0x0041u }, { 0x006Eu, 0x0041u }, { 0x03BCu, 0x0041u }, { 0x006Du, 0x0041u }, 
    { 0x006Bu, 0x0041u }, { 0x004Bu, 0x0042u }, { 0x004Du, 0x0042u }, { 0x0047u, 0x0042u }, 
    { 0x0063u, 0x0061u }, { 0x006Bu, 0x0063u }, { 0x0070u, 0x0046u }, { 0x006Eu, 0x0046u }, 
    { 0x03BCu, 0x0046u }, { 0x03BCu, 0x0067u }, { 0x006Du, 0x0067u }, { 0x006Bu, 0x0067u }, 
    { 0x0048u, 0x007Au }, { 0x006Bu, 0x0048u }, { 0x004Du, 0x0048u }, { 0x0047u, 0x0048u }, 
    { 0x0054u, 0x0048u }, { 0x03BCu, 0x2113u }, { 0x006Du, 0x2113u }, { 0x0064u, 0x2113u }, 
    { 0x006Bu, 0x2113u }, { 0x0066u, 0x006Du }, { 0x006Eu, 0x006Du }, { 0x03BCu, 0x006Du }, 
    { 0x006Du, 0x006Du }, { 0x0063u, 0x006Du }, { 0x006Bu, 0x006Du }, { 0x006Du, 0x006Du }, 
    { 0x0063u, 0x006Du }, { 0x006Du, 0x00B2u }, { 0x006Bu, 0x006Du }, { 0x006Du, 0x006Du }, 
    { 0x0063u, 0x006Du }, { 0x006Du, 0x00B3u }, { 0x006Bu, 0x006Du }, { 0x006Du, 0x2215u }, 
    { 0x006Du, 0x2215u }, { 0x0050u, 0x0061u }, { 0x006Bu, 0x0050u }, { 0x004Du, 0x0050u }, 
    { 0x0047u, 0x0050u }, { 0x0072u, 0x0061u }, { 0x0072u, 0x0061u }, { 0x0072u, 0x0061u }, 
    { 0x0070u, 0x0073u }, { 0x006Eu, 0x0073u }, { 0x03BCu, 0x0073u }, { 0x006Du, 0x0073u }, 
    { 0x0070u, 0x0056u }, { 0x006Eu, 0x0056u }, { 0x03BCu, 0x0056u }, { 0x006Du, 0x0056u }, 
    { 0x006Bu, 0x0056u }, { 0x004Du, 0x0056u }, { 0x0070u, 0x0057u }, { 0x006Eu, 0x0057u }, 
    { 0x03BCu, 0x0057u }, { 0x006Du, 0x0057u }, { 0x006Bu, 0x0057u }, { 0x004Du, 0x0057u }, 
    { 0x006Bu, 0x03A9u }, { 0x004Du, 0x03A9u }, { 0x0061u, 0x002Eu }, { 0x0042u, 0x0071u }, 
    { 0x0063u, 0x0063u }, { 0x0063u, 0x0064u }, { 0x0043u, 0x2215u }, { 0x0043u, 0x006Fu }, 
    { 0x0064u, 0x0042u }, { 0x0047u, 0x0079u }, { 0x0068u, 0x0061u }, { 0x0048u, 0x0050u }, 
    { 0x0069u, 0x006Eu }, { 0x004Bu, 0x004Bu }, { 0x004Bu, 0x004Du }, { 0x006Bu, 0x0074u }, 
    { 0x006Cu, 0x006Du }, { 0x006Cu, 0x006Eu }, { 0x006Cu, 0x006Fu }, { 0x006Cu, 0x0078u }, 
    { 0x006Du, 0x0062u }, { 0x006Du, 0x0069u }, { 0x006Du, 0x006Fu }, { 0x0050u, 0x0048u }, 
    { 0x0070u, 0x002Eu }, { 0x0050u, 0x0050u }, { 0x0050u, 0x0052u }, { 0x0073u, 0x0072u }, 
    { 0x0053u, 0x0076u }, { 0x0057u, 0x0062u }, { 0x33DEu, 0x0000u }, { 0x33DFu, 0x0000u }, 
    { 0x0031u, 0x65E5u }, { 0x0032u, 0x65E5u }, { 0x0033u, 0x65E5u }, { 0x0034u, 0x65E5u }, 
    { 0x0035u, 0x65E5u }, { 0x0036u, 0x65E5u }, { 0x0037u, 0x65E5u }, { 0x0038u, 0x65E5u }, 
    { 0x0039u, 0x65E5u }, { 0x0031u, 0x0030u }, { 0x0031u, 0x0031u }, { 0x0031u, 0x0032u }, 
    { 0x0031u, 0x0033u }, { 0x0031u, 0x0034u }, { 0x0031u, 0x0035u }, { 0x0031u, 0x0036u }, 
    { 0x0031u, 0x0037u }, { 0x0031u, 0x0038u }, { 0x0031u, 0x0039u }, { 0x0032u, 0x0030u }, 
    { 0x0032u, 0x0031u }, { 0x0032u, 0x0032u }, { 0x0032u, 0x0033u }, { 0x0032u, 0x0034u }, 
    { 0x0032u, 0x0035u }, { 0x0032u, 0x0036u }, { 0x0032u, 0x0037u }, { 0x0032u, 0x0038u }, 
    { 0x0032u, 0x0039u }, { 0x0033u, 0x0030u }, { 0x0033u, 0x0031u }, { 0x33FFu, 0x0000u }
  };

}; // namespace Babylon

dload(Babylon::CJK_Compatibility3300);
