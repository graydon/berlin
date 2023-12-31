/*$Id: 180-24F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:04 +0200.
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

  class Latin_ExtendedB180 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Latin_ExtendedB180() {
      m_first_letter = 0x180;
      m_last_letter  = 0x24F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x000001A0, 0x00000300)] = 0x1EDC;
      m_composeMap[make_pair(0x000001A0, 0x00000301)] = 0x1EDA;
      m_composeMap[make_pair(0x000001A0, 0x00000303)] = 0x1EE0;
      m_composeMap[make_pair(0x000001A0, 0x00000309)] = 0x1EDE;
      m_composeMap[make_pair(0x000001A0, 0x00000323)] = 0x1EE2;
      m_composeMap[make_pair(0x000001A1, 0x00000300)] = 0x1EDD;
      m_composeMap[make_pair(0x000001A1, 0x00000301)] = 0x1EDB;
      m_composeMap[make_pair(0x000001A1, 0x00000303)] = 0x1EE1;
      m_composeMap[make_pair(0x000001A1, 0x00000309)] = 0x1EDF;
      m_composeMap[make_pair(0x000001A1, 0x00000323)] = 0x1EE3;
      m_composeMap[make_pair(0x000001AF, 0x00000300)] = 0x1EEA;
      m_composeMap[make_pair(0x000001AF, 0x00000301)] = 0x1EE8;
      m_composeMap[make_pair(0x000001AF, 0x00000303)] = 0x1EEE;
      m_composeMap[make_pair(0x000001AF, 0x00000309)] = 0x1EEC;
      m_composeMap[make_pair(0x000001AF, 0x00000323)] = 0x1EF0;
      m_composeMap[make_pair(0x000001B0, 0x00000300)] = 0x1EEB;
      m_composeMap[make_pair(0x000001B0, 0x00000301)] = 0x1EE9;
      m_composeMap[make_pair(0x000001B0, 0x00000303)] = 0x1EEF;
      m_composeMap[make_pair(0x000001B0, 0x00000309)] = 0x1EED;
      m_composeMap[make_pair(0x000001B0, 0x00000323)] = 0x1EF1;
      m_composeMap[make_pair(0x000001B7, 0x0000030C)] = 0x01EE;
      m_composeMap[make_pair(0x000001EA, 0x00000304)] = 0x01EC;
      m_composeMap[make_pair(0x000001EB, 0x00000304)] = 0x01ED;
      m_composeMap[make_pair(0x00000226, 0x00000304)] = 0x01E0;
      m_composeMap[make_pair(0x00000227, 0x00000304)] = 0x01E1;
      m_composeMap[make_pair(0x00000228, 0x00000306)] = 0x1E1C;
      m_composeMap[make_pair(0x00000229, 0x00000306)] = 0x1E1D;
      m_composeMap[make_pair(0x0000022E, 0x00000304)] = 0x0230;
      m_composeMap[make_pair(0x0000022F, 0x00000304)] = 0x0231;

    }


    ~Latin_ExtendedB180() {
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
      return "Latin Extended-B";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Latin_ExtendedB180::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Latin_ExtendedB180::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Latin_ExtendedB180::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Latin_ExtendedB180::_cat[uc - m_first_letter]);
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
      return Babylon::Char_Decomp(Latin_ExtendedB180::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Latin_ExtendedB180::m_decompStr[uc - m_first_letter][0];
      us[1] = Latin_ExtendedB180::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Latin_ExtendedB180::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Latin_ExtendedB180::m_ea[uc - m_first_letter]);
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
    Latin_ExtendedB180(const Latin_ExtendedB180 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<208> m_is_defined;
    static const UCS4 m_upper[208];
    static const UCS4 m_lower[208];
    static const UCS4 m_title[208];
    static const unsigned char _cat[208];
    static const unsigned char _decomp[208];
    static const UCS2 m_decompStr[208][2];
    static const unsigned char m_lb[208];
    static const unsigned char m_ea[208];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Latin_ExtendedB180

    const std::bitset<208> Latin_ExtendedB180::m_is_defined(std::string("0000000000000000000000000000111111111111111111001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const UCS4 Latin_ExtendedB180::m_upper[] = {
    0x0180, 0x0181, 0x0182, 0x0182, 0x0184, 0x0184, 0x0186, 0x0187, 
    0x0187, 0x0189, 0x018A, 0x018B, 0x018B, 0x018D, 0x018E, 0x018F, 
    0x0190, 0x0191, 0x0191, 0x0193, 0x0194, 0x01F6, 0x0196, 0x0197, 
    0x0198, 0x0198, 0x019A, 0x019B, 0x019C, 0x019D, 0x019E, 0x019F, 
    0x01A0, 0x01A0, 0x01A2, 0x01A2, 0x01A4, 0x01A4, 0x01A6, 0x01A7, 
    0x01A7, 0x01A9, 0x01AA, 0x01AB, 0x01AC, 0x01AC, 0x01AE, 0x01AF, 
    0x01AF, 0x01B1, 0x01B2, 0x01B3, 0x01B3, 0x01B5, 0x01B5, 0x01B7, 
    0x01B8, 0x01B8, 0x01BA, 0x01BB, 0x01BC, 0x01BC, 0x01BE, 0x01F7, 
    0x01C0, 0x01C1, 0x01C2, 0x01C3, 0x01C4, 0x01C4, 0x01C4, 0x01C7, 
    0x01C7, 0x01C7, 0x01CA, 0x01CA, 0x01CA, 0x01CD, 0x01CD, 0x01CF, 
    0x01CF, 0x01D1, 0x01D1, 0x01D3, 0x01D3, 0x01D5, 0x01D5, 0x01D7, 
    0x01D7, 0x01D9, 0x01D9, 0x01DB, 0x01DB, 0x018E, 0x01DE, 0x01DE, 
    0x01E0, 0x01E0, 0x01E2, 0x01E2, 0x01E4, 0x01E4, 0x01E6, 0x01E6, 
    0x01E8, 0x01E8, 0x01EA, 0x01EA, 0x01EC, 0x01EC, 0x01EE, 0x01EE, 
    0x01F0, 0x01F1, 0x01F1, 0x01F1, 0x01F4, 0x01F4, 0x01F6, 0x01F7, 
    0x01F8, 0x01F8, 0x01FA, 0x01FA, 0x01FC, 0x01FC, 0x01FE, 0x01FE, 
    0x0200, 0x0200, 0x0202, 0x0202, 0x0204, 0x0204, 0x0206, 0x0206, 
    0x0208, 0x0208, 0x020A, 0x020A, 0x020C, 0x020C, 0x020E, 0x020E, 
    0x0210, 0x0210, 0x0212, 0x0212, 0x0214, 0x0214, 0x0216, 0x0216, 
    0x0218, 0x0218, 0x021A, 0x021A, 0x021C, 0x021C, 0x021E, 0x021E, 
    0x0220, 0x0221, 0x0222, 0x0222, 0x0224, 0x0224, 0x0226, 0x0226, 
    0x0228, 0x0228, 0x022A, 0x022A, 0x022C, 0x022C, 0x022E, 0x022E, 
    0x0230, 0x0230, 0x0232, 0x0232, 0x0234, 0x0235, 0x0236, 0x0237, 
    0x0238, 0x0239, 0x023A, 0x023B, 0x023C, 0x023D, 0x023E, 0x023F, 
    0x0240, 0x0241, 0x0242, 0x0243, 0x0244, 0x0245, 0x0246, 0x0247, 
    0x0248, 0x0249, 0x024A, 0x024B, 0x024C, 0x024D, 0x024E, 0x024F
  };

  const UCS4 Latin_ExtendedB180::m_lower[] = {
    0x0180, 0x0253, 0x0183, 0x0183, 0x0185, 0x0185, 0x0254, 0x0188, 
    0x0188, 0x0256, 0x0257, 0x018C, 0x018C, 0x018D, 0x01DD, 0x0259, 
    0x025B, 0x0192, 0x0192, 0x0260, 0x0263, 0x0195, 0x0269, 0x0268, 
    0x0199, 0x0199, 0x019A, 0x019B, 0x026F, 0x0272, 0x019E, 0x0275, 
    0x01A1, 0x01A1, 0x01A3, 0x01A3, 0x01A5, 0x01A5, 0x0280, 0x01A8, 
    0x01A8, 0x0283, 0x01AA, 0x01AB, 0x01AD, 0x01AD, 0x0288, 0x01B0, 
    0x01B0, 0x028A, 0x028B, 0x01B4, 0x01B4, 0x01B6, 0x01B6, 0x0292, 
    0x01B9, 0x01B9, 0x01BA, 0x01BB, 0x01BD, 0x01BD, 0x01BE, 0x01BF, 
    0x01C0, 0x01C1, 0x01C2, 0x01C3, 0x01C6, 0x01C6, 0x01C6, 0x01C9, 
    0x01C9, 0x01C9, 0x01CC, 0x01CC, 0x01CC, 0x01CE, 0x01CE, 0x01D0, 
    0x01D0, 0x01D2, 0x01D2, 0x01D4, 0x01D4, 0x01D6, 0x01D6, 0x01D8, 
    0x01D8, 0x01DA, 0x01DA, 0x01DC, 0x01DC, 0x01DD, 0x01DF, 0x01DF, 
    0x01E1, 0x01E1, 0x01E3, 0x01E3, 0x01E5, 0x01E5, 0x01E7, 0x01E7, 
    0x01E9, 0x01E9, 0x01EB, 0x01EB, 0x01ED, 0x01ED, 0x01EF, 0x01EF, 
    0x01F0, 0x01F3, 0x01F3, 0x01F3, 0x01F5, 0x01F5, 0x0195, 0x01BF, 
    0x01F9, 0x01F9, 0x01FB, 0x01FB, 0x01FD, 0x01FD, 0x01FF, 0x01FF, 
    0x0201, 0x0201, 0x0203, 0x0203, 0x0205, 0x0205, 0x0207, 0x0207, 
    0x0209, 0x0209, 0x020B, 0x020B, 0x020D, 0x020D, 0x020F, 0x020F, 
    0x0211, 0x0211, 0x0213, 0x0213, 0x0215, 0x0215, 0x0217, 0x0217, 
    0x0219, 0x0219, 0x021B, 0x021B, 0x021D, 0x021D, 0x021F, 0x021F, 
    0x0220, 0x0221, 0x0223, 0x0223, 0x0225, 0x0225, 0x0227, 0x0227, 
    0x0229, 0x0229, 0x022B, 0x022B, 0x022D, 0x022D, 0x022F, 0x022F, 
    0x0231, 0x0231, 0x0233, 0x0233, 0x0234, 0x0235, 0x0236, 0x0237, 
    0x0238, 0x0239, 0x023A, 0x023B, 0x023C, 0x023D, 0x023E, 0x023F, 
    0x0240, 0x0241, 0x0242, 0x0243, 0x0244, 0x0245, 0x0246, 0x0247, 
    0x0248, 0x0249, 0x024A, 0x024B, 0x024C, 0x024D, 0x024E, 0x024F
  };

  const UCS4 Latin_ExtendedB180::m_title[] = {
    0x0180, 0x0181, 0x0182, 0x0182, 0x0184, 0x0184, 0x0186, 0x0187, 
    0x0187, 0x0189, 0x018A, 0x018B, 0x018B, 0x018D, 0x018E, 0x018F, 
    0x0190, 0x0191, 0x0191, 0x0193, 0x0194, 0x01F6, 0x0196, 0x0197, 
    0x0198, 0x0198, 0x019A, 0x019B, 0x019C, 0x019D, 0x019E, 0x019F, 
    0x01A0, 0x01A0, 0x01A2, 0x01A2, 0x01A4, 0x01A4, 0x01A6, 0x01A7, 
    0x01A7, 0x01A9, 0x01AA, 0x01AB, 0x01AC, 0x01AC, 0x01AE, 0x01AF, 
    0x01AF, 0x01B1, 0x01B2, 0x01B3, 0x01B3, 0x01B5, 0x01B5, 0x01B7, 
    0x01B8, 0x01B8, 0x01BA, 0x01BB, 0x01BC, 0x01BC, 0x01BE, 0x01F7, 
    0x01C0, 0x01C1, 0x01C2, 0x01C3, 0x01C5, 0x01C5, 0x01C5, 0x01C8, 
    0x01C8, 0x01C8, 0x01CB, 0x01CB, 0x01CB, 0x01CD, 0x01CD, 0x01CF, 
    0x01CF, 0x01D1, 0x01D1, 0x01D3, 0x01D3, 0x01D5, 0x01D5, 0x01D7, 
    0x01D7, 0x01D9, 0x01D9, 0x01DB, 0x01DB, 0x018E, 0x01DE, 0x01DE, 
    0x01E0, 0x01E0, 0x01E2, 0x01E2, 0x01E4, 0x01E4, 0x01E6, 0x01E6, 
    0x01E8, 0x01E8, 0x01EA, 0x01EA, 0x01EC, 0x01EC, 0x01EE, 0x01EE, 
    0x01F0, 0x01F2, 0x01F2, 0x01F2, 0x01F4, 0x01F4, 0x01F6, 0x01F7, 
    0x01F8, 0x01F8, 0x01FA, 0x01FA, 0x01FC, 0x01FC, 0x01FE, 0x01FE, 
    0x0200, 0x0200, 0x0202, 0x0202, 0x0204, 0x0204, 0x0206, 0x0206, 
    0x0208, 0x0208, 0x020A, 0x020A, 0x020C, 0x020C, 0x020E, 0x020E, 
    0x0210, 0x0210, 0x0212, 0x0212, 0x0214, 0x0214, 0x0216, 0x0216, 
    0x0218, 0x0218, 0x021A, 0x021A, 0x021C, 0x021C, 0x021E, 0x021E, 
    0x0220, 0x0221, 0x0222, 0x0222, 0x0224, 0x0224, 0x0226, 0x0226, 
    0x0228, 0x0228, 0x022A, 0x022A, 0x022C, 0x022C, 0x022E, 0x022E, 
    0x0230, 0x0230, 0x0232, 0x0232, 0x0234, 0x0235, 0x0236, 0x0237, 
    0x0238, 0x0239, 0x023A, 0x023B, 0x023C, 0x023D, 0x023E, 0x023F, 
    0x0240, 0x0241, 0x0242, 0x0243, 0x0244, 0x0245, 0x0246, 0x0247, 
    0x0248, 0x0249, 0x024A, 0x024B, 0x024C, 0x024D, 0x024E, 0x024F
  };

  const unsigned char Latin_ExtendedB180::_cat[] = {
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lo, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lu, CAT_Lt, CAT_Ll, CAT_Lu, 
    CAT_Lt, CAT_Ll, CAT_Lu, CAT_Lt, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Lu, CAT_Lt, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll
  };

  const unsigned char Latin_ExtendedB180::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Latin_ExtendedB180::m_decompStr[][2] = {
    { 0x0180u, 0x0000u }, { 0x0181u, 0x0000u }, { 0x0182u, 0x0000u }, { 0x0183u, 0x0000u }, 
    { 0x0184u, 0x0000u }, { 0x0185u, 0x0000u }, { 0x0186u, 0x0000u }, { 0x0187u, 0x0000u }, 
    { 0x0188u, 0x0000u }, { 0x0189u, 0x0000u }, { 0x018Au, 0x0000u }, { 0x018Bu, 0x0000u }, 
    { 0x018Cu, 0x0000u }, { 0x018Du, 0x0000u }, { 0x018Eu, 0x0000u }, { 0x018Fu, 0x0000u }, 
    { 0x0190u, 0x0000u }, { 0x0191u, 0x0000u }, { 0x0192u, 0x0000u }, { 0x0193u, 0x0000u }, 
    { 0x0194u, 0x0000u }, { 0x0195u, 0x0000u }, { 0x0196u, 0x0000u }, { 0x0197u, 0x0000u }, 
    { 0x0198u, 0x0000u }, { 0x0199u, 0x0000u }, { 0x019Au, 0x0000u }, { 0x019Bu, 0x0000u }, 
    { 0x019Cu, 0x0000u }, { 0x019Du, 0x0000u }, { 0x019Eu, 0x0000u }, { 0x019Fu, 0x0000u }, 
    { 0x004Fu, 0x031Bu }, { 0x006Fu, 0x031Bu }, { 0x01A2u, 0x0000u }, { 0x01A3u, 0x0000u }, 
    { 0x01A4u, 0x0000u }, { 0x01A5u, 0x0000u }, { 0x01A6u, 0x0000u }, { 0x01A7u, 0x0000u }, 
    { 0x01A8u, 0x0000u }, { 0x01A9u, 0x0000u }, { 0x01AAu, 0x0000u }, { 0x01ABu, 0x0000u }, 
    { 0x01ACu, 0x0000u }, { 0x01ADu, 0x0000u }, { 0x01AEu, 0x0000u }, { 0x0055u, 0x031Bu }, 
    { 0x0075u, 0x031Bu }, { 0x01B1u, 0x0000u }, { 0x01B2u, 0x0000u }, { 0x01B3u, 0x0000u }, 
    { 0x01B4u, 0x0000u }, { 0x01B5u, 0x0000u }, { 0x01B6u, 0x0000u }, { 0x01B7u, 0x0000u }, 
    { 0x01B8u, 0x0000u }, { 0x01B9u, 0x0000u }, { 0x01BAu, 0x0000u }, { 0x01BBu, 0x0000u }, 
    { 0x01BCu, 0x0000u }, { 0x01BDu, 0x0000u }, { 0x01BEu, 0x0000u }, { 0x01BFu, 0x0000u }, 
    { 0x01C0u, 0x0000u }, { 0x01C1u, 0x0000u }, { 0x01C2u, 0x0000u }, { 0x01C3u, 0x0000u }, 
    { 0x0044u, 0x017Du }, { 0x0044u, 0x017Eu }, { 0x0064u, 0x017Eu }, { 0x004Cu, 0x004Au }, 
    { 0x004Cu, 0x006Au }, { 0x006Cu, 0x006Au }, { 0x004Eu, 0x004Au }, { 0x004Eu, 0x006Au }, 
    { 0x006Eu, 0x006Au }, { 0x0041u, 0x030Cu }, { 0x0061u, 0x030Cu }, { 0x0049u, 0x030Cu }, 
    { 0x0069u, 0x030Cu }, { 0x004Fu, 0x030Cu }, { 0x006Fu, 0x030Cu }, { 0x0055u, 0x030Cu }, 
    { 0x0075u, 0x030Cu }, { 0x00DCu, 0x0304u }, { 0x00FCu, 0x0304u }, { 0x00DCu, 0x0301u }, 
    { 0x00FCu, 0x0301u }, { 0x00DCu, 0x030Cu }, { 0x00FCu, 0x030Cu }, { 0x00DCu, 0x0300u }, 
    { 0x00FCu, 0x0300u }, { 0x01DDu, 0x0000u }, { 0x00C4u, 0x0304u }, { 0x00E4u, 0x0304u }, 
    { 0x0226u, 0x0304u }, { 0x0227u, 0x0304u }, { 0x00C6u, 0x0304u }, { 0x00E6u, 0x0304u }, 
    { 0x01E4u, 0x0000u }, { 0x01E5u, 0x0000u }, { 0x0047u, 0x030Cu }, { 0x0067u, 0x030Cu }, 
    { 0x004Bu, 0x030Cu }, { 0x006Bu, 0x030Cu }, { 0x004Fu, 0x0328u }, { 0x006Fu, 0x0328u }, 
    { 0x01EAu, 0x0304u }, { 0x01EBu, 0x0304u }, { 0x01B7u, 0x030Cu }, { 0x0292u, 0x030Cu }, 
    { 0x006Au, 0x030Cu }, { 0x0044u, 0x005Au }, { 0x0044u, 0x007Au }, { 0x0064u, 0x007Au }, 
    { 0x0047u, 0x0301u }, { 0x0067u, 0x0301u }, { 0x01F6u, 0x0000u }, { 0x01F7u, 0x0000u }, 
    { 0x004Eu, 0x0300u }, { 0x006Eu, 0x0300u }, { 0x00C5u, 0x0301u }, { 0x00E5u, 0x0301u }, 
    { 0x00C6u, 0x0301u }, { 0x00E6u, 0x0301u }, { 0x00D8u, 0x0301u }, { 0x00F8u, 0x0301u }, 
    { 0x0041u, 0x030Fu }, { 0x0061u, 0x030Fu }, { 0x0041u, 0x0311u }, { 0x0061u, 0x0311u }, 
    { 0x0045u, 0x030Fu }, { 0x0065u, 0x030Fu }, { 0x0045u, 0x0311u }, { 0x0065u, 0x0311u }, 
    { 0x0049u, 0x030Fu }, { 0x0069u, 0x030Fu }, { 0x0049u, 0x0311u }, { 0x0069u, 0x0311u }, 
    { 0x004Fu, 0x030Fu }, { 0x006Fu, 0x030Fu }, { 0x004Fu, 0x0311u }, { 0x006Fu, 0x0311u }, 
    { 0x0052u, 0x030Fu }, { 0x0072u, 0x030Fu }, { 0x0052u, 0x0311u }, { 0x0072u, 0x0311u }, 
    { 0x0055u, 0x030Fu }, { 0x0075u, 0x030Fu }, { 0x0055u, 0x0311u }, { 0x0075u, 0x0311u }, 
    { 0x0053u, 0x0326u }, { 0x0073u, 0x0326u }, { 0x0054u, 0x0326u }, { 0x0074u, 0x0326u }, 
    { 0x021Cu, 0x0000u }, { 0x021Du, 0x0000u }, { 0x0048u, 0x030Cu }, { 0x0068u, 0x030Cu }, 
    { 0x0220u, 0x0000u }, { 0x0221u, 0x0000u }, { 0x0222u, 0x0000u }, { 0x0223u, 0x0000u }, 
    { 0x0224u, 0x0000u }, { 0x0225u, 0x0000u }, { 0x0041u, 0x0307u }, { 0x0061u, 0x0307u }, 
    { 0x0045u, 0x0327u }, { 0x0065u, 0x0327u }, { 0x00D6u, 0x0304u }, { 0x00F6u, 0x0304u }, 
    { 0x00D5u, 0x0304u }, { 0x00F5u, 0x0304u }, { 0x004Fu, 0x0307u }, { 0x006Fu, 0x0307u }, 
    { 0x022Eu, 0x0304u }, { 0x022Fu, 0x0304u }, { 0x0059u, 0x0304u }, { 0x0079u, 0x0304u }, 
    { 0x0234u, 0x0000u }, { 0x0235u, 0x0000u }, { 0x0236u, 0x0000u }, { 0x0237u, 0x0000u }, 
    { 0x0238u, 0x0000u }, { 0x0239u, 0x0000u }, { 0x023Au, 0x0000u }, { 0x023Bu, 0x0000u }, 
    { 0x023Cu, 0x0000u }, { 0x023Du, 0x0000u }, { 0x023Eu, 0x0000u }, { 0x023Fu, 0x0000u }, 
    { 0x0240u, 0x0000u }, { 0x0241u, 0x0000u }, { 0x0242u, 0x0000u }, { 0x0243u, 0x0000u }, 
    { 0x0244u, 0x0000u }, { 0x0245u, 0x0000u }, { 0x0246u, 0x0000u }, { 0x0247u, 0x0000u }, 
    { 0x0248u, 0x0000u }, { 0x0249u, 0x0000u }, { 0x024Au, 0x0000u }, { 0x024Bu, 0x0000u }, 
    { 0x024Cu, 0x0000u }, { 0x024Du, 0x0000u }, { 0x024Eu, 0x0000u }, { 0x024Fu, 0x0000u }
  };

  const unsigned char Latin_ExtendedB180::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, 
    LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, 
    LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Latin_ExtendedB180::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

}; // namespace Babylon

dload(Babylon::Latin_ExtendedB180);
