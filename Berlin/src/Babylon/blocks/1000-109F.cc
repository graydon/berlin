/*$Id: 1000-109F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:35 +0200.
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

  class Myanmar1000 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Myanmar1000() {
      m_first_letter = 0x1000;
      m_last_letter  = 0x109F;
      // m_version="3.1" // Not yet supported!

    }


    ~Myanmar1000() {
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
      return "Myanmar";
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
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x1040u:
        return 0;
        break;
      case 0x1041u:
        return 1;
        break;
      case 0x1042u:
        return 2;
        break;
      case 0x1043u:
        return 3;
        break;
      case 0x1044u:
        return 4;
        break;
      case 0x1045u:
        return 5;
        break;
      case 0x1046u:
        return 6;
        break;
      case 0x1047u:
        return 7;
        break;
      case 0x1048u:
        return 8;
        break;
      case 0x1049u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x1040u:
      case 0x1041u:
      case 0x1042u:
      case 0x1043u:
      case 0x1044u:
      case 0x1045u:
      case 0x1046u:
      case 0x1047u:
      case 0x1048u:
      case 0x1049u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x1040u:
        return 0;
        break;
      case 0x1041u:
        return 1;
        break;
      case 0x1042u:
        return 2;
        break;
      case 0x1043u:
        return 3;
        break;
      case 0x1044u:
        return 4;
        break;
      case 0x1045u:
        return 5;
        break;
      case 0x1046u:
        return 6;
        break;
      case 0x1047u:
        return 7;
        break;
      case 0x1048u:
        return 8;
        break;
      case 0x1049u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x1040u:
      case 0x1041u:
      case 0x1042u:
      case 0x1043u:
      case 0x1044u:
      case 0x1045u:
      case 0x1046u:
      case 0x1047u:
      case 0x1048u:
      case 0x1049u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x1040u:
        return 0.000000;
        break;
      case 0x1041u:
        return 1.000000;
        break;
      case 0x1042u:
        return 2.000000;
        break;
      case 0x1043u:
        return 3.000000;
        break;
      case 0x1044u:
        return 4.000000;
        break;
      case 0x1045u:
        return 5.000000;
        break;
      case 0x1046u:
        return 6.000000;
        break;
      case 0x1047u:
        return 7.000000;
        break;
      case 0x1048u:
        return 8.000000;
        break;
      case 0x1049u:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x1040u:
      case 0x1041u:
      case 0x1042u:
      case 0x1043u:
      case 0x1044u:
      case 0x1045u:
      case 0x1046u:
      case 0x1047u:
      case 0x1048u:
      case 0x1049u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Myanmar1000::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Myanmar1000::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Myanmar1000::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Myanmar1000::m_decompStr[uc - m_first_letter][0];
      us[1] = Myanmar1000::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Myanmar1000::m_lb[uc - m_first_letter]);
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
      return m_Terminal_Punctuation.test(uc - m_first_letter);
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
    Myanmar1000(const Myanmar1000 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<160> m_is_defined;
    static const unsigned char _cat[160];
    static const unsigned char _comb_cl[160];
    static const unsigned char m_bidir[160];
    static const UCS2 m_decompStr[160][2];
    static const unsigned char m_lb[160];
    static const std::bitset<160> m_Terminal_Punctuation;
    static const std::bitset<160> m_Other_Alphabetic;

  }; // class Myanmar1000

    const std::bitset<160> Myanmar1000::m_is_defined(std::string("0000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111110000001111000111111101101111101111111111111111111111111111111111"));

  const unsigned char Myanmar1000::_cat[] = {
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mc, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, 
    CAT_Mc, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Mc, 
    CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo
  };

  const unsigned char Myanmar1000::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 7, 
    0, 9, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Myanmar1000::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS2 Myanmar1000::m_decompStr[][2] = {
    { 0x1000u, 0x0000u }, { 0x1001u, 0x0000u }, { 0x1002u, 0x0000u }, { 0x1003u, 0x0000u }, 
    { 0x1004u, 0x0000u }, { 0x1005u, 0x0000u }, { 0x1006u, 0x0000u }, { 0x1007u, 0x0000u }, 
    { 0x1008u, 0x0000u }, { 0x1009u, 0x0000u }, { 0x100Au, 0x0000u }, { 0x100Bu, 0x0000u }, 
    { 0x100Cu, 0x0000u }, { 0x100Du, 0x0000u }, { 0x100Eu, 0x0000u }, { 0x100Fu, 0x0000u }, 
    { 0x1010u, 0x0000u }, { 0x1011u, 0x0000u }, { 0x1012u, 0x0000u }, { 0x1013u, 0x0000u }, 
    { 0x1014u, 0x0000u }, { 0x1015u, 0x0000u }, { 0x1016u, 0x0000u }, { 0x1017u, 0x0000u }, 
    { 0x1018u, 0x0000u }, { 0x1019u, 0x0000u }, { 0x101Au, 0x0000u }, { 0x101Bu, 0x0000u }, 
    { 0x101Cu, 0x0000u }, { 0x101Du, 0x0000u }, { 0x101Eu, 0x0000u }, { 0x101Fu, 0x0000u }, 
    { 0x1020u, 0x0000u }, { 0x1021u, 0x0000u }, { 0x1022u, 0x0000u }, { 0x1023u, 0x0000u }, 
    { 0x1024u, 0x0000u }, { 0x1025u, 0x0000u }, { 0x1025u, 0x102Eu }, { 0x1027u, 0x0000u }, 
    { 0x1028u, 0x0000u }, { 0x1029u, 0x0000u }, { 0x102Au, 0x0000u }, { 0x102Bu, 0x0000u }, 
    { 0x102Cu, 0x0000u }, { 0x102Du, 0x0000u }, { 0x102Eu, 0x0000u }, { 0x102Fu, 0x0000u }, 
    { 0x1030u, 0x0000u }, { 0x1031u, 0x0000u }, { 0x1032u, 0x0000u }, { 0x1033u, 0x0000u }, 
    { 0x1034u, 0x0000u }, { 0x1035u, 0x0000u }, { 0x1036u, 0x0000u }, { 0x1037u, 0x0000u }, 
    { 0x1038u, 0x0000u }, { 0x1039u, 0x0000u }, { 0x103Au, 0x0000u }, { 0x103Bu, 0x0000u }, 
    { 0x103Cu, 0x0000u }, { 0x103Du, 0x0000u }, { 0x103Eu, 0x0000u }, { 0x103Fu, 0x0000u }, 
    { 0x1040u, 0x0000u }, { 0x1041u, 0x0000u }, { 0x1042u, 0x0000u }, { 0x1043u, 0x0000u }, 
    { 0x1044u, 0x0000u }, { 0x1045u, 0x0000u }, { 0x1046u, 0x0000u }, { 0x1047u, 0x0000u }, 
    { 0x1048u, 0x0000u }, { 0x1049u, 0x0000u }, { 0x104Au, 0x0000u }, { 0x104Bu, 0x0000u }, 
    { 0x104Cu, 0x0000u }, { 0x104Du, 0x0000u }, { 0x104Eu, 0x0000u }, { 0x104Fu, 0x0000u }, 
    { 0x1050u, 0x0000u }, { 0x1051u, 0x0000u }, { 0x1052u, 0x0000u }, { 0x1053u, 0x0000u }, 
    { 0x1054u, 0x0000u }, { 0x1055u, 0x0000u }, { 0x1056u, 0x0000u }, { 0x1057u, 0x0000u }, 
    { 0x1058u, 0x0000u }, { 0x1059u, 0x0000u }, { 0x105Au, 0x0000u }, { 0x105Bu, 0x0000u }, 
    { 0x105Cu, 0x0000u }, { 0x105Du, 0x0000u }, { 0x105Eu, 0x0000u }, { 0x105Fu, 0x0000u }, 
    { 0x1060u, 0x0000u }, { 0x1061u, 0x0000u }, { 0x1062u, 0x0000u }, { 0x1063u, 0x0000u }, 
    { 0x1064u, 0x0000u }, { 0x1065u, 0x0000u }, { 0x1066u, 0x0000u }, { 0x1067u, 0x0000u }, 
    { 0x1068u, 0x0000u }, { 0x1069u, 0x0000u }, { 0x106Au, 0x0000u }, { 0x106Bu, 0x0000u }, 
    { 0x106Cu, 0x0000u }, { 0x106Du, 0x0000u }, { 0x106Eu, 0x0000u }, { 0x106Fu, 0x0000u }, 
    { 0x1070u, 0x0000u }, { 0x1071u, 0x0000u }, { 0x1072u, 0x0000u }, { 0x1073u, 0x0000u }, 
    { 0x1074u, 0x0000u }, { 0x1075u, 0x0000u }, { 0x1076u, 0x0000u }, { 0x1077u, 0x0000u }, 
    { 0x1078u, 0x0000u }, { 0x1079u, 0x0000u }, { 0x107Au, 0x0000u }, { 0x107Bu, 0x0000u }, 
    { 0x107Cu, 0x0000u }, { 0x107Du, 0x0000u }, { 0x107Eu, 0x0000u }, { 0x107Fu, 0x0000u }, 
    { 0x1080u, 0x0000u }, { 0x1081u, 0x0000u }, { 0x1082u, 0x0000u }, { 0x1083u, 0x0000u }, 
    { 0x1084u, 0x0000u }, { 0x1085u, 0x0000u }, { 0x1086u, 0x0000u }, { 0x1087u, 0x0000u }, 
    { 0x1088u, 0x0000u }, { 0x1089u, 0x0000u }, { 0x108Au, 0x0000u }, { 0x108Bu, 0x0000u }, 
    { 0x108Cu, 0x0000u }, { 0x108Du, 0x0000u }, { 0x108Eu, 0x0000u }, { 0x108Fu, 0x0000u }, 
    { 0x1090u, 0x0000u }, { 0x1091u, 0x0000u }, { 0x1092u, 0x0000u }, { 0x1093u, 0x0000u }, 
    { 0x1094u, 0x0000u }, { 0x1095u, 0x0000u }, { 0x1096u, 0x0000u }, { 0x1097u, 0x0000u }, 
    { 0x1098u, 0x0000u }, { 0x1099u, 0x0000u }, { 0x109Au, 0x0000u }, { 0x109Bu, 0x0000u }, 
    { 0x109Cu, 0x0000u }, { 0x109Du, 0x0000u }, { 0x109Eu, 0x0000u }, { 0x109Fu, 0x0000u }
  };

  const unsigned char Myanmar1000::m_lb[] = {
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_SA, LB_SA, LB_SA, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, 
    LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA, LB_SA
  };

    const std::bitset<160> Myanmar1000::m_Terminal_Punctuation(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<160> Myanmar1000::m_Other_Alphabetic(std::string("0000000000000000000000000000000000000000000000000000000000000000000000111100000000000000000000000000000000000001111000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Myanmar1000);
