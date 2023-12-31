/*$Id: 1E00-1EFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:25 +0200.
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

  class Latin_Extended_Additional1E00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Latin_Extended_Additional1E00() {
      m_first_letter = 0x1E00;
      m_last_letter  = 0x1EFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00001E36, 0x00000304)] = 0x1E38;
      m_composeMap[make_pair(0x00001E37, 0x00000304)] = 0x1E39;
      m_composeMap[make_pair(0x00001E5A, 0x00000304)] = 0x1E5C;
      m_composeMap[make_pair(0x00001E5B, 0x00000304)] = 0x1E5D;
      m_composeMap[make_pair(0x00001E62, 0x00000307)] = 0x1E68;
      m_composeMap[make_pair(0x00001E63, 0x00000307)] = 0x1E69;
      m_composeMap[make_pair(0x00001EA0, 0x00000302)] = 0x1EAC;
      m_composeMap[make_pair(0x00001EA0, 0x00000306)] = 0x1EB6;
      m_composeMap[make_pair(0x00001EA1, 0x00000302)] = 0x1EAD;
      m_composeMap[make_pair(0x00001EA1, 0x00000306)] = 0x1EB7;
      m_composeMap[make_pair(0x00001EB8, 0x00000302)] = 0x1EC6;
      m_composeMap[make_pair(0x00001EB9, 0x00000302)] = 0x1EC7;
      m_composeMap[make_pair(0x00001ECC, 0x00000302)] = 0x1ED8;
      m_composeMap[make_pair(0x00001ECD, 0x00000302)] = 0x1ED9;

    }


    ~Latin_Extended_Additional1E00() {
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
      return "Latin Extended Additional";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Latin_Extended_Additional1E00::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Latin_Extended_Additional1E00::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Latin_Extended_Additional1E00::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Latin_Extended_Additional1E00::_cat[uc - m_first_letter]);
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
      return Babylon::Char_Decomp(Latin_Extended_Additional1E00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Latin_Extended_Additional1E00::m_decompStr[uc - m_first_letter][0];
      us[1] = Latin_Extended_Additional1E00::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(LB_AL);
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
    Latin_Extended_Additional1E00(const Latin_Extended_Additional1E00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const UCS4 m_upper[256];
    static const UCS4 m_lower[256];
    static const UCS4 m_title[256];
    static const unsigned char _cat[256];
    static const unsigned char _decomp[256];
    static const UCS2 m_decompStr[256][2];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Latin_Extended_Additional1E00

    const std::bitset<256> Latin_Extended_Additional1E00::m_is_defined(std::string("0000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const UCS4 Latin_Extended_Additional1E00::m_upper[] = {
    0x1E00, 0x1E00, 0x1E02, 0x1E02, 0x1E04, 0x1E04, 0x1E06, 0x1E06, 
    0x1E08, 0x1E08, 0x1E0A, 0x1E0A, 0x1E0C, 0x1E0C, 0x1E0E, 0x1E0E, 
    0x1E10, 0x1E10, 0x1E12, 0x1E12, 0x1E14, 0x1E14, 0x1E16, 0x1E16, 
    0x1E18, 0x1E18, 0x1E1A, 0x1E1A, 0x1E1C, 0x1E1C, 0x1E1E, 0x1E1E, 
    0x1E20, 0x1E20, 0x1E22, 0x1E22, 0x1E24, 0x1E24, 0x1E26, 0x1E26, 
    0x1E28, 0x1E28, 0x1E2A, 0x1E2A, 0x1E2C, 0x1E2C, 0x1E2E, 0x1E2E, 
    0x1E30, 0x1E30, 0x1E32, 0x1E32, 0x1E34, 0x1E34, 0x1E36, 0x1E36, 
    0x1E38, 0x1E38, 0x1E3A, 0x1E3A, 0x1E3C, 0x1E3C, 0x1E3E, 0x1E3E, 
    0x1E40, 0x1E40, 0x1E42, 0x1E42, 0x1E44, 0x1E44, 0x1E46, 0x1E46, 
    0x1E48, 0x1E48, 0x1E4A, 0x1E4A, 0x1E4C, 0x1E4C, 0x1E4E, 0x1E4E, 
    0x1E50, 0x1E50, 0x1E52, 0x1E52, 0x1E54, 0x1E54, 0x1E56, 0x1E56, 
    0x1E58, 0x1E58, 0x1E5A, 0x1E5A, 0x1E5C, 0x1E5C, 0x1E5E, 0x1E5E, 
    0x1E60, 0x1E60, 0x1E62, 0x1E62, 0x1E64, 0x1E64, 0x1E66, 0x1E66, 
    0x1E68, 0x1E68, 0x1E6A, 0x1E6A, 0x1E6C, 0x1E6C, 0x1E6E, 0x1E6E, 
    0x1E70, 0x1E70, 0x1E72, 0x1E72, 0x1E74, 0x1E74, 0x1E76, 0x1E76, 
    0x1E78, 0x1E78, 0x1E7A, 0x1E7A, 0x1E7C, 0x1E7C, 0x1E7E, 0x1E7E, 
    0x1E80, 0x1E80, 0x1E82, 0x1E82, 0x1E84, 0x1E84, 0x1E86, 0x1E86, 
    0x1E88, 0x1E88, 0x1E8A, 0x1E8A, 0x1E8C, 0x1E8C, 0x1E8E, 0x1E8E, 
    0x1E90, 0x1E90, 0x1E92, 0x1E92, 0x1E94, 0x1E94, 0x1E96, 0x1E97, 
    0x1E98, 0x1E99, 0x1E9A, 0x1E60, 0x1E9C, 0x1E9D, 0x1E9E, 0x1E9F, 
    0x1EA0, 0x1EA0, 0x1EA2, 0x1EA2, 0x1EA4, 0x1EA4, 0x1EA6, 0x1EA6, 
    0x1EA8, 0x1EA8, 0x1EAA, 0x1EAA, 0x1EAC, 0x1EAC, 0x1EAE, 0x1EAE, 
    0x1EB0, 0x1EB0, 0x1EB2, 0x1EB2, 0x1EB4, 0x1EB4, 0x1EB6, 0x1EB6, 
    0x1EB8, 0x1EB8, 0x1EBA, 0x1EBA, 0x1EBC, 0x1EBC, 0x1EBE, 0x1EBE, 
    0x1EC0, 0x1EC0, 0x1EC2, 0x1EC2, 0x1EC4, 0x1EC4, 0x1EC6, 0x1EC6, 
    0x1EC8, 0x1EC8, 0x1ECA, 0x1ECA, 0x1ECC, 0x1ECC, 0x1ECE, 0x1ECE, 
    0x1ED0, 0x1ED0, 0x1ED2, 0x1ED2, 0x1ED4, 0x1ED4, 0x1ED6, 0x1ED6, 
    0x1ED8, 0x1ED8, 0x1EDA, 0x1EDA, 0x1EDC, 0x1EDC, 0x1EDE, 0x1EDE, 
    0x1EE0, 0x1EE0, 0x1EE2, 0x1EE2, 0x1EE4, 0x1EE4, 0x1EE6, 0x1EE6, 
    0x1EE8, 0x1EE8, 0x1EEA, 0x1EEA, 0x1EEC, 0x1EEC, 0x1EEE, 0x1EEE, 
    0x1EF0, 0x1EF0, 0x1EF2, 0x1EF2, 0x1EF4, 0x1EF4, 0x1EF6, 0x1EF6, 
    0x1EF8, 0x1EF8, 0x1EFA, 0x1EFB, 0x1EFC, 0x1EFD, 0x1EFE, 0x1EFF
  };

  const UCS4 Latin_Extended_Additional1E00::m_lower[] = {
    0x1E01, 0x1E01, 0x1E03, 0x1E03, 0x1E05, 0x1E05, 0x1E07, 0x1E07, 
    0x1E09, 0x1E09, 0x1E0B, 0x1E0B, 0x1E0D, 0x1E0D, 0x1E0F, 0x1E0F, 
    0x1E11, 0x1E11, 0x1E13, 0x1E13, 0x1E15, 0x1E15, 0x1E17, 0x1E17, 
    0x1E19, 0x1E19, 0x1E1B, 0x1E1B, 0x1E1D, 0x1E1D, 0x1E1F, 0x1E1F, 
    0x1E21, 0x1E21, 0x1E23, 0x1E23, 0x1E25, 0x1E25, 0x1E27, 0x1E27, 
    0x1E29, 0x1E29, 0x1E2B, 0x1E2B, 0x1E2D, 0x1E2D, 0x1E2F, 0x1E2F, 
    0x1E31, 0x1E31, 0x1E33, 0x1E33, 0x1E35, 0x1E35, 0x1E37, 0x1E37, 
    0x1E39, 0x1E39, 0x1E3B, 0x1E3B, 0x1E3D, 0x1E3D, 0x1E3F, 0x1E3F, 
    0x1E41, 0x1E41, 0x1E43, 0x1E43, 0x1E45, 0x1E45, 0x1E47, 0x1E47, 
    0x1E49, 0x1E49, 0x1E4B, 0x1E4B, 0x1E4D, 0x1E4D, 0x1E4F, 0x1E4F, 
    0x1E51, 0x1E51, 0x1E53, 0x1E53, 0x1E55, 0x1E55, 0x1E57, 0x1E57, 
    0x1E59, 0x1E59, 0x1E5B, 0x1E5B, 0x1E5D, 0x1E5D, 0x1E5F, 0x1E5F, 
    0x1E61, 0x1E61, 0x1E63, 0x1E63, 0x1E65, 0x1E65, 0x1E67, 0x1E67, 
    0x1E69, 0x1E69, 0x1E6B, 0x1E6B, 0x1E6D, 0x1E6D, 0x1E6F, 0x1E6F, 
    0x1E71, 0x1E71, 0x1E73, 0x1E73, 0x1E75, 0x1E75, 0x1E77, 0x1E77, 
    0x1E79, 0x1E79, 0x1E7B, 0x1E7B, 0x1E7D, 0x1E7D, 0x1E7F, 0x1E7F, 
    0x1E81, 0x1E81, 0x1E83, 0x1E83, 0x1E85, 0x1E85, 0x1E87, 0x1E87, 
    0x1E89, 0x1E89, 0x1E8B, 0x1E8B, 0x1E8D, 0x1E8D, 0x1E8F, 0x1E8F, 
    0x1E91, 0x1E91, 0x1E93, 0x1E93, 0x1E95, 0x1E95, 0x1E96, 0x1E97, 
    0x1E98, 0x1E99, 0x1E9A, 0x1E9B, 0x1E9C, 0x1E9D, 0x1E9E, 0x1E9F, 
    0x1EA1, 0x1EA1, 0x1EA3, 0x1EA3, 0x1EA5, 0x1EA5, 0x1EA7, 0x1EA7, 
    0x1EA9, 0x1EA9, 0x1EAB, 0x1EAB, 0x1EAD, 0x1EAD, 0x1EAF, 0x1EAF, 
    0x1EB1, 0x1EB1, 0x1EB3, 0x1EB3, 0x1EB5, 0x1EB5, 0x1EB7, 0x1EB7, 
    0x1EB9, 0x1EB9, 0x1EBB, 0x1EBB, 0x1EBD, 0x1EBD, 0x1EBF, 0x1EBF, 
    0x1EC1, 0x1EC1, 0x1EC3, 0x1EC3, 0x1EC5, 0x1EC5, 0x1EC7, 0x1EC7, 
    0x1EC9, 0x1EC9, 0x1ECB, 0x1ECB, 0x1ECD, 0x1ECD, 0x1ECF, 0x1ECF, 
    0x1ED1, 0x1ED1, 0x1ED3, 0x1ED3, 0x1ED5, 0x1ED5, 0x1ED7, 0x1ED7, 
    0x1ED9, 0x1ED9, 0x1EDB, 0x1EDB, 0x1EDD, 0x1EDD, 0x1EDF, 0x1EDF, 
    0x1EE1, 0x1EE1, 0x1EE3, 0x1EE3, 0x1EE5, 0x1EE5, 0x1EE7, 0x1EE7, 
    0x1EE9, 0x1EE9, 0x1EEB, 0x1EEB, 0x1EED, 0x1EED, 0x1EEF, 0x1EEF, 
    0x1EF1, 0x1EF1, 0x1EF3, 0x1EF3, 0x1EF5, 0x1EF5, 0x1EF7, 0x1EF7, 
    0x1EF9, 0x1EF9, 0x1EFA, 0x1EFB, 0x1EFC, 0x1EFD, 0x1EFE, 0x1EFF
  };

  const UCS4 Latin_Extended_Additional1E00::m_title[] = {
    0x1E00, 0x1E00, 0x1E02, 0x1E02, 0x1E04, 0x1E04, 0x1E06, 0x1E06, 
    0x1E08, 0x1E08, 0x1E0A, 0x1E0A, 0x1E0C, 0x1E0C, 0x1E0E, 0x1E0E, 
    0x1E10, 0x1E10, 0x1E12, 0x1E12, 0x1E14, 0x1E14, 0x1E16, 0x1E16, 
    0x1E18, 0x1E18, 0x1E1A, 0x1E1A, 0x1E1C, 0x1E1C, 0x1E1E, 0x1E1E, 
    0x1E20, 0x1E20, 0x1E22, 0x1E22, 0x1E24, 0x1E24, 0x1E26, 0x1E26, 
    0x1E28, 0x1E28, 0x1E2A, 0x1E2A, 0x1E2C, 0x1E2C, 0x1E2E, 0x1E2E, 
    0x1E30, 0x1E30, 0x1E32, 0x1E32, 0x1E34, 0x1E34, 0x1E36, 0x1E36, 
    0x1E38, 0x1E38, 0x1E3A, 0x1E3A, 0x1E3C, 0x1E3C, 0x1E3E, 0x1E3E, 
    0x1E40, 0x1E40, 0x1E42, 0x1E42, 0x1E44, 0x1E44, 0x1E46, 0x1E46, 
    0x1E48, 0x1E48, 0x1E4A, 0x1E4A, 0x1E4C, 0x1E4C, 0x1E4E, 0x1E4E, 
    0x1E50, 0x1E50, 0x1E52, 0x1E52, 0x1E54, 0x1E54, 0x1E56, 0x1E56, 
    0x1E58, 0x1E58, 0x1E5A, 0x1E5A, 0x1E5C, 0x1E5C, 0x1E5E, 0x1E5E, 
    0x1E60, 0x1E60, 0x1E62, 0x1E62, 0x1E64, 0x1E64, 0x1E66, 0x1E66, 
    0x1E68, 0x1E68, 0x1E6A, 0x1E6A, 0x1E6C, 0x1E6C, 0x1E6E, 0x1E6E, 
    0x1E70, 0x1E70, 0x1E72, 0x1E72, 0x1E74, 0x1E74, 0x1E76, 0x1E76, 
    0x1E78, 0x1E78, 0x1E7A, 0x1E7A, 0x1E7C, 0x1E7C, 0x1E7E, 0x1E7E, 
    0x1E80, 0x1E80, 0x1E82, 0x1E82, 0x1E84, 0x1E84, 0x1E86, 0x1E86, 
    0x1E88, 0x1E88, 0x1E8A, 0x1E8A, 0x1E8C, 0x1E8C, 0x1E8E, 0x1E8E, 
    0x1E90, 0x1E90, 0x1E92, 0x1E92, 0x1E94, 0x1E94, 0x1E96, 0x1E97, 
    0x1E98, 0x1E99, 0x1E9A, 0x1E60, 0x1E9C, 0x1E9D, 0x1E9E, 0x1E9F, 
    0x1EA0, 0x1EA0, 0x1EA2, 0x1EA2, 0x1EA4, 0x1EA4, 0x1EA6, 0x1EA6, 
    0x1EA8, 0x1EA8, 0x1EAA, 0x1EAA, 0x1EAC, 0x1EAC, 0x1EAE, 0x1EAE, 
    0x1EB0, 0x1EB0, 0x1EB2, 0x1EB2, 0x1EB4, 0x1EB4, 0x1EB6, 0x1EB6, 
    0x1EB8, 0x1EB8, 0x1EBA, 0x1EBA, 0x1EBC, 0x1EBC, 0x1EBE, 0x1EBE, 
    0x1EC0, 0x1EC0, 0x1EC2, 0x1EC2, 0x1EC4, 0x1EC4, 0x1EC6, 0x1EC6, 
    0x1EC8, 0x1EC8, 0x1ECA, 0x1ECA, 0x1ECC, 0x1ECC, 0x1ECE, 0x1ECE, 
    0x1ED0, 0x1ED0, 0x1ED2, 0x1ED2, 0x1ED4, 0x1ED4, 0x1ED6, 0x1ED6, 
    0x1ED8, 0x1ED8, 0x1EDA, 0x1EDA, 0x1EDC, 0x1EDC, 0x1EDE, 0x1EDE, 
    0x1EE0, 0x1EE0, 0x1EE2, 0x1EE2, 0x1EE4, 0x1EE4, 0x1EE6, 0x1EE6, 
    0x1EE8, 0x1EE8, 0x1EEA, 0x1EEA, 0x1EEC, 0x1EEC, 0x1EEE, 0x1EEE, 
    0x1EF0, 0x1EF0, 0x1EF2, 0x1EF2, 0x1EF4, 0x1EF4, 0x1EF6, 0x1EF6, 
    0x1EF8, 0x1EF8, 0x1EFA, 0x1EFB, 0x1EFC, 0x1EFD, 0x1EFE, 0x1EFF
  };

  const unsigned char Latin_Extended_Additional1E00::_cat[] = {
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu
  };

  const unsigned char Latin_Extended_Additional1E00::_decomp[] = {
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Latin_Extended_Additional1E00::m_decompStr[][2] = {
    { 0x0041u, 0x0325u }, { 0x0061u, 0x0325u }, { 0x0042u, 0x0307u }, { 0x0062u, 0x0307u }, 
    { 0x0042u, 0x0323u }, { 0x0062u, 0x0323u }, { 0x0042u, 0x0331u }, { 0x0062u, 0x0331u }, 
    { 0x00C7u, 0x0301u }, { 0x00E7u, 0x0301u }, { 0x0044u, 0x0307u }, { 0x0064u, 0x0307u }, 
    { 0x0044u, 0x0323u }, { 0x0064u, 0x0323u }, { 0x0044u, 0x0331u }, { 0x0064u, 0x0331u }, 
    { 0x0044u, 0x0327u }, { 0x0064u, 0x0327u }, { 0x0044u, 0x032Du }, { 0x0064u, 0x032Du }, 
    { 0x0112u, 0x0300u }, { 0x0113u, 0x0300u }, { 0x0112u, 0x0301u }, { 0x0113u, 0x0301u }, 
    { 0x0045u, 0x032Du }, { 0x0065u, 0x032Du }, { 0x0045u, 0x0330u }, { 0x0065u, 0x0330u }, 
    { 0x0228u, 0x0306u }, { 0x0229u, 0x0306u }, { 0x0046u, 0x0307u }, { 0x0066u, 0x0307u }, 
    { 0x0047u, 0x0304u }, { 0x0067u, 0x0304u }, { 0x0048u, 0x0307u }, { 0x0068u, 0x0307u }, 
    { 0x0048u, 0x0323u }, { 0x0068u, 0x0323u }, { 0x0048u, 0x0308u }, { 0x0068u, 0x0308u }, 
    { 0x0048u, 0x0327u }, { 0x0068u, 0x0327u }, { 0x0048u, 0x032Eu }, { 0x0068u, 0x032Eu }, 
    { 0x0049u, 0x0330u }, { 0x0069u, 0x0330u }, { 0x00CFu, 0x0301u }, { 0x00EFu, 0x0301u }, 
    { 0x004Bu, 0x0301u }, { 0x006Bu, 0x0301u }, { 0x004Bu, 0x0323u }, { 0x006Bu, 0x0323u }, 
    { 0x004Bu, 0x0331u }, { 0x006Bu, 0x0331u }, { 0x004Cu, 0x0323u }, { 0x006Cu, 0x0323u }, 
    { 0x1E36u, 0x0304u }, { 0x1E37u, 0x0304u }, { 0x004Cu, 0x0331u }, { 0x006Cu, 0x0331u }, 
    { 0x004Cu, 0x032Du }, { 0x006Cu, 0x032Du }, { 0x004Du, 0x0301u }, { 0x006Du, 0x0301u }, 
    { 0x004Du, 0x0307u }, { 0x006Du, 0x0307u }, { 0x004Du, 0x0323u }, { 0x006Du, 0x0323u }, 
    { 0x004Eu, 0x0307u }, { 0x006Eu, 0x0307u }, { 0x004Eu, 0x0323u }, { 0x006Eu, 0x0323u }, 
    { 0x004Eu, 0x0331u }, { 0x006Eu, 0x0331u }, { 0x004Eu, 0x032Du }, { 0x006Eu, 0x032Du }, 
    { 0x00D5u, 0x0301u }, { 0x00F5u, 0x0301u }, { 0x00D5u, 0x0308u }, { 0x00F5u, 0x0308u }, 
    { 0x014Cu, 0x0300u }, { 0x014Du, 0x0300u }, { 0x014Cu, 0x0301u }, { 0x014Du, 0x0301u }, 
    { 0x0050u, 0x0301u }, { 0x0070u, 0x0301u }, { 0x0050u, 0x0307u }, { 0x0070u, 0x0307u }, 
    { 0x0052u, 0x0307u }, { 0x0072u, 0x0307u }, { 0x0052u, 0x0323u }, { 0x0072u, 0x0323u }, 
    { 0x1E5Au, 0x0304u }, { 0x1E5Bu, 0x0304u }, { 0x0052u, 0x0331u }, { 0x0072u, 0x0331u }, 
    { 0x0053u, 0x0307u }, { 0x0073u, 0x0307u }, { 0x0053u, 0x0323u }, { 0x0073u, 0x0323u }, 
    { 0x015Au, 0x0307u }, { 0x015Bu, 0x0307u }, { 0x0160u, 0x0307u }, { 0x0161u, 0x0307u }, 
    { 0x1E62u, 0x0307u }, { 0x1E63u, 0x0307u }, { 0x0054u, 0x0307u }, { 0x0074u, 0x0307u }, 
    { 0x0054u, 0x0323u }, { 0x0074u, 0x0323u }, { 0x0054u, 0x0331u }, { 0x0074u, 0x0331u }, 
    { 0x0054u, 0x032Du }, { 0x0074u, 0x032Du }, { 0x0055u, 0x0324u }, { 0x0075u, 0x0324u }, 
    { 0x0055u, 0x0330u }, { 0x0075u, 0x0330u }, { 0x0055u, 0x032Du }, { 0x0075u, 0x032Du }, 
    { 0x0168u, 0x0301u }, { 0x0169u, 0x0301u }, { 0x016Au, 0x0308u }, { 0x016Bu, 0x0308u }, 
    { 0x0056u, 0x0303u }, { 0x0076u, 0x0303u }, { 0x0056u, 0x0323u }, { 0x0076u, 0x0323u }, 
    { 0x0057u, 0x0300u }, { 0x0077u, 0x0300u }, { 0x0057u, 0x0301u }, { 0x0077u, 0x0301u }, 
    { 0x0057u, 0x0308u }, { 0x0077u, 0x0308u }, { 0x0057u, 0x0307u }, { 0x0077u, 0x0307u }, 
    { 0x0057u, 0x0323u }, { 0x0077u, 0x0323u }, { 0x0058u, 0x0307u }, { 0x0078u, 0x0307u }, 
    { 0x0058u, 0x0308u }, { 0x0078u, 0x0308u }, { 0x0059u, 0x0307u }, { 0x0079u, 0x0307u }, 
    { 0x005Au, 0x0302u }, { 0x007Au, 0x0302u }, { 0x005Au, 0x0323u }, { 0x007Au, 0x0323u }, 
    { 0x005Au, 0x0331u }, { 0x007Au, 0x0331u }, { 0x0068u, 0x0331u }, { 0x0074u, 0x0308u }, 
    { 0x0077u, 0x030Au }, { 0x0079u, 0x030Au }, { 0x0061u, 0x02BEu }, { 0x017Fu, 0x0307u }, 
    { 0x1E9Cu, 0x0000u }, { 0x1E9Du, 0x0000u }, { 0x1E9Eu, 0x0000u }, { 0x1E9Fu, 0x0000u }, 
    { 0x0041u, 0x0323u }, { 0x0061u, 0x0323u }, { 0x0041u, 0x0309u }, { 0x0061u, 0x0309u }, 
    { 0x00C2u, 0x0301u }, { 0x00E2u, 0x0301u }, { 0x00C2u, 0x0300u }, { 0x00E2u, 0x0300u }, 
    { 0x00C2u, 0x0309u }, { 0x00E2u, 0x0309u }, { 0x00C2u, 0x0303u }, { 0x00E2u, 0x0303u }, 
    { 0x1EA0u, 0x0302u }, { 0x1EA1u, 0x0302u }, { 0x0102u, 0x0301u }, { 0x0103u, 0x0301u }, 
    { 0x0102u, 0x0300u }, { 0x0103u, 0x0300u }, { 0x0102u, 0x0309u }, { 0x0103u, 0x0309u }, 
    { 0x0102u, 0x0303u }, { 0x0103u, 0x0303u }, { 0x1EA0u, 0x0306u }, { 0x1EA1u, 0x0306u }, 
    { 0x0045u, 0x0323u }, { 0x0065u, 0x0323u }, { 0x0045u, 0x0309u }, { 0x0065u, 0x0309u }, 
    { 0x0045u, 0x0303u }, { 0x0065u, 0x0303u }, { 0x00CAu, 0x0301u }, { 0x00EAu, 0x0301u }, 
    { 0x00CAu, 0x0300u }, { 0x00EAu, 0x0300u }, { 0x00CAu, 0x0309u }, { 0x00EAu, 0x0309u }, 
    { 0x00CAu, 0x0303u }, { 0x00EAu, 0x0303u }, { 0x1EB8u, 0x0302u }, { 0x1EB9u, 0x0302u }, 
    { 0x0049u, 0x0309u }, { 0x0069u, 0x0309u }, { 0x0049u, 0x0323u }, { 0x0069u, 0x0323u }, 
    { 0x004Fu, 0x0323u }, { 0x006Fu, 0x0323u }, { 0x004Fu, 0x0309u }, { 0x006Fu, 0x0309u }, 
    { 0x00D4u, 0x0301u }, { 0x00F4u, 0x0301u }, { 0x00D4u, 0x0300u }, { 0x00F4u, 0x0300u }, 
    { 0x00D4u, 0x0309u }, { 0x00F4u, 0x0309u }, { 0x00D4u, 0x0303u }, { 0x00F4u, 0x0303u }, 
    { 0x1ECCu, 0x0302u }, { 0x1ECDu, 0x0302u }, { 0x01A0u, 0x0301u }, { 0x01A1u, 0x0301u }, 
    { 0x01A0u, 0x0300u }, { 0x01A1u, 0x0300u }, { 0x01A0u, 0x0309u }, { 0x01A1u, 0x0309u }, 
    { 0x01A0u, 0x0303u }, { 0x01A1u, 0x0303u }, { 0x01A0u, 0x0323u }, { 0x01A1u, 0x0323u }, 
    { 0x0055u, 0x0323u }, { 0x0075u, 0x0323u }, { 0x0055u, 0x0309u }, { 0x0075u, 0x0309u }, 
    { 0x01AFu, 0x0301u }, { 0x01B0u, 0x0301u }, { 0x01AFu, 0x0300u }, { 0x01B0u, 0x0300u }, 
    { 0x01AFu, 0x0309u }, { 0x01B0u, 0x0309u }, { 0x01AFu, 0x0303u }, { 0x01B0u, 0x0303u }, 
    { 0x01AFu, 0x0323u }, { 0x01B0u, 0x0323u }, { 0x0059u, 0x0300u }, { 0x0079u, 0x0300u }, 
    { 0x0059u, 0x0323u }, { 0x0079u, 0x0323u }, { 0x0059u, 0x0309u }, { 0x0079u, 0x0309u }, 
    { 0x0059u, 0x0303u }, { 0x0079u, 0x0303u }, { 0x1EFAu, 0x0000u }, { 0x1EFBu, 0x0000u }, 
    { 0x1EFCu, 0x0000u }, { 0x1EFDu, 0x0000u }, { 0x1EFEu, 0x0000u }, { 0x1EFFu, 0x0000u }
  };

}; // namespace Babylon

dload(Babylon::Latin_Extended_Additional1E00);
