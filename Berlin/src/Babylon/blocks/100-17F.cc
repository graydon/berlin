/*$Id: 100-17F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:01 +0200.
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

  class Latin_ExtendedA100 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Latin_ExtendedA100() {
      m_first_letter = 0x100;
      m_last_letter  = 0x17F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000102, 0x00000300)] = 0x1EB0;
      m_composeMap[make_pair(0x00000102, 0x00000301)] = 0x1EAE;
      m_composeMap[make_pair(0x00000102, 0x00000303)] = 0x1EB4;
      m_composeMap[make_pair(0x00000102, 0x00000309)] = 0x1EB2;
      m_composeMap[make_pair(0x00000103, 0x00000300)] = 0x1EB1;
      m_composeMap[make_pair(0x00000103, 0x00000301)] = 0x1EAF;
      m_composeMap[make_pair(0x00000103, 0x00000303)] = 0x1EB5;
      m_composeMap[make_pair(0x00000103, 0x00000309)] = 0x1EB3;
      m_composeMap[make_pair(0x00000112, 0x00000300)] = 0x1E14;
      m_composeMap[make_pair(0x00000112, 0x00000301)] = 0x1E16;
      m_composeMap[make_pair(0x00000113, 0x00000300)] = 0x1E15;
      m_composeMap[make_pair(0x00000113, 0x00000301)] = 0x1E17;
      m_composeMap[make_pair(0x0000014C, 0x00000300)] = 0x1E50;
      m_composeMap[make_pair(0x0000014C, 0x00000301)] = 0x1E52;
      m_composeMap[make_pair(0x0000014D, 0x00000300)] = 0x1E51;
      m_composeMap[make_pair(0x0000014D, 0x00000301)] = 0x1E53;
      m_composeMap[make_pair(0x0000015A, 0x00000307)] = 0x1E64;
      m_composeMap[make_pair(0x0000015B, 0x00000307)] = 0x1E65;
      m_composeMap[make_pair(0x00000160, 0x00000307)] = 0x1E66;
      m_composeMap[make_pair(0x00000161, 0x00000307)] = 0x1E67;
      m_composeMap[make_pair(0x00000168, 0x00000301)] = 0x1E78;
      m_composeMap[make_pair(0x00000169, 0x00000301)] = 0x1E79;
      m_composeMap[make_pair(0x0000016A, 0x00000308)] = 0x1E7A;
      m_composeMap[make_pair(0x0000016B, 0x00000308)] = 0x1E7B;
      m_composeMap[make_pair(0x0000017F, 0x00000307)] = 0x1E9B;

    }


    ~Latin_ExtendedA100() {
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
      return "Latin Extended-A";
    }

    bool is_defined(const UCS4 uc) const {
      return 1;
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Latin_ExtendedA100::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Latin_ExtendedA100::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Latin_ExtendedA100::m_title[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Latin_ExtendedA100::_cat[uc - m_first_letter]);
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
      return Babylon::Char_Decomp(Latin_ExtendedA100::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Latin_ExtendedA100::m_decompStr[uc - m_first_letter][0];
      us[1] = Latin_ExtendedA100::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Latin_ExtendedA100::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Latin_ExtendedA100::m_ea[uc - m_first_letter]);
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
    Latin_ExtendedA100(const Latin_ExtendedA100 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const UCS4 m_upper[128];
    static const UCS4 m_lower[128];
    static const UCS4 m_title[128];
    static const unsigned char _cat[128];
    static const unsigned char _decomp[128];
    static const UCS2 m_decompStr[128][2];
    static const unsigned char m_lb[128];
    static const unsigned char m_ea[128];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Latin_ExtendedA100

  const UCS4 Latin_ExtendedA100::m_upper[] = {
    0x0100, 0x0100, 0x0102, 0x0102, 0x0104, 0x0104, 0x0106, 0x0106, 
    0x0108, 0x0108, 0x010A, 0x010A, 0x010C, 0x010C, 0x010E, 0x010E, 
    0x0110, 0x0110, 0x0112, 0x0112, 0x0114, 0x0114, 0x0116, 0x0116, 
    0x0118, 0x0118, 0x011A, 0x011A, 0x011C, 0x011C, 0x011E, 0x011E, 
    0x0120, 0x0120, 0x0122, 0x0122, 0x0124, 0x0124, 0x0126, 0x0126, 
    0x0128, 0x0128, 0x012A, 0x012A, 0x012C, 0x012C, 0x012E, 0x012E, 
    0x0130, 0x0049, 0x0132, 0x0132, 0x0134, 0x0134, 0x0136, 0x0136, 
    0x0138, 0x0139, 0x0139, 0x013B, 0x013B, 0x013D, 0x013D, 0x013F, 
    0x013F, 0x0141, 0x0141, 0x0143, 0x0143, 0x0145, 0x0145, 0x0147, 
    0x0147, 0x0149, 0x014A, 0x014A, 0x014C, 0x014C, 0x014E, 0x014E, 
    0x0150, 0x0150, 0x0152, 0x0152, 0x0154, 0x0154, 0x0156, 0x0156, 
    0x0158, 0x0158, 0x015A, 0x015A, 0x015C, 0x015C, 0x015E, 0x015E, 
    0x0160, 0x0160, 0x0162, 0x0162, 0x0164, 0x0164, 0x0166, 0x0166, 
    0x0168, 0x0168, 0x016A, 0x016A, 0x016C, 0x016C, 0x016E, 0x016E, 
    0x0170, 0x0170, 0x0172, 0x0172, 0x0174, 0x0174, 0x0176, 0x0176, 
    0x0178, 0x0179, 0x0179, 0x017B, 0x017B, 0x017D, 0x017D, 0x0053
  };

  const UCS4 Latin_ExtendedA100::m_lower[] = {
    0x0101, 0x0101, 0x0103, 0x0103, 0x0105, 0x0105, 0x0107, 0x0107, 
    0x0109, 0x0109, 0x010B, 0x010B, 0x010D, 0x010D, 0x010F, 0x010F, 
    0x0111, 0x0111, 0x0113, 0x0113, 0x0115, 0x0115, 0x0117, 0x0117, 
    0x0119, 0x0119, 0x011B, 0x011B, 0x011D, 0x011D, 0x011F, 0x011F, 
    0x0121, 0x0121, 0x0123, 0x0123, 0x0125, 0x0125, 0x0127, 0x0127, 
    0x0129, 0x0129, 0x012B, 0x012B, 0x012D, 0x012D, 0x012F, 0x012F, 
    0x0069, 0x0131, 0x0133, 0x0133, 0x0135, 0x0135, 0x0137, 0x0137, 
    0x0138, 0x013A, 0x013A, 0x013C, 0x013C, 0x013E, 0x013E, 0x0140, 
    0x0140, 0x0142, 0x0142, 0x0144, 0x0144, 0x0146, 0x0146, 0x0148, 
    0x0148, 0x0149, 0x014B, 0x014B, 0x014D, 0x014D, 0x014F, 0x014F, 
    0x0151, 0x0151, 0x0153, 0x0153, 0x0155, 0x0155, 0x0157, 0x0157, 
    0x0159, 0x0159, 0x015B, 0x015B, 0x015D, 0x015D, 0x015F, 0x015F, 
    0x0161, 0x0161, 0x0163, 0x0163, 0x0165, 0x0165, 0x0167, 0x0167, 
    0x0169, 0x0169, 0x016B, 0x016B, 0x016D, 0x016D, 0x016F, 0x016F, 
    0x0171, 0x0171, 0x0173, 0x0173, 0x0175, 0x0175, 0x0177, 0x0177, 
    0x00FF, 0x017A, 0x017A, 0x017C, 0x017C, 0x017E, 0x017E, 0x017F
  };

  const UCS4 Latin_ExtendedA100::m_title[] = {
    0x0100, 0x0100, 0x0102, 0x0102, 0x0104, 0x0104, 0x0106, 0x0106, 
    0x0108, 0x0108, 0x010A, 0x010A, 0x010C, 0x010C, 0x010E, 0x010E, 
    0x0110, 0x0110, 0x0112, 0x0112, 0x0114, 0x0114, 0x0116, 0x0116, 
    0x0118, 0x0118, 0x011A, 0x011A, 0x011C, 0x011C, 0x011E, 0x011E, 
    0x0120, 0x0120, 0x0122, 0x0122, 0x0124, 0x0124, 0x0126, 0x0126, 
    0x0128, 0x0128, 0x012A, 0x012A, 0x012C, 0x012C, 0x012E, 0x012E, 
    0x0130, 0x0049, 0x0132, 0x0132, 0x0134, 0x0134, 0x0136, 0x0136, 
    0x0138, 0x0139, 0x0139, 0x013B, 0x013B, 0x013D, 0x013D, 0x013F, 
    0x013F, 0x0141, 0x0141, 0x0143, 0x0143, 0x0145, 0x0145, 0x0147, 
    0x0147, 0x0149, 0x014A, 0x014A, 0x014C, 0x014C, 0x014E, 0x014E, 
    0x0150, 0x0150, 0x0152, 0x0152, 0x0154, 0x0154, 0x0156, 0x0156, 
    0x0158, 0x0158, 0x015A, 0x015A, 0x015C, 0x015C, 0x015E, 0x015E, 
    0x0160, 0x0160, 0x0162, 0x0162, 0x0164, 0x0164, 0x0166, 0x0166, 
    0x0168, 0x0168, 0x016A, 0x016A, 0x016C, 0x016C, 0x016E, 0x016E, 
    0x0170, 0x0170, 0x0172, 0x0172, 0x0174, 0x0174, 0x0176, 0x0176, 
    0x0178, 0x0179, 0x0179, 0x017B, 0x017B, 0x017D, 0x017D, 0x0053
  };

  const unsigned char Latin_ExtendedA100::_cat[] = {
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll
  };

  const unsigned char Latin_ExtendedA100::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT
  };

  const UCS2 Latin_ExtendedA100::m_decompStr[][2] = {
    { 0x0041u, 0x0304u }, { 0x0061u, 0x0304u }, { 0x0041u, 0x0306u }, { 0x0061u, 0x0306u }, 
    { 0x0041u, 0x0328u }, { 0x0061u, 0x0328u }, { 0x0043u, 0x0301u }, { 0x0063u, 0x0301u }, 
    { 0x0043u, 0x0302u }, { 0x0063u, 0x0302u }, { 0x0043u, 0x0307u }, { 0x0063u, 0x0307u }, 
    { 0x0043u, 0x030Cu }, { 0x0063u, 0x030Cu }, { 0x0044u, 0x030Cu }, { 0x0064u, 0x030Cu }, 
    { 0x0110u, 0x0000u }, { 0x0111u, 0x0000u }, { 0x0045u, 0x0304u }, { 0x0065u, 0x0304u }, 
    { 0x0045u, 0x0306u }, { 0x0065u, 0x0306u }, { 0x0045u, 0x0307u }, { 0x0065u, 0x0307u }, 
    { 0x0045u, 0x0328u }, { 0x0065u, 0x0328u }, { 0x0045u, 0x030Cu }, { 0x0065u, 0x030Cu }, 
    { 0x0047u, 0x0302u }, { 0x0067u, 0x0302u }, { 0x0047u, 0x0306u }, { 0x0067u, 0x0306u }, 
    { 0x0047u, 0x0307u }, { 0x0067u, 0x0307u }, { 0x0047u, 0x0327u }, { 0x0067u, 0x0327u }, 
    { 0x0048u, 0x0302u }, { 0x0068u, 0x0302u }, { 0x0126u, 0x0000u }, { 0x0127u, 0x0000u }, 
    { 0x0049u, 0x0303u }, { 0x0069u, 0x0303u }, { 0x0049u, 0x0304u }, { 0x0069u, 0x0304u }, 
    { 0x0049u, 0x0306u }, { 0x0069u, 0x0306u }, { 0x0049u, 0x0328u }, { 0x0069u, 0x0328u }, 
    { 0x0049u, 0x0307u }, { 0x0131u, 0x0000u }, { 0x0049u, 0x004Au }, { 0x0069u, 0x006Au }, 
    { 0x004Au, 0x0302u }, { 0x006Au, 0x0302u }, { 0x004Bu, 0x0327u }, { 0x006Bu, 0x0327u }, 
    { 0x0138u, 0x0000u }, { 0x004Cu, 0x0301u }, { 0x006Cu, 0x0301u }, { 0x004Cu, 0x0327u }, 
    { 0x006Cu, 0x0327u }, { 0x004Cu, 0x030Cu }, { 0x006Cu, 0x030Cu }, { 0x004Cu, 0x00B7u }, 
    { 0x006Cu, 0x00B7u }, { 0x0141u, 0x0000u }, { 0x0142u, 0x0000u }, { 0x004Eu, 0x0301u }, 
    { 0x006Eu, 0x0301u }, { 0x004Eu, 0x0327u }, { 0x006Eu, 0x0327u }, { 0x004Eu, 0x030Cu }, 
    { 0x006Eu, 0x030Cu }, { 0x02BCu, 0x006Eu }, { 0x014Au, 0x0000u }, { 0x014Bu, 0x0000u }, 
    { 0x004Fu, 0x0304u }, { 0x006Fu, 0x0304u }, { 0x004Fu, 0x0306u }, { 0x006Fu, 0x0306u }, 
    { 0x004Fu, 0x030Bu }, { 0x006Fu, 0x030Bu }, { 0x0152u, 0x0000u }, { 0x0153u, 0x0000u }, 
    { 0x0052u, 0x0301u }, { 0x0072u, 0x0301u }, { 0x0052u, 0x0327u }, { 0x0072u, 0x0327u }, 
    { 0x0052u, 0x030Cu }, { 0x0072u, 0x030Cu }, { 0x0053u, 0x0301u }, { 0x0073u, 0x0301u }, 
    { 0x0053u, 0x0302u }, { 0x0073u, 0x0302u }, { 0x0053u, 0x0327u }, { 0x0073u, 0x0327u }, 
    { 0x0053u, 0x030Cu }, { 0x0073u, 0x030Cu }, { 0x0054u, 0x0327u }, { 0x0074u, 0x0327u }, 
    { 0x0054u, 0x030Cu }, { 0x0074u, 0x030Cu }, { 0x0166u, 0x0000u }, { 0x0167u, 0x0000u }, 
    { 0x0055u, 0x0303u }, { 0x0075u, 0x0303u }, { 0x0055u, 0x0304u }, { 0x0075u, 0x0304u }, 
    { 0x0055u, 0x0306u }, { 0x0075u, 0x0306u }, { 0x0055u, 0x030Au }, { 0x0075u, 0x030Au }, 
    { 0x0055u, 0x030Bu }, { 0x0075u, 0x030Bu }, { 0x0055u, 0x0328u }, { 0x0075u, 0x0328u }, 
    { 0x0057u, 0x0302u }, { 0x0077u, 0x0302u }, { 0x0059u, 0x0302u }, { 0x0079u, 0x0302u }, 
    { 0x0059u, 0x0308u }, { 0x005Au, 0x0301u }, { 0x007Au, 0x0301u }, { 0x005Au, 0x0307u }, 
    { 0x007Au, 0x0307u }, { 0x005Au, 0x030Cu }, { 0x007Au, 0x030Cu }, { 0x0073u, 0x0000u }
  };

  const unsigned char Latin_ExtendedA100::m_lb[] = {
    LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Latin_ExtendedA100::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

}; // namespace Babylon

dload(Babylon::Latin_ExtendedA100);
