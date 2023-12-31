/*$Id: 2100-214F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:45 +0200.
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

  class Letterlike_Symbols2100 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Letterlike_Symbols2100() {
      m_first_letter = 0x2100;
      m_last_letter  = 0x214F;
      // m_version="3.1" // Not yet supported!

    }


    ~Letterlike_Symbols2100() {
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
      return "Letterlike Symbols";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return uc;
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Letterlike_Symbols2100::m_lower[uc - m_first_letter];
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
      return Babylon::Gen_Cat(Letterlike_Symbols2100::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Letterlike_Symbols2100::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Letterlike_Symbols2100::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Letterlike_Symbols2100::m_decompStr[uc - m_first_letter][0];
      us[1] = Letterlike_Symbols2100::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0x2100:
        us.resize(3);
        us[2u] = 0x0063u;
        break;

      case 0x2101:
        us.resize(3);
        us[2u] = 0x0073u;
        break;

      case 0x2105:
        us.resize(3);
        us[2u] = 0x006Fu;
        break;

      case 0x2106:
        us.resize(3);
        us[2u] = 0x0075u;
        break;

      case 0x2121:
        us.resize(3);
        us[2u] = 0x004Cu;
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
      return Babylon::Line_Break(Letterlike_Symbols2100::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Letterlike_Symbols2100::m_ea[uc - m_first_letter]);
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
      return m_Other_Math.test(uc - m_first_letter);
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
    Letterlike_Symbols2100(const Letterlike_Symbols2100 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<80> m_is_defined;
    static const UCS4 m_lower[80];
    static const unsigned char _cat[80];
    static const unsigned char m_bidir[80];
    static const unsigned char _decomp[80];
    static const UCS2 m_decompStr[80][2];
    static const unsigned char m_lb[80];
    static const unsigned char m_ea[80];
    static const std::bitset<80> m_Other_Math;

  }; // class Letterlike_Symbols2100

    const std::bitset<80> Letterlike_Symbols2100::m_is_defined(std::string("00000000000000000000011111111111111111111111111111111111111111111111111111111111"));

  const UCS4 Letterlike_Symbols2100::m_lower[] = {
    0x2100, 0x2101, 0x2102, 0x2103, 0x2104, 0x2105, 0x2106, 0x2107, 
    0x2108, 0x2109, 0x210A, 0x210B, 0x210C, 0x210D, 0x210E, 0x210F, 
    0x2110, 0x2111, 0x2112, 0x2113, 0x2114, 0x2115, 0x2116, 0x2117, 
    0x2118, 0x2119, 0x211A, 0x211B, 0x211C, 0x211D, 0x211E, 0x211F, 
    0x2120, 0x2121, 0x2122, 0x2123, 0x2124, 0x2125, 0x03C9, 0x2127, 
    0x2128, 0x2129, 0x006B, 0x00E5, 0x212C, 0x212D, 0x212E, 0x212F, 
    0x2130, 0x2131, 0x2132, 0x2133, 0x2134, 0x2135, 0x2136, 0x2137, 
    0x2138, 0x2139, 0x213A, 0x213B, 0x213C, 0x213D, 0x213E, 0x213F, 
    0x2140, 0x2141, 0x2142, 0x2143, 0x2144, 0x2145, 0x2146, 0x2147, 
    0x2148, 0x2149, 0x214A, 0x214B, 0x214C, 0x214D, 0x214E, 0x214F
  };

  const unsigned char Letterlike_Symbols2100::_cat[] = {
    CAT_So, CAT_So, CAT_Lu, CAT_So, CAT_So, CAT_So, CAT_So, CAT_Lu, 
    CAT_So, CAT_So, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_So, CAT_Lu, CAT_So, CAT_So, 
    CAT_So, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_Lu, CAT_So, CAT_Lu, CAT_So, 
    CAT_Lu, CAT_So, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_So, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_So, CAT_Lu, CAT_Ll, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Ll, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So
  };

  const unsigned char Letterlike_Symbols2100::m_bidir[] = {
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ET, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON
  };

  const unsigned char Letterlike_Symbols2100::_decomp[] = {
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_FONT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, 
    DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_FONT, DECOMP_COMPAT, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_SUPER, DECOMP_COMPAT, DECOMP_SUPER, DECOMP_CANONICAL, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_FONT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_FONT, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_FONT, 
    DECOMP_FONT, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_FONT, DECOMP_FONT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_FONT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Letterlike_Symbols2100::m_decompStr[][2] = {
    { 0x0061u, 0x002Fu }, { 0x0061u, 0x002Fu }, { 0x0043u, 0x0000u }, { 0x00B0u, 0x0043u }, 
    { 0x2104u, 0x0000u }, { 0x0063u, 0x002Fu }, { 0x0063u, 0x002Fu }, { 0x0190u, 0x0000u }, 
    { 0x2108u, 0x0000u }, { 0x00B0u, 0x0046u }, { 0x0067u, 0x0000u }, { 0x0048u, 0x0000u }, 
    { 0x0048u, 0x0000u }, { 0x0048u, 0x0000u }, { 0x0068u, 0x0000u }, { 0x0127u, 0x0000u }, 
    { 0x0049u, 0x0000u }, { 0x0049u, 0x0000u }, { 0x004Cu, 0x0000u }, { 0x006Cu, 0x0000u }, 
    { 0x2114u, 0x0000u }, { 0x004Eu, 0x0000u }, { 0x004Eu, 0x006Fu }, { 0x2117u, 0x0000u }, 
    { 0x2118u, 0x0000u }, { 0x0050u, 0x0000u }, { 0x0051u, 0x0000u }, { 0x0052u, 0x0000u }, 
    { 0x0052u, 0x0000u }, { 0x0052u, 0x0000u }, { 0x211Eu, 0x0000u }, { 0x211Fu, 0x0000u }, 
    { 0x0053u, 0x004Du }, { 0x0054u, 0x0045u }, { 0x0054u, 0x004Du }, { 0x2123u, 0x0000u }, 
    { 0x005Au, 0x0000u }, { 0x2125u, 0x0000u }, { 0x03A9u, 0x0000u }, { 0x2127u, 0x0000u }, 
    { 0x005Au, 0x0000u }, { 0x2129u, 0x0000u }, { 0x004Bu, 0x0000u }, { 0x00C5u, 0x0000u }, 
    { 0x0042u, 0x0000u }, { 0x0043u, 0x0000u }, { 0x212Eu, 0x0000u }, { 0x0065u, 0x0000u }, 
    { 0x0045u, 0x0000u }, { 0x0046u, 0x0000u }, { 0x2132u, 0x0000u }, { 0x004Du, 0x0000u }, 
    { 0x006Fu, 0x0000u }, { 0x05D0u, 0x0000u }, { 0x05D1u, 0x0000u }, { 0x05D2u, 0x0000u }, 
    { 0x05D3u, 0x0000u }, { 0x0069u, 0x0000u }, { 0x213Au, 0x0000u }, { 0x213Bu, 0x0000u }, 
    { 0x213Cu, 0x0000u }, { 0x213Du, 0x0000u }, { 0x213Eu, 0x0000u }, { 0x213Fu, 0x0000u }, 
    { 0x2140u, 0x0000u }, { 0x2141u, 0x0000u }, { 0x2142u, 0x0000u }, { 0x2143u, 0x0000u }, 
    { 0x2144u, 0x0000u }, { 0x2145u, 0x0000u }, { 0x2146u, 0x0000u }, { 0x2147u, 0x0000u }, 
    { 0x2148u, 0x0000u }, { 0x2149u, 0x0000u }, { 0x214Au, 0x0000u }, { 0x214Bu, 0x0000u }, 
    { 0x214Cu, 0x0000u }, { 0x214Du, 0x0000u }, { 0x214Eu, 0x0000u }, { 0x214Fu, 0x0000u }
  };

  const unsigned char Letterlike_Symbols2100::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_PO, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_PO, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_PR, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_PO, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Letterlike_Symbols2100::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

    const std::bitset<80> Letterlike_Symbols2100::m_Other_Math(std::string("00000000000000000000000111111011101100000000000000111110000011111111110000000000"));

}; // namespace Babylon

dload(Babylon::Letterlike_Symbols2100);
