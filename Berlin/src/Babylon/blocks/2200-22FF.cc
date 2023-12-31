/*$Id: 2200-22FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:51 +0200.
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

  class Mathematical_Operators2200 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Mathematical_Operators2200() {
      m_first_letter = 0x2200;
      m_last_letter  = 0x22FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00002203, 0x00000338)] = 0x2204;
      m_composeMap[make_pair(0x00002208, 0x00000338)] = 0x2209;
      m_composeMap[make_pair(0x0000220B, 0x00000338)] = 0x220C;
      m_composeMap[make_pair(0x00002223, 0x00000338)] = 0x2224;
      m_composeMap[make_pair(0x00002225, 0x00000338)] = 0x2226;
      m_composeMap[make_pair(0x0000223C, 0x00000338)] = 0x2241;
      m_composeMap[make_pair(0x00002243, 0x00000338)] = 0x2244;
      m_composeMap[make_pair(0x00002245, 0x00000338)] = 0x2247;
      m_composeMap[make_pair(0x00002248, 0x00000338)] = 0x2249;
      m_composeMap[make_pair(0x0000224D, 0x00000338)] = 0x226D;
      m_composeMap[make_pair(0x00002261, 0x00000338)] = 0x2262;
      m_composeMap[make_pair(0x00002264, 0x00000338)] = 0x2270;
      m_composeMap[make_pair(0x00002265, 0x00000338)] = 0x2271;
      m_composeMap[make_pair(0x00002272, 0x00000338)] = 0x2274;
      m_composeMap[make_pair(0x00002273, 0x00000338)] = 0x2275;
      m_composeMap[make_pair(0x00002276, 0x00000338)] = 0x2278;
      m_composeMap[make_pair(0x00002277, 0x00000338)] = 0x2279;
      m_composeMap[make_pair(0x0000227A, 0x00000338)] = 0x2280;
      m_composeMap[make_pair(0x0000227B, 0x00000338)] = 0x2281;
      m_composeMap[make_pair(0x0000227C, 0x00000338)] = 0x22E0;
      m_composeMap[make_pair(0x0000227D, 0x00000338)] = 0x22E1;
      m_composeMap[make_pair(0x00002282, 0x00000338)] = 0x2284;
      m_composeMap[make_pair(0x00002283, 0x00000338)] = 0x2285;
      m_composeMap[make_pair(0x00002286, 0x00000338)] = 0x2288;
      m_composeMap[make_pair(0x00002287, 0x00000338)] = 0x2289;
      m_composeMap[make_pair(0x00002291, 0x00000338)] = 0x22E2;
      m_composeMap[make_pair(0x00002292, 0x00000338)] = 0x22E3;
      m_composeMap[make_pair(0x000022A2, 0x00000338)] = 0x22AC;
      m_composeMap[make_pair(0x000022A8, 0x00000338)] = 0x22AD;
      m_composeMap[make_pair(0x000022A9, 0x00000338)] = 0x22AE;
      m_composeMap[make_pair(0x000022AB, 0x00000338)] = 0x22AF;
      m_composeMap[make_pair(0x000022B2, 0x00000338)] = 0x22EA;
      m_composeMap[make_pair(0x000022B3, 0x00000338)] = 0x22EB;
      m_composeMap[make_pair(0x000022B4, 0x00000338)] = 0x22EC;
      m_composeMap[make_pair(0x000022B5, 0x00000338)] = 0x22ED;

    }


    ~Mathematical_Operators2200() {
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
      return "Mathematical Operators";
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
      return Babylon::Gen_Cat(CAT_Sm);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Mathematical_Operators2200::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Mathematical_Operators2200::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Mathematical_Operators2200::m_decompStr[uc - m_first_letter][0];
      us[1] = Mathematical_Operators2200::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0x222D:
        us.resize(3);
        us[2u] = 0x222Bu;
        break;

      case 0x2230:
        us.resize(3);
        us[2u] = 0x222Eu;
        break;
      }
      if (us[1] == 0x0000u) {
        us.resize(1);
      }

      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return m_mirror.test(uc - m_first_letter);
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(Mathematical_Operators2200::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Mathematical_Operators2200::m_ea[uc - m_first_letter]);
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
    Mathematical_Operators2200(const Mathematical_Operators2200 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const unsigned char m_bidir[256];
    static const unsigned char _decomp[256];
    static const UCS2 m_decompStr[256][2];
    static const std::bitset<256> m_mirror;
    static const unsigned char m_lb[256];
    static const unsigned char m_ea[256];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;

  }; // class Mathematical_Operators2200

    const std::bitset<256> Mathematical_Operators2200::m_is_defined(std::string("0000000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char Mathematical_Operators2200::m_bidir[] = {
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON
  };

  const unsigned char Mathematical_Operators2200::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
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
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 Mathematical_Operators2200::m_decompStr[][2] = {
    { 0x2200u, 0x0000u }, { 0x2201u, 0x0000u }, { 0x2202u, 0x0000u }, { 0x2203u, 0x0000u }, 
    { 0x2203u, 0x0338u }, { 0x2205u, 0x0000u }, { 0x2206u, 0x0000u }, { 0x2207u, 0x0000u }, 
    { 0x2208u, 0x0000u }, { 0x2208u, 0x0338u }, { 0x220Au, 0x0000u }, { 0x220Bu, 0x0000u }, 
    { 0x220Bu, 0x0338u }, { 0x220Du, 0x0000u }, { 0x220Eu, 0x0000u }, { 0x220Fu, 0x0000u }, 
    { 0x2210u, 0x0000u }, { 0x2211u, 0x0000u }, { 0x2212u, 0x0000u }, { 0x2213u, 0x0000u }, 
    { 0x2214u, 0x0000u }, { 0x2215u, 0x0000u }, { 0x2216u, 0x0000u }, { 0x2217u, 0x0000u }, 
    { 0x2218u, 0x0000u }, { 0x2219u, 0x0000u }, { 0x221Au, 0x0000u }, { 0x221Bu, 0x0000u }, 
    { 0x221Cu, 0x0000u }, { 0x221Du, 0x0000u }, { 0x221Eu, 0x0000u }, { 0x221Fu, 0x0000u }, 
    { 0x2220u, 0x0000u }, { 0x2221u, 0x0000u }, { 0x2222u, 0x0000u }, { 0x2223u, 0x0000u }, 
    { 0x2223u, 0x0338u }, { 0x2225u, 0x0000u }, { 0x2225u, 0x0338u }, { 0x2227u, 0x0000u }, 
    { 0x2228u, 0x0000u }, { 0x2229u, 0x0000u }, { 0x222Au, 0x0000u }, { 0x222Bu, 0x0000u }, 
    { 0x222Bu, 0x222Bu }, { 0x222Bu, 0x222Bu }, { 0x222Eu, 0x0000u }, { 0x222Eu, 0x222Eu }, 
    { 0x222Eu, 0x222Eu }, { 0x2231u, 0x0000u }, { 0x2232u, 0x0000u }, { 0x2233u, 0x0000u }, 
    { 0x2234u, 0x0000u }, { 0x2235u, 0x0000u }, { 0x2236u, 0x0000u }, { 0x2237u, 0x0000u }, 
    { 0x2238u, 0x0000u }, { 0x2239u, 0x0000u }, { 0x223Au, 0x0000u }, { 0x223Bu, 0x0000u }, 
    { 0x223Cu, 0x0000u }, { 0x223Du, 0x0000u }, { 0x223Eu, 0x0000u }, { 0x223Fu, 0x0000u }, 
    { 0x2240u, 0x0000u }, { 0x223Cu, 0x0338u }, { 0x2242u, 0x0000u }, { 0x2243u, 0x0000u }, 
    { 0x2243u, 0x0338u }, { 0x2245u, 0x0000u }, { 0x2246u, 0x0000u }, { 0x2245u, 0x0338u }, 
    { 0x2248u, 0x0000u }, { 0x2248u, 0x0338u }, { 0x224Au, 0x0000u }, { 0x224Bu, 0x0000u }, 
    { 0x224Cu, 0x0000u }, { 0x224Du, 0x0000u }, { 0x224Eu, 0x0000u }, { 0x224Fu, 0x0000u }, 
    { 0x2250u, 0x0000u }, { 0x2251u, 0x0000u }, { 0x2252u, 0x0000u }, { 0x2253u, 0x0000u }, 
    { 0x2254u, 0x0000u }, { 0x2255u, 0x0000u }, { 0x2256u, 0x0000u }, { 0x2257u, 0x0000u }, 
    { 0x2258u, 0x0000u }, { 0x2259u, 0x0000u }, { 0x225Au, 0x0000u }, { 0x225Bu, 0x0000u }, 
    { 0x225Cu, 0x0000u }, { 0x225Du, 0x0000u }, { 0x225Eu, 0x0000u }, { 0x225Fu, 0x0000u }, 
    { 0x003Du, 0x0338u }, { 0x2261u, 0x0000u }, { 0x2261u, 0x0338u }, { 0x2263u, 0x0000u }, 
    { 0x2264u, 0x0000u }, { 0x2265u, 0x0000u }, { 0x2266u, 0x0000u }, { 0x2267u, 0x0000u }, 
    { 0x2268u, 0x0000u }, { 0x2269u, 0x0000u }, { 0x226Au, 0x0000u }, { 0x226Bu, 0x0000u }, 
    { 0x226Cu, 0x0000u }, { 0x224Du, 0x0338u }, { 0x003Cu, 0x0338u }, { 0x003Eu, 0x0338u }, 
    { 0x2264u, 0x0338u }, { 0x2265u, 0x0338u }, { 0x2272u, 0x0000u }, { 0x2273u, 0x0000u }, 
    { 0x2272u, 0x0338u }, { 0x2273u, 0x0338u }, { 0x2276u, 0x0000u }, { 0x2277u, 0x0000u }, 
    { 0x2276u, 0x0338u }, { 0x2277u, 0x0338u }, { 0x227Au, 0x0000u }, { 0x227Bu, 0x0000u }, 
    { 0x227Cu, 0x0000u }, { 0x227Du, 0x0000u }, { 0x227Eu, 0x0000u }, { 0x227Fu, 0x0000u }, 
    { 0x227Au, 0x0338u }, { 0x227Bu, 0x0338u }, { 0x2282u, 0x0000u }, { 0x2283u, 0x0000u }, 
    { 0x2282u, 0x0338u }, { 0x2283u, 0x0338u }, { 0x2286u, 0x0000u }, { 0x2287u, 0x0000u }, 
    { 0x2286u, 0x0338u }, { 0x2287u, 0x0338u }, { 0x228Au, 0x0000u }, { 0x228Bu, 0x0000u }, 
    { 0x228Cu, 0x0000u }, { 0x228Du, 0x0000u }, { 0x228Eu, 0x0000u }, { 0x228Fu, 0x0000u }, 
    { 0x2290u, 0x0000u }, { 0x2291u, 0x0000u }, { 0x2292u, 0x0000u }, { 0x2293u, 0x0000u }, 
    { 0x2294u, 0x0000u }, { 0x2295u, 0x0000u }, { 0x2296u, 0x0000u }, { 0x2297u, 0x0000u }, 
    { 0x2298u, 0x0000u }, { 0x2299u, 0x0000u }, { 0x229Au, 0x0000u }, { 0x229Bu, 0x0000u }, 
    { 0x229Cu, 0x0000u }, { 0x229Du, 0x0000u }, { 0x229Eu, 0x0000u }, { 0x229Fu, 0x0000u }, 
    { 0x22A0u, 0x0000u }, { 0x22A1u, 0x0000u }, { 0x22A2u, 0x0000u }, { 0x22A3u, 0x0000u }, 
    { 0x22A4u, 0x0000u }, { 0x22A5u, 0x0000u }, { 0x22A6u, 0x0000u }, { 0x22A7u, 0x0000u }, 
    { 0x22A8u, 0x0000u }, { 0x22A9u, 0x0000u }, { 0x22AAu, 0x0000u }, { 0x22ABu, 0x0000u }, 
    { 0x22A2u, 0x0338u }, { 0x22A8u, 0x0338u }, { 0x22A9u, 0x0338u }, { 0x22ABu, 0x0338u }, 
    { 0x22B0u, 0x0000u }, { 0x22B1u, 0x0000u }, { 0x22B2u, 0x0000u }, { 0x22B3u, 0x0000u }, 
    { 0x22B4u, 0x0000u }, { 0x22B5u, 0x0000u }, { 0x22B6u, 0x0000u }, { 0x22B7u, 0x0000u }, 
    { 0x22B8u, 0x0000u }, { 0x22B9u, 0x0000u }, { 0x22BAu, 0x0000u }, { 0x22BBu, 0x0000u }, 
    { 0x22BCu, 0x0000u }, { 0x22BDu, 0x0000u }, { 0x22BEu, 0x0000u }, { 0x22BFu, 0x0000u }, 
    { 0x22C0u, 0x0000u }, { 0x22C1u, 0x0000u }, { 0x22C2u, 0x0000u }, { 0x22C3u, 0x0000u }, 
    { 0x22C4u, 0x0000u }, { 0x22C5u, 0x0000u }, { 0x22C6u, 0x0000u }, { 0x22C7u, 0x0000u }, 
    { 0x22C8u, 0x0000u }, { 0x22C9u, 0x0000u }, { 0x22CAu, 0x0000u }, { 0x22CBu, 0x0000u }, 
    { 0x22CCu, 0x0000u }, { 0x22CDu, 0x0000u }, { 0x22CEu, 0x0000u }, { 0x22CFu, 0x0000u }, 
    { 0x22D0u, 0x0000u }, { 0x22D1u, 0x0000u }, { 0x22D2u, 0x0000u }, { 0x22D3u, 0x0000u }, 
    { 0x22D4u, 0x0000u }, { 0x22D5u, 0x0000u }, { 0x22D6u, 0x0000u }, { 0x22D7u, 0x0000u }, 
    { 0x22D8u, 0x0000u }, { 0x22D9u, 0x0000u }, { 0x22DAu, 0x0000u }, { 0x22DBu, 0x0000u }, 
    { 0x22DCu, 0x0000u }, { 0x22DDu, 0x0000u }, { 0x22DEu, 0x0000u }, { 0x22DFu, 0x0000u }, 
    { 0x227Cu, 0x0338u }, { 0x227Du, 0x0338u }, { 0x2291u, 0x0338u }, { 0x2292u, 0x0338u }, 
    { 0x22E4u, 0x0000u }, { 0x22E5u, 0x0000u }, { 0x22E6u, 0x0000u }, { 0x22E7u, 0x0000u }, 
    { 0x22E8u, 0x0000u }, { 0x22E9u, 0x0000u }, { 0x22B2u, 0x0338u }, { 0x22B3u, 0x0338u }, 
    { 0x22B4u, 0x0338u }, { 0x22B5u, 0x0338u }, { 0x22EEu, 0x0000u }, { 0x22EFu, 0x0000u }, 
    { 0x22F0u, 0x0000u }, { 0x22F1u, 0x0000u }, { 0x22F2u, 0x0000u }, { 0x22F3u, 0x0000u }, 
    { 0x22F4u, 0x0000u }, { 0x22F5u, 0x0000u }, { 0x22F6u, 0x0000u }, { 0x22F7u, 0x0000u }, 
    { 0x22F8u, 0x0000u }, { 0x22F9u, 0x0000u }, { 0x22FAu, 0x0000u }, { 0x22FBu, 0x0000u }, 
    { 0x22FCu, 0x0000u }, { 0x22FDu, 0x0000u }, { 0x22FEu, 0x0000u }, { 0x22FFu, 0x0000u }
  };

  const std::bitset<256> Mathematical_Operators2200::m_mirror(std::string("0000000000000011001111111111111111111111110000110011111000000000110000011111111111111111110011000000000100000111100111111111111111111111111111111100111111110101100000000011110000011111111111111111101000001111111110000101011110111100011000100011111100011110"));

  const unsigned char Mathematical_Operators2200::m_lb[] = {
    LB_AI, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AL, LB_AI, LB_PR, LB_PR, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AL, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI
  };

  const unsigned char Mathematical_Operators2200::m_ea[] = {
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A
  };

}; // namespace Babylon

dload(Babylon::Mathematical_Operators2200);
