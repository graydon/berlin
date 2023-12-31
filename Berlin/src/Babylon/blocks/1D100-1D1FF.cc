/*$Id: 1D100-1D1FF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:05:33 +0200.
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

  class Musical_Symbols1D100 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Musical_Symbols1D100() {
      m_first_letter = 0x1D100;
      m_last_letter  = 0x1D1FF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x0001D157, 0x0001D165)] = 0x1D15E;
      m_composeMap[make_pair(0x0001D158, 0x0001D165)] = 0x1D15F;
      m_composeMap[make_pair(0x0001D15F, 0x0001D16E)] = 0x1D160;
      m_composeMap[make_pair(0x0001D15F, 0x0001D16F)] = 0x1D161;
      m_composeMap[make_pair(0x0001D15F, 0x0001D170)] = 0x1D162;
      m_composeMap[make_pair(0x0001D15F, 0x0001D171)] = 0x1D163;
      m_composeMap[make_pair(0x0001D15F, 0x0001D172)] = 0x1D164;
      m_composeMap[make_pair(0x0001D1B9, 0x0001D165)] = 0x1D1BB;
      m_composeMap[make_pair(0x0001D1BA, 0x0001D165)] = 0x1D1BC;
      m_composeMap[make_pair(0x0001D1BB, 0x0001D16E)] = 0x1D1BD;
      m_composeMap[make_pair(0x0001D1BB, 0x0001D16F)] = 0x1D1BF;
      m_composeMap[make_pair(0x0001D1BC, 0x0001D16E)] = 0x1D1BE;
      m_composeMap[make_pair(0x0001D1BC, 0x0001D16F)] = 0x1D1C0;

    }


    ~Musical_Symbols1D100() {
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
      return "Musical Symbols";
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
      return Babylon::Gen_Cat(Musical_Symbols1D100::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Musical_Symbols1D100::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Musical_Symbols1D100::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Musical_Symbols1D100::m_decompStr[uc - m_first_letter][0];
      us[1] = Musical_Symbols1D100::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Musical_Symbols1D100::m_lb[uc - m_first_letter]);
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
    Musical_Symbols1D100(const Musical_Symbols1D100 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<256> m_is_defined;
    static const unsigned char _cat[256];
    static const unsigned char _comb_cl[256];
    static const unsigned char m_bidir[256];
    static const UCS4 m_decompStr[256][2];
    static const unsigned char m_lb[256];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<256> m_Diacritic;

  }; // class Musical_Symbols1D100

    const std::bitset<256> Musical_Symbols1D100::m_is_defined(std::string("0000000000000000000000000000000000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000111111111111111111111111111111111111111"));

  const unsigned char Musical_Symbols1D100::_cat[] = {
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
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_Mc, CAT_Mc, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_So, CAT_So, CAT_So, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, 
    CAT_Cf, CAT_Cf, CAT_Cf, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_So, CAT_So, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, CAT_So, 
    CAT_So, CAT_So, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_So, CAT_So, 
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

  const unsigned char Musical_Symbols1D100::_comb_cl[] = {
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
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 216, 216, 1, 
    1, 1, 0, 0, 0, 226, 216, 216, 
    216, 216, 216, 0, 0, 0, 0, 0, 
    0, 0, 0, 220, 220, 220, 220, 220, 
    220, 220, 220, 0, 0, 230, 230, 230, 
    230, 230, 220, 220, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 230, 230, 230, 230, 0, 0, 
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

  const unsigned char Musical_Symbols1D100::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS4 Musical_Symbols1D100::m_decompStr[][2] = {
    { 0x1D100u, 0x0000u }, { 0x1D101u, 0x0000u }, { 0x1D102u, 0x0000u }, { 0x1D103u, 0x0000u }, 
    { 0x1D104u, 0x0000u }, { 0x1D105u, 0x0000u }, { 0x1D106u, 0x0000u }, { 0x1D107u, 0x0000u }, 
    { 0x1D108u, 0x0000u }, { 0x1D109u, 0x0000u }, { 0x1D10Au, 0x0000u }, { 0x1D10Bu, 0x0000u }, 
    { 0x1D10Cu, 0x0000u }, { 0x1D10Du, 0x0000u }, { 0x1D10Eu, 0x0000u }, { 0x1D10Fu, 0x0000u }, 
    { 0x1D110u, 0x0000u }, { 0x1D111u, 0x0000u }, { 0x1D112u, 0x0000u }, { 0x1D113u, 0x0000u }, 
    { 0x1D114u, 0x0000u }, { 0x1D115u, 0x0000u }, { 0x1D116u, 0x0000u }, { 0x1D117u, 0x0000u }, 
    { 0x1D118u, 0x0000u }, { 0x1D119u, 0x0000u }, { 0x1D11Au, 0x0000u }, { 0x1D11Bu, 0x0000u }, 
    { 0x1D11Cu, 0x0000u }, { 0x1D11Du, 0x0000u }, { 0x1D11Eu, 0x0000u }, { 0x1D11Fu, 0x0000u }, 
    { 0x1D120u, 0x0000u }, { 0x1D121u, 0x0000u }, { 0x1D122u, 0x0000u }, { 0x1D123u, 0x0000u }, 
    { 0x1D124u, 0x0000u }, { 0x1D125u, 0x0000u }, { 0x1D126u, 0x0000u }, { 0x1D127u, 0x0000u }, 
    { 0x1D128u, 0x0000u }, { 0x1D129u, 0x0000u }, { 0x1D12Au, 0x0000u }, { 0x1D12Bu, 0x0000u }, 
    { 0x1D12Cu, 0x0000u }, { 0x1D12Du, 0x0000u }, { 0x1D12Eu, 0x0000u }, { 0x1D12Fu, 0x0000u }, 
    { 0x1D130u, 0x0000u }, { 0x1D131u, 0x0000u }, { 0x1D132u, 0x0000u }, { 0x1D133u, 0x0000u }, 
    { 0x1D134u, 0x0000u }, { 0x1D135u, 0x0000u }, { 0x1D136u, 0x0000u }, { 0x1D137u, 0x0000u }, 
    { 0x1D138u, 0x0000u }, { 0x1D139u, 0x0000u }, { 0x1D13Au, 0x0000u }, { 0x1D13Bu, 0x0000u }, 
    { 0x1D13Cu, 0x0000u }, { 0x1D13Du, 0x0000u }, { 0x1D13Eu, 0x0000u }, { 0x1D13Fu, 0x0000u }, 
    { 0x1D140u, 0x0000u }, { 0x1D141u, 0x0000u }, { 0x1D142u, 0x0000u }, { 0x1D143u, 0x0000u }, 
    { 0x1D144u, 0x0000u }, { 0x1D145u, 0x0000u }, { 0x1D146u, 0x0000u }, { 0x1D147u, 0x0000u }, 
    { 0x1D148u, 0x0000u }, { 0x1D149u, 0x0000u }, { 0x1D14Au, 0x0000u }, { 0x1D14Bu, 0x0000u }, 
    { 0x1D14Cu, 0x0000u }, { 0x1D14Du, 0x0000u }, { 0x1D14Eu, 0x0000u }, { 0x1D14Fu, 0x0000u }, 
    { 0x1D150u, 0x0000u }, { 0x1D151u, 0x0000u }, { 0x1D152u, 0x0000u }, { 0x1D153u, 0x0000u }, 
    { 0x1D154u, 0x0000u }, { 0x1D155u, 0x0000u }, { 0x1D156u, 0x0000u }, { 0x1D157u, 0x0000u }, 
    { 0x1D158u, 0x0000u }, { 0x1D159u, 0x0000u }, { 0x1D15Au, 0x0000u }, { 0x1D15Bu, 0x0000u }, 
    { 0x1D15Cu, 0x0000u }, { 0x1D15Du, 0x0000u }, { 0x1D157u, 0x1D165u }, { 0x1D158u, 0x1D165u }, 
    { 0x1D15Fu, 0x1D16Eu }, { 0x1D15Fu, 0x1D16Fu }, { 0x1D15Fu, 0x1D170u }, { 0x1D15Fu, 0x1D171u }, 
    { 0x1D15Fu, 0x1D172u }, { 0x1D165u, 0x0000u }, { 0x1D166u, 0x0000u }, { 0x1D167u, 0x0000u }, 
    { 0x1D168u, 0x0000u }, { 0x1D169u, 0x0000u }, { 0x1D16Au, 0x0000u }, { 0x1D16Bu, 0x0000u }, 
    { 0x1D16Cu, 0x0000u }, { 0x1D16Du, 0x0000u }, { 0x1D16Eu, 0x0000u }, { 0x1D16Fu, 0x0000u }, 
    { 0x1D170u, 0x0000u }, { 0x1D171u, 0x0000u }, { 0x1D172u, 0x0000u }, { 0x1D173u, 0x0000u }, 
    { 0x1D174u, 0x0000u }, { 0x1D175u, 0x0000u }, { 0x1D176u, 0x0000u }, { 0x1D177u, 0x0000u }, 
    { 0x1D178u, 0x0000u }, { 0x1D179u, 0x0000u }, { 0x1D17Au, 0x0000u }, { 0x1D17Bu, 0x0000u }, 
    { 0x1D17Cu, 0x0000u }, { 0x1D17Du, 0x0000u }, { 0x1D17Eu, 0x0000u }, { 0x1D17Fu, 0x0000u }, 
    { 0x1D180u, 0x0000u }, { 0x1D181u, 0x0000u }, { 0x1D182u, 0x0000u }, { 0x1D183u, 0x0000u }, 
    { 0x1D184u, 0x0000u }, { 0x1D185u, 0x0000u }, { 0x1D186u, 0x0000u }, { 0x1D187u, 0x0000u }, 
    { 0x1D188u, 0x0000u }, { 0x1D189u, 0x0000u }, { 0x1D18Au, 0x0000u }, { 0x1D18Bu, 0x0000u }, 
    { 0x1D18Cu, 0x0000u }, { 0x1D18Du, 0x0000u }, { 0x1D18Eu, 0x0000u }, { 0x1D18Fu, 0x0000u }, 
    { 0x1D190u, 0x0000u }, { 0x1D191u, 0x0000u }, { 0x1D192u, 0x0000u }, { 0x1D193u, 0x0000u }, 
    { 0x1D194u, 0x0000u }, { 0x1D195u, 0x0000u }, { 0x1D196u, 0x0000u }, { 0x1D197u, 0x0000u }, 
    { 0x1D198u, 0x0000u }, { 0x1D199u, 0x0000u }, { 0x1D19Au, 0x0000u }, { 0x1D19Bu, 0x0000u }, 
    { 0x1D19Cu, 0x0000u }, { 0x1D19Du, 0x0000u }, { 0x1D19Eu, 0x0000u }, { 0x1D19Fu, 0x0000u }, 
    { 0x1D1A0u, 0x0000u }, { 0x1D1A1u, 0x0000u }, { 0x1D1A2u, 0x0000u }, { 0x1D1A3u, 0x0000u }, 
    { 0x1D1A4u, 0x0000u }, { 0x1D1A5u, 0x0000u }, { 0x1D1A6u, 0x0000u }, { 0x1D1A7u, 0x0000u }, 
    { 0x1D1A8u, 0x0000u }, { 0x1D1A9u, 0x0000u }, { 0x1D1AAu, 0x0000u }, { 0x1D1ABu, 0x0000u }, 
    { 0x1D1ACu, 0x0000u }, { 0x1D1ADu, 0x0000u }, { 0x1D1AEu, 0x0000u }, { 0x1D1AFu, 0x0000u }, 
    { 0x1D1B0u, 0x0000u }, { 0x1D1B1u, 0x0000u }, { 0x1D1B2u, 0x0000u }, { 0x1D1B3u, 0x0000u }, 
    { 0x1D1B4u, 0x0000u }, { 0x1D1B5u, 0x0000u }, { 0x1D1B6u, 0x0000u }, { 0x1D1B7u, 0x0000u }, 
    { 0x1D1B8u, 0x0000u }, { 0x1D1B9u, 0x0000u }, { 0x1D1BAu, 0x0000u }, { 0x1D1B9u, 0x1D165u }, 
    { 0x1D1BAu, 0x1D165u }, { 0x1D1BBu, 0x1D16Eu }, { 0x1D1BCu, 0x1D16Eu }, { 0x1D1BBu, 0x1D16Fu }, 
    { 0x1D1BCu, 0x1D16Fu }, { 0x1D1C1u, 0x0000u }, { 0x1D1C2u, 0x0000u }, { 0x1D1C3u, 0x0000u }, 
    { 0x1D1C4u, 0x0000u }, { 0x1D1C5u, 0x0000u }, { 0x1D1C6u, 0x0000u }, { 0x1D1C7u, 0x0000u }, 
    { 0x1D1C8u, 0x0000u }, { 0x1D1C9u, 0x0000u }, { 0x1D1CAu, 0x0000u }, { 0x1D1CBu, 0x0000u }, 
    { 0x1D1CCu, 0x0000u }, { 0x1D1CDu, 0x0000u }, { 0x1D1CEu, 0x0000u }, { 0x1D1CFu, 0x0000u }, 
    { 0x1D1D0u, 0x0000u }, { 0x1D1D1u, 0x0000u }, { 0x1D1D2u, 0x0000u }, { 0x1D1D3u, 0x0000u }, 
    { 0x1D1D4u, 0x0000u }, { 0x1D1D5u, 0x0000u }, { 0x1D1D6u, 0x0000u }, { 0x1D1D7u, 0x0000u }, 
    { 0x1D1D8u, 0x0000u }, { 0x1D1D9u, 0x0000u }, { 0x1D1DAu, 0x0000u }, { 0x1D1DBu, 0x0000u }, 
    { 0x1D1DCu, 0x0000u }, { 0x1D1DDu, 0x0000u }, { 0x1D1DEu, 0x0000u }, { 0x1D1DFu, 0x0000u }, 
    { 0x1D1E0u, 0x0000u }, { 0x1D1E1u, 0x0000u }, { 0x1D1E2u, 0x0000u }, { 0x1D1E3u, 0x0000u }, 
    { 0x1D1E4u, 0x0000u }, { 0x1D1E5u, 0x0000u }, { 0x1D1E6u, 0x0000u }, { 0x1D1E7u, 0x0000u }, 
    { 0x1D1E8u, 0x0000u }, { 0x1D1E9u, 0x0000u }, { 0x1D1EAu, 0x0000u }, { 0x1D1EBu, 0x0000u }, 
    { 0x1D1ECu, 0x0000u }, { 0x1D1EDu, 0x0000u }, { 0x1D1EEu, 0x0000u }, { 0x1D1EFu, 0x0000u }, 
    { 0x1D1F0u, 0x0000u }, { 0x1D1F1u, 0x0000u }, { 0x1D1F2u, 0x0000u }, { 0x1D1F3u, 0x0000u }, 
    { 0x1D1F4u, 0x0000u }, { 0x1D1F5u, 0x0000u }, { 0x1D1F6u, 0x0000u }, { 0x1D1F7u, 0x0000u }, 
    { 0x1D1F8u, 0x0000u }, { 0x1D1F9u, 0x0000u }, { 0x1D1FAu, 0x0000u }, { 0x1D1FBu, 0x0000u }, 
    { 0x1D1FCu, 0x0000u }, { 0x1D1FDu, 0x0000u }, { 0x1D1FEu, 0x0000u }, { 0x1D1FFu, 0x0000u }
  };

  const unsigned char Musical_Symbols1D100::m_lb[] = {
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
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, 
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

    const std::bitset<256> Musical_Symbols1D100::m_Diacritic(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000111100000000000000000000000000000011111110011111111000000001111110001110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Musical_Symbols1D100);
