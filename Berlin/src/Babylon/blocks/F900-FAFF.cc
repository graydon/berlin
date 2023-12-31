/*$Id: F900-FAFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:04:35 +0200.
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

  class CJK_Compatibility_IdeographsF900 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    CJK_Compatibility_IdeographsF900() {
      m_first_letter = 0xF900;
      m_last_letter  = 0xFAFF;
      // m_version="3.1" // Not yet supported!

    }


    ~CJK_Compatibility_IdeographsF900() {
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
      return "CJK Compatibility Ideographs";
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
      return Babylon::Gen_Cat(CAT_Lo);
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
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(1);
      us[0] = CJK_Compatibility_IdeographsF900::m_decompStr[uc - m_first_letter];
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
      return m_Ideographic.test(uc - m_first_letter);
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
    CJK_Compatibility_IdeographsF900(const CJK_Compatibility_IdeographsF900 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<512> m_is_defined;
    static const UCS4 m_decompStr[512];
    static const std::bitset<512> m_Ideographic;

  }; // class CJK_Compatibility_IdeographsF900

    const std::bitset<512> CJK_Compatibility_IdeographsF900::m_is_defined(std::string("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const UCS4 CJK_Compatibility_IdeographsF900::m_decompStr[] = {
    0x8C48u, 0x66F4u, 0x8ECAu, 0x8CC8u, 
    0x6ED1u, 0x4E32u, 0x53E5u, 0x9F9Cu, 
    0x9F9Cu, 0x5951u, 0x91D1u, 0x5587u, 
    0x5948u, 0x61F6u, 0x7669u, 0x7F85u, 
    0x863Fu, 0x87BAu, 0x88F8u, 0x908Fu, 
    0x6A02u, 0x6D1Bu, 0x70D9u, 0x73DEu, 
    0x843Du, 0x916Au, 0x99F1u, 0x4E82u, 
    0x5375u, 0x6B04u, 0x721Bu, 0x862Du, 
    0x9E1Eu, 0x5D50u, 0x6FEBu, 0x85CDu, 
    0x8964u, 0x62C9u, 0x81D8u, 0x881Fu, 
    0x5ECAu, 0x6717u, 0x6D6Au, 0x72FCu, 
    0x90CEu, 0x4F86u, 0x51B7u, 0x52DEu, 
    0x64C4u, 0x6AD3u, 0x7210u, 0x76E7u, 
    0x8001u, 0x8606u, 0x865Cu, 0x8DEFu, 
    0x9732u, 0x9B6Fu, 0x9DFAu, 0x788Cu, 
    0x797Fu, 0x7DA0u, 0x83C9u, 0x9304u, 
    0x9E7Fu, 0x8AD6u, 0x58DFu, 0x5F04u, 
    0x7C60u, 0x807Eu, 0x7262u, 0x78CAu, 
    0x8CC2u, 0x96F7u, 0x58D8u, 0x5C62u, 
    0x6A13u, 0x6DDAu, 0x6F0Fu, 0x7D2Fu, 
    0x7E37u, 0x96FBu, 0x52D2u, 0x808Bu, 
    0x51DCu, 0x51CCu, 0x7A1Cu, 0x7DBEu, 
    0x83F1u, 0x9675u, 0x8B80u, 0x62CFu, 
    0x6A02u, 0x8AFEu, 0x4E39u, 0x5BE7u, 
    0x6012u, 0x7387u, 0x7570u, 0x5317u, 
    0x78FBu, 0x4FBFu, 0x5FA9u, 0x4E0Du, 
    0x6CCCu, 0x6578u, 0x7D22u, 0x53C3u, 
    0x585Eu, 0x7701u, 0x8449u, 0x8AAAu, 
    0x6BBAu, 0x8FB0u, 0x6C88u, 0x62FEu, 
    0x82E5u, 0x63A0u, 0x7565u, 0x4EAEu, 
    0x5169u, 0x51C9u, 0x6881u, 0x7CE7u, 
    0x826Fu, 0x8AD2u, 0x91CFu, 0x52F5u, 
    0x5442u, 0x5973u, 0x5EECu, 0x65C5u, 
    0x6FFEu, 0x792Au, 0x95ADu, 0x9A6Au, 
    0x9E97u, 0x9ECEu, 0x529Bu, 0x66C6u, 
    0x6B77u, 0x8F62u, 0x5E74u, 0x6190u, 
    0x6200u, 0x649Au, 0x6F23u, 0x7149u, 
    0x7489u, 0x79CAu, 0x7DF4u, 0x806Fu, 
    0x8F26u, 0x84EEu, 0x9023u, 0x934Au, 
    0x5217u, 0x52A3u, 0x54BDu, 0x70C8u, 
    0x88C2u, 0x8AAAu, 0x5EC9u, 0x5FF5u, 
    0x637Bu, 0x6BAEu, 0x7C3Eu, 0x7375u, 
    0x4EE4u, 0x56F9u, 0x5BE7u, 0x5DBAu, 
    0x601Cu, 0x73B2u, 0x7469u, 0x7F9Au, 
    0x8046u, 0x9234u, 0x96F6u, 0x9748u, 
    0x9818u, 0x4F8Bu, 0x79AEu, 0x91B4u, 
    0x96B8u, 0x60E1u, 0x4E86u, 0x50DAu, 
    0x5BEEu, 0x5C3Fu, 0x6599u, 0x6A02u, 
    0x71CEu, 0x7642u, 0x84FCu, 0x907Cu, 
    0x9F8Du, 0x6688u, 0x962Eu, 0x5289u, 
    0x677Bu, 0x67F3u, 0x6D41u, 0x6E9Cu, 
    0x7409u, 0x7559u, 0x786Bu, 0x7D10u, 
    0x985Eu, 0x516Du, 0x622Eu, 0x9678u, 
    0x502Bu, 0x5D19u, 0x6DEAu, 0x8F2Au, 
    0x5F8Bu, 0x6144u, 0x6817u, 0x7387u, 
    0x9686u, 0x5229u, 0x540Fu, 0x5C65u, 
    0x6613u, 0x674Eu, 0x68A8u, 0x6CE5u, 
    0x7406u, 0x75E2u, 0x7F79u, 0x88CFu, 
    0x88E1u, 0x91CCu, 0x96E2u, 0x533Fu, 
    0x6EBAu, 0x541Du, 0x71D0u, 0x7498u, 
    0x85FAu, 0x96A3u, 0x9C57u, 0x9E9Fu, 
    0x6797u, 0x6DCBu, 0x81E8u, 0x7ACBu, 
    0x7B20u, 0x7C92u, 0x72C0u, 0x7099u, 
    0x8B58u, 0x4EC0u, 0x8336u, 0x523Au, 
    0x5207u, 0x5EA6u, 0x62D3u, 0x7CD6u, 
    0x5B85u, 0x6D1Eu, 0x66B4u, 0x8F3Bu, 
    0x884Cu, 0x964Du, 0x898Bu, 0x5ED3u, 
    0x5140u, 0x55C0u, 0xFA0Eu, 0xFA0Fu, 
    0x585Au, 0xFA11u, 0x6674u, 0xFA13u, 
    0xFA14u, 0x51DEu, 0x732Au, 0x76CAu, 
    0x793Cu, 0x795Eu, 0x7965u, 0x798Fu, 
    0x9756u, 0x7CBEu, 0x7FBDu, 0xFA1Fu, 
    0x8612u, 0xFA21u, 0x8AF8u, 0xFA23u, 
    0xFA24u, 0x9038u, 0x90FDu, 0xFA27u, 
    0xFA28u, 0xFA29u, 0x98EFu, 0x98FCu, 
    0x9928u, 0x9DB4u, 0xFA2Eu, 0xFA2Fu, 
    0xFA30u, 0xFA31u, 0xFA32u, 0xFA33u, 
    0xFA34u, 0xFA35u, 0xFA36u, 0xFA37u, 
    0xFA38u, 0xFA39u, 0xFA3Au, 0xFA3Bu, 
    0xFA3Cu, 0xFA3Du, 0xFA3Eu, 0xFA3Fu, 
    0xFA40u, 0xFA41u, 0xFA42u, 0xFA43u, 
    0xFA44u, 0xFA45u, 0xFA46u, 0xFA47u, 
    0xFA48u, 0xFA49u, 0xFA4Au, 0xFA4Bu, 
    0xFA4Cu, 0xFA4Du, 0xFA4Eu, 0xFA4Fu, 
    0xFA50u, 0xFA51u, 0xFA52u, 0xFA53u, 
    0xFA54u, 0xFA55u, 0xFA56u, 0xFA57u, 
    0xFA58u, 0xFA59u, 0xFA5Au, 0xFA5Bu, 
    0xFA5Cu, 0xFA5Du, 0xFA5Eu, 0xFA5Fu, 
    0xFA60u, 0xFA61u, 0xFA62u, 0xFA63u, 
    0xFA64u, 0xFA65u, 0xFA66u, 0xFA67u, 
    0xFA68u, 0xFA69u, 0xFA6Au, 0xFA6Bu, 
    0xFA6Cu, 0xFA6Du, 0xFA6Eu, 0xFA6Fu, 
    0xFA70u, 0xFA71u, 0xFA72u, 0xFA73u, 
    0xFA74u, 0xFA75u, 0xFA76u, 0xFA77u, 
    0xFA78u, 0xFA79u, 0xFA7Au, 0xFA7Bu, 
    0xFA7Cu, 0xFA7Du, 0xFA7Eu, 0xFA7Fu, 
    0xFA80u, 0xFA81u, 0xFA82u, 0xFA83u, 
    0xFA84u, 0xFA85u, 0xFA86u, 0xFA87u, 
    0xFA88u, 0xFA89u, 0xFA8Au, 0xFA8Bu, 
    0xFA8Cu, 0xFA8Du, 0xFA8Eu, 0xFA8Fu, 
    0xFA90u, 0xFA91u, 0xFA92u, 0xFA93u, 
    0xFA94u, 0xFA95u, 0xFA96u, 0xFA97u, 
    0xFA98u, 0xFA99u, 0xFA9Au, 0xFA9Bu, 
    0xFA9Cu, 0xFA9Du, 0xFA9Eu, 0xFA9Fu, 
    0xFAA0u, 0xFAA1u, 0xFAA2u, 0xFAA3u, 
    0xFAA4u, 0xFAA5u, 0xFAA6u, 0xFAA7u, 
    0xFAA8u, 0xFAA9u, 0xFAAAu, 0xFAABu, 
    0xFAACu, 0xFAADu, 0xFAAEu, 0xFAAFu, 
    0xFAB0u, 0xFAB1u, 0xFAB2u, 0xFAB3u, 
    0xFAB4u, 0xFAB5u, 0xFAB6u, 0xFAB7u, 
    0xFAB8u, 0xFAB9u, 0xFABAu, 0xFABBu, 
    0xFABCu, 0xFABDu, 0xFABEu, 0xFABFu, 
    0xFAC0u, 0xFAC1u, 0xFAC2u, 0xFAC3u, 
    0xFAC4u, 0xFAC5u, 0xFAC6u, 0xFAC7u, 
    0xFAC8u, 0xFAC9u, 0xFACAu, 0xFACBu, 
    0xFACCu, 0xFACDu, 0xFACEu, 0xFACFu, 
    0xFAD0u, 0xFAD1u, 0xFAD2u, 0xFAD3u, 
    0xFAD4u, 0xFAD5u, 0xFAD6u, 0xFAD7u, 
    0xFAD8u, 0xFAD9u, 0xFADAu, 0xFADBu, 
    0xFADCu, 0xFADDu, 0xFADEu, 0xFADFu, 
    0xFAE0u, 0xFAE1u, 0xFAE2u, 0xFAE3u, 
    0xFAE4u, 0xFAE5u, 0xFAE6u, 0xFAE7u, 
    0xFAE8u, 0xFAE9u, 0xFAEAu, 0xFAEBu, 
    0xFAECu, 0xFAEDu, 0xFAEEu, 0xFAEFu, 
    0xFAF0u, 0xFAF1u, 0xFAF2u, 0xFAF3u, 
    0xFAF4u, 0xFAF5u, 0xFAF6u, 0xFAF7u, 
    0xFAF8u, 0xFAF9u, 0xFAFAu, 0xFAFBu, 
    0xFAFCu, 0xFAFDu, 0xFAFEu, 0xFAFFu
  };

    const std::bitset<512> CJK_Compatibility_IdeographsF900::m_Ideographic(std::string("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

}; // namespace Babylon

dload(Babylon::CJK_Compatibility_IdeographsF900);
