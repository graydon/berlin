/*$Id: 2F800-2FA1F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:21:26 +0200.
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

  class CJK_Compatibility_Ideographs_Supplement2F800 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    CJK_Compatibility_Ideographs_Supplement2F800() {
      m_first_letter = 0x2F800;
      m_last_letter  = 0x2FA1F;
      // m_version="3.1" // Not yet supported!

    }


    ~CJK_Compatibility_Ideographs_Supplement2F800() {
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
      return "CJK Compatibility Ideographs Supplement";
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
      us[0] = CJK_Compatibility_Ideographs_Supplement2F800::m_decompStr[uc - m_first_letter];
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
    CJK_Compatibility_Ideographs_Supplement2F800(const CJK_Compatibility_Ideographs_Supplement2F800 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<544> m_is_defined;
    static const UCS4 m_decompStr[544];
    static const std::bitset<544> m_Ideographic;

  }; // class CJK_Compatibility_Ideographs_Supplement2F800

    const std::bitset<544> CJK_Compatibility_Ideographs_Supplement2F800::m_is_defined(std::string("0011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const UCS4 CJK_Compatibility_Ideographs_Supplement2F800::m_decompStr[] = {
    0x4E3Du, 0x4E38u, 0x4E41u, 0x20122u, 
    0x4F60u, 0x4FAEu, 0x4FBBu, 0x5002u, 
    0x507Au, 0x5099u, 0x50E7u, 0x50CFu, 
    0x349Eu, 0x2063Au, 0x514Du, 0x5154u, 
    0x5164u, 0x5177u, 0x2051Cu, 0x34B9u, 
    0x5167u, 0x518Du, 0x2054Bu, 0x5197u, 
    0x51A4u, 0x4ECCu, 0x51ACu, 0x51B5u, 
    0x291DFu, 0x51F5u, 0x5203u, 0x34DFu, 
    0x523Bu, 0x5246u, 0x5272u, 0x5277u, 
    0x3515u, 0x52C7u, 0x52C9u, 0x52E4u, 
    0x52FAu, 0x5305u, 0x5306u, 0x5317u, 
    0x5349u, 0x5351u, 0x535Au, 0x5373u, 
    0x537Du, 0x537Fu, 0x537Fu, 0x537Fu, 
    0x20A2Cu, 0x7070u, 0x53CAu, 0x53DFu, 
    0x20B63u, 0x53EBu, 0x53F1u, 0x5406u, 
    0x549Eu, 0x5438u, 0x5448u, 0x5468u, 
    0x54A2u, 0x54F6u, 0x5510u, 0x5553u, 
    0x5563u, 0x5584u, 0x5584u, 0x5599u, 
    0x55ABu, 0x55B3u, 0x55C2u, 0x5716u, 
    0x5606u, 0x5717u, 0x5651u, 0x5674u, 
    0x5207u, 0x58EEu, 0x57CEu, 0x57F4u, 
    0x580Du, 0x578Bu, 0x5832u, 0x5831u, 
    0x58ACu, 0x214E4u, 0x58F2u, 0x58F7u, 
    0x5906u, 0x591Au, 0x5922u, 0x5962u, 
    0x216A8u, 0x216EAu, 0x59ECu, 0x5A1Bu, 
    0x5A27u, 0x59D8u, 0x5A66u, 0x36EEu, 
    0x2136Au, 0x5B08u, 0x5B3Eu, 0x5B3Eu, 
    0x219C8u, 0x5BC3u, 0x5BD8u, 0x5BE7u, 
    0x5BF3u, 0x21B18u, 0x5BFFu, 0x5C06u, 
    0x5F33u, 0x5C22u, 0x3781u, 0x5C60u, 
    0x5C6Eu, 0x5CC0u, 0x5C8Du, 0x21DE4u, 
    0x5D43u, 0x21DE6u, 0x5D6Eu, 0x5D6Bu, 
    0x5D7Cu, 0x5DE1u, 0x5DE2u, 0x382Fu, 
    0x5DFDu, 0x5E28u, 0x5E3Du, 0x5E69u, 
    0x3862u, 0x22183u, 0x387Cu, 0x5EB0u, 
    0x5EB3u, 0x5EB6u, 0x5ECAu, 0x2A392u, 
    0x5EFEu, 0x22331u, 0x22331u, 0x8201u, 
    0x5F22u, 0x5F22u, 0x38C7u, 0x232B8u, 
    0x261DAu, 0x5F62u, 0x5F6Bu, 0x38E3u, 
    0x5F9Au, 0x5FCDu, 0x5FD7u, 0x5FF9u, 
    0x6081u, 0x393Au, 0x391Cu, 0x6094u, 
    0x226D4u, 0x60C7u, 0x6148u, 0x614Cu, 
    0x614Eu, 0x614Cu, 0x617Au, 0x618Eu, 
    0x61B2u, 0x61A4u, 0x61AFu, 0x61DEu, 
    0x61F2u, 0x61F6u, 0x6210u, 0x621Bu, 
    0x625Du, 0x62B1u, 0x62D4u, 0x6350u, 
    0x22B0Cu, 0x633Du, 0x62FCu, 0x6368u, 
    0x6383u, 0x63E4u, 0x22BF1u, 0x6422u, 
    0x63C5u, 0x63A9u, 0x3A2Eu, 0x6469u, 
    0x647Eu, 0x649Du, 0x6477u, 0x3A6Cu, 
    0x654Fu, 0x656Cu, 0x2300Au, 0x65E3u, 
    0x66F8u, 0x6649u, 0x3B19u, 0x6691u, 
    0x3B08u, 0x3AE4u, 0x5192u, 0x5195u, 
    0x6700u, 0x669Cu, 0x80ADu, 0x43D9u, 
    0x6717u, 0x671Bu, 0x6721u, 0x675Eu, 
    0x6753u, 0x233C3u, 0x3B49u, 0x67FAu, 
    0x6785u, 0x6852u, 0x6885u, 0x2346Du, 
    0x688Eu, 0x681Fu, 0x6914u, 0x3B9Du, 
    0x6942u, 0x69A3u, 0x69EAu, 0x6AA8u, 
    0x236A3u, 0x6ADBu, 0x3C18u, 0x6B21u, 
    0x238A7u, 0x6B54u, 0x3C4Eu, 0x6B72u, 
    0x6B9Fu, 0x6BBAu, 0x6BBBu, 0x23A8Du, 
    0x21D0Bu, 0x23AFAu, 0x6C4Eu, 0x23CBCu, 
    0x6CBFu, 0x6CCDu, 0x6C67u, 0x6D16u, 
    0x6D3Eu, 0x6D77u, 0x6D41u, 0x6D69u, 
    0x6D78u, 0x6D85u, 0x23D1Eu, 0x6D34u, 
    0x6E2Fu, 0x6E6Eu, 0x3D33u, 0x6ECBu, 
    0x6EC7u, 0x23ED1u, 0x6DF9u, 0x6F6Eu, 
    0x23F5Eu, 0x23F8Eu, 0x6FC6u, 0x7039u, 
    0x701Eu, 0x701Bu, 0x3D96u, 0x704Au, 
    0x707Du, 0x7077u, 0x70ADu, 0x20525u, 
    0x7145u, 0x24263u, 0x719Cu, 0x43ABu, 
    0x7228u, 0x7235u, 0x7250u, 0x24608u, 
    0x7280u, 0x7295u, 0x24735u, 0x24814u, 
    0x737Au, 0x738Bu, 0x3EACu, 0x73A5u, 
    0x3EB8u, 0x3EB8u, 0x7447u, 0x745Cu, 
    0x7471u, 0x7485u, 0x74CAu, 0x3F1Bu, 
    0x7524u, 0x24C36u, 0x753Eu, 0x24C92u, 
    0x7570u, 0x2219Fu, 0x7610u, 0x24FA1u, 
    0x24FB8u, 0x25044u, 0x3FFCu, 0x4008u, 
    0x76F4u, 0x250F3u, 0x250F2u, 0x25119u, 
    0x25133u, 0x771Eu, 0x771Fu, 0x771Fu, 
    0x774Au, 0x4039u, 0x778Bu, 0x4046u, 
    0x4096u, 0x2541Du, 0x784Eu, 0x788Cu, 
    0x78CCu, 0x40E3u, 0x25626u, 0x7956u, 
    0x2569Au, 0x256C5u, 0x798Fu, 0x79EBu, 
    0x412Fu, 0x7A40u, 0x7A4Au, 0x7A4Fu, 
    0x2597Cu, 0x25AA7u, 0x25AA7u, 0x7AAEu, 
    0x4202u, 0x25BABu, 0x7BC6u, 0x7BC9u, 
    0x4227u, 0x25C80u, 0x7CD2u, 0x42A0u, 
    0x7CE8u, 0x7CE3u, 0x7D00u, 0x25F86u, 
    0x7D63u, 0x4301u, 0x7DC7u, 0x7E02u, 
    0x7E45u, 0x4334u, 0x26228u, 0x26247u, 
    0x4359u, 0x262D9u, 0x7F7Au, 0x2633Eu, 
    0x7F95u, 0x7FFAu, 0x8005u, 0x264DAu, 
    0x26523u, 0x8060u, 0x265A8u, 0x8070u, 
    0x2335Fu, 0x43D5u, 0x80B2u, 0x8103u, 
    0x440Bu, 0x813Eu, 0x5AB5u, 0x267A7u, 
    0x267B5u, 0x23393u, 0x2339Cu, 0x8201u, 
    0x8204u, 0x8F9Eu, 0x446Bu, 0x8291u, 
    0x828Bu, 0x829Du, 0x52B3u, 0x82B1u, 
    0x82B3u, 0x82BDu, 0x82E6u, 0x26B3Cu, 
    0x82E5u, 0x831Du, 0x8363u, 0x83ADu, 
    0x8323u, 0x83BDu, 0x83E7u, 0x8457u, 
    0x8353u, 0x83CAu, 0x83CCu, 0x83DCu, 
    0x26C36u, 0x26D6Bu, 0x26CD5u, 0x452Bu, 
    0x84F1u, 0x84F3u, 0x8516u, 0x273CAu, 
    0x8564u, 0x26F2Cu, 0x455Du, 0x4561u, 
    0x26FB1u, 0x270D2u, 0x456Bu, 0x8650u, 
    0x865Cu, 0x8667u, 0x8669u, 0x86A9u, 
    0x8688u, 0x870Eu, 0x86E2u, 0x8779u, 
    0x8728u, 0x876Bu, 0x8786u, 0x4D57u, 
    0x87E1u, 0x8801u, 0x45F9u, 0x8860u, 
    0x8863u, 0x27667u, 0x88D7u, 0x88DEu, 
    0x4635u, 0x88FAu, 0x34BBu, 0x278AEu, 
    0x27966u, 0x46BEu, 0x46C7u, 0x8AA0u, 
    0x8AEDu, 0x8B8Au, 0x8C55u, 0x27CA8u, 
    0x8CABu, 0x8CC1u, 0x8D1Bu, 0x8D77u, 
    0x27F2Fu, 0x20804u, 0x8DCBu, 0x8DBCu, 
    0x8DF0u, 0x208DEu, 0x8ED4u, 0x8F38u, 
    0x285D2u, 0x285EDu, 0x9094u, 0x90F1u, 
    0x9111u, 0x2872Eu, 0x911Bu, 0x9238u, 
    0x92D7u, 0x92D8u, 0x927Cu, 0x93F9u, 
    0x9415u, 0x28BFAu, 0x958Bu, 0x4995u, 
    0x95B7u, 0x28D77u, 0x49E6u, 0x96C3u, 
    0x5DB2u, 0x9723u, 0x29145u, 0x2921Au, 
    0x4A6Eu, 0x4A76u, 0x97E0u, 0x2940Au, 
    0x4AB2u, 0x29496u, 0x980Bu, 0x980Bu, 
    0x9829u, 0x295B6u, 0x98E2u, 0x4B33u, 
    0x9929u, 0x99A7u, 0x99C2u, 0x99FEu, 
    0x4BCEu, 0x29B30u, 0x9B12u, 0x9C40u, 
    0x9CFDu, 0x4CCEu, 0x4CEDu, 0x9D67u, 
    0x2A0CEu, 0x4CF8u, 0x2A105u, 0x2A20Eu, 
    0x2A291u, 0x9EBBu, 0x4D56u, 0x9EF9u, 
    0x9EFEu, 0x9F05u, 0x9F0Fu, 0x9F16u, 
    0x9F3Bu, 0x2A600u, 0x2FA1Eu, 0x2FA1Fu
  };

    const std::bitset<544> CJK_Compatibility_Ideographs_Supplement2F800::m_Ideographic(std::string("0011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

}; // namespace Babylon

dload(Babylon::CJK_Compatibility_Ideographs_Supplement2F800);
