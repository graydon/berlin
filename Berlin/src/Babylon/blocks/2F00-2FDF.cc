/*$Id: 2F00-2FDF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:49:39 +0200.
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

  class Kangxi_Radicals2F00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Kangxi_Radicals2F00() {
      m_first_letter = 0x2F00;
      m_last_letter  = 0x2FDF;
      // m_version="3.1" // Not yet supported!

    }


    ~Kangxi_Radicals2F00() {
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
      return "Kangxi Radicals";
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
      return Babylon::Bidir_Props(BIDIR_ON);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Kangxi_Radicals2F00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(1);
      us[0] = Kangxi_Radicals2F00::m_decompStr[uc - m_first_letter];
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
    Kangxi_Radicals2F00(const Kangxi_Radicals2F00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<224> m_is_defined;
    static const unsigned char _decomp[224];
    static const UCS4 m_decompStr[224];

  }; // class Kangxi_Radicals2F00

    const std::bitset<224> Kangxi_Radicals2F00::m_is_defined(std::string("00000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char Kangxi_Radicals2F00::_decomp[] = {
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS4 Kangxi_Radicals2F00::m_decompStr[] = {
    0x4E00u, 0x4E28u, 0x4E36u, 0x4E3Fu, 
    0x4E59u, 0x4E85u, 0x4E8Cu, 0x4EA0u, 
    0x4EBAu, 0x513Fu, 0x5165u, 0x516Bu, 
    0x5182u, 0x5196u, 0x51ABu, 0x51E0u, 
    0x51F5u, 0x5200u, 0x529Bu, 0x52F9u, 
    0x5315u, 0x531Au, 0x5338u, 0x5341u, 
    0x535Cu, 0x5369u, 0x5382u, 0x53B6u, 
    0x53C8u, 0x53E3u, 0x56D7u, 0x571Fu, 
    0x58EBu, 0x5902u, 0x590Au, 0x5915u, 
    0x5927u, 0x5973u, 0x5B50u, 0x5B80u, 
    0x5BF8u, 0x5C0Fu, 0x5C22u, 0x5C38u, 
    0x5C6Eu, 0x5C71u, 0x5DDBu, 0x5DE5u, 
    0x5DF1u, 0x5DFEu, 0x5E72u, 0x5E7Au, 
    0x5E7Fu, 0x5EF4u, 0x5EFEu, 0x5F0Bu, 
    0x5F13u, 0x5F50u, 0x5F61u, 0x5F73u, 
    0x5FC3u, 0x6208u, 0x6236u, 0x624Bu, 
    0x652Fu, 0x6534u, 0x6587u, 0x6597u, 
    0x65A4u, 0x65B9u, 0x65E0u, 0x65E5u, 
    0x66F0u, 0x6708u, 0x6728u, 0x6B20u, 
    0x6B62u, 0x6B79u, 0x6BB3u, 0x6BCBu, 
    0x6BD4u, 0x6BDBu, 0x6C0Fu, 0x6C14u, 
    0x6C34u, 0x706Bu, 0x722Au, 0x7236u, 
    0x723Bu, 0x723Fu, 0x7247u, 0x7259u, 
    0x725Bu, 0x72ACu, 0x7384u, 0x7389u, 
    0x74DCu, 0x74E6u, 0x7518u, 0x751Fu, 
    0x7528u, 0x7530u, 0x758Bu, 0x7592u, 
    0x7676u, 0x767Du, 0x76AEu, 0x76BFu, 
    0x76EEu, 0x77DBu, 0x77E2u, 0x77F3u, 
    0x793Au, 0x79B8u, 0x79BEu, 0x7A74u, 
    0x7ACBu, 0x7AF9u, 0x7C73u, 0x7CF8u, 
    0x7F36u, 0x7F51u, 0x7F8Au, 0x7FBDu, 
    0x8001u, 0x800Cu, 0x8012u, 0x8033u, 
    0x807Fu, 0x8089u, 0x81E3u, 0x81EAu, 
    0x81F3u, 0x81FCu, 0x820Cu, 0x821Bu, 
    0x821Fu, 0x826Eu, 0x8272u, 0x8278u, 
    0x864Du, 0x866Bu, 0x8840u, 0x884Cu, 
    0x8863u, 0x897Eu, 0x898Bu, 0x89D2u, 
    0x8A00u, 0x8C37u, 0x8C46u, 0x8C55u, 
    0x8C78u, 0x8C9Du, 0x8D64u, 0x8D70u, 
    0x8DB3u, 0x8EABu, 0x8ECAu, 0x8F9Bu, 
    0x8FB0u, 0x8FB5u, 0x9091u, 0x9149u, 
    0x91C6u, 0x91CCu, 0x91D1u, 0x9577u, 
    0x9580u, 0x961Cu, 0x96B6u, 0x96B9u, 
    0x96E8u, 0x9751u, 0x975Eu, 0x9762u, 
    0x9769u, 0x97CBu, 0x97EDu, 0x97F3u, 
    0x9801u, 0x98A8u, 0x98DBu, 0x98DFu, 
    0x9996u, 0x9999u, 0x99ACu, 0x9AA8u, 
    0x9AD8u, 0x9ADFu, 0x9B25u, 0x9B2Fu, 
    0x9B32u, 0x9B3Cu, 0x9B5Au, 0x9CE5u, 
    0x9E75u, 0x9E7Fu, 0x9EA5u, 0x9EBBu, 
    0x9EC3u, 0x9ECDu, 0x9ED1u, 0x9EF9u, 
    0x9EFDu, 0x9F0Eu, 0x9F13u, 0x9F20u, 
    0x9F3Bu, 0x9F4Au, 0x9F52u, 0x9F8Du, 
    0x9F9Cu, 0x9FA0u, 0x2FD6u, 0x2FD7u, 
    0x2FD8u, 0x2FD9u, 0x2FDAu, 0x2FDBu, 
    0x2FDCu, 0x2FDDu, 0x2FDEu, 0x2FDFu
  };

}; // namespace Babylon

dload(Babylon::Kangxi_Radicals2F00);
