/*$Id: FF00-FFEF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:05:15 +0200.
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

  class Halfwidth_and_Fullwidth_FormsFF00 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Halfwidth_and_Fullwidth_FormsFF00() {
      m_first_letter = 0xFF00;
      m_last_letter  = 0xFFEF;
      // m_version="3.1" // Not yet supported!

    }


    ~Halfwidth_and_Fullwidth_FormsFF00() {
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
      return "Halfwidth and Fullwidth Forms";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Halfwidth_and_Fullwidth_FormsFF00::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Halfwidth_and_Fullwidth_FormsFF00::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Halfwidth_and_Fullwidth_FormsFF00::m_title[uc - m_first_letter];
    }

    int dec_digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0xFF10u:
        return 0;
        break;
      case 0xFF11u:
        return 1;
        break;
      case 0xFF12u:
        return 2;
        break;
      case 0xFF13u:
        return 3;
        break;
      case 0xFF14u:
        return 4;
        break;
      case 0xFF15u:
        return 5;
        break;
      case 0xFF16u:
        return 6;
        break;
      case 0xFF17u:
        return 7;
        break;
      case 0xFF18u:
        return 8;
        break;
      case 0xFF19u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0xFF10u:
      case 0xFF11u:
      case 0xFF12u:
      case 0xFF13u:
      case 0xFF14u:
      case 0xFF15u:
      case 0xFF16u:
      case 0xFF17u:
      case 0xFF18u:
      case 0xFF19u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0xFF10u:
        return 0;
        break;
      case 0xFF11u:
        return 1;
        break;
      case 0xFF12u:
        return 2;
        break;
      case 0xFF13u:
        return 3;
        break;
      case 0xFF14u:
        return 4;
        break;
      case 0xFF15u:
        return 5;
        break;
      case 0xFF16u:
        return 6;
        break;
      case 0xFF17u:
        return 7;
        break;
      case 0xFF18u:
        return 8;
        break;
      case 0xFF19u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0xFF10u:
      case 0xFF11u:
      case 0xFF12u:
      case 0xFF13u:
      case 0xFF14u:
      case 0xFF15u:
      case 0xFF16u:
      case 0xFF17u:
      case 0xFF18u:
      case 0xFF19u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0xFF10u:
        return 0.000000;
        break;
      case 0xFF11u:
        return 1.000000;
        break;
      case 0xFF12u:
        return 2.000000;
        break;
      case 0xFF13u:
        return 3.000000;
        break;
      case 0xFF14u:
        return 4.000000;
        break;
      case 0xFF15u:
        return 5.000000;
        break;
      case 0xFF16u:
        return 6.000000;
        break;
      case 0xFF17u:
        return 7.000000;
        break;
      case 0xFF18u:
        return 8.000000;
        break;
      case 0xFF19u:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0xFF10u:
      case 0xFF11u:
      case 0xFF12u:
      case 0xFF13u:
      case 0xFF14u:
      case 0xFF15u:
      case 0xFF16u:
      case 0xFF17u:
      case 0xFF18u:
      case 0xFF19u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Halfwidth_and_Fullwidth_FormsFF00::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Halfwidth_and_Fullwidth_FormsFF00::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Halfwidth_and_Fullwidth_FormsFF00::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(1);
      us[0] = Halfwidth_and_Fullwidth_FormsFF00::m_decompStr[uc - m_first_letter];
      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(Halfwidth_and_Fullwidth_FormsFF00::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Halfwidth_and_Fullwidth_FormsFF00::m_ea[uc - m_first_letter]);
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
      return m_Hex_Digit.test(uc - m_first_letter);
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
    Halfwidth_and_Fullwidth_FormsFF00(const Halfwidth_and_Fullwidth_FormsFF00 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<240> m_is_defined;
    static const UCS4 m_upper[240];
    static const UCS4 m_lower[240];
    static const UCS4 m_title[240];
    static const unsigned char _cat[240];
    static const unsigned char m_bidir[240];
    static const unsigned char _decomp[240];
    static const UCS4 m_decompStr[240];
    static const unsigned char m_lb[240];
    static const unsigned char m_ea[240];
    static const std::bitset<240> m_Terminal_Punctuation;
    static const std::bitset<240> m_Hex_Digit;
    static const std::bitset<240> m_Diacritic;

  }; // class Halfwidth_and_Fullwidth_FormsFF00

    const std::bitset<240> Halfwidth_and_Fullwidth_FormsFF00::m_is_defined(std::string("011111110111111100011100111111001111110011111100011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110"));

  const UCS4 Halfwidth_and_Fullwidth_FormsFF00::m_upper[] = {
    0xFF00, 0xFF01, 0xFF02, 0xFF03, 0xFF04, 0xFF05, 0xFF06, 0xFF07, 
    0xFF08, 0xFF09, 0xFF0A, 0xFF0B, 0xFF0C, 0xFF0D, 0xFF0E, 0xFF0F, 
    0xFF10, 0xFF11, 0xFF12, 0xFF13, 0xFF14, 0xFF15, 0xFF16, 0xFF17, 
    0xFF18, 0xFF19, 0xFF1A, 0xFF1B, 0xFF1C, 0xFF1D, 0xFF1E, 0xFF1F, 
    0xFF20, 0xFF21, 0xFF22, 0xFF23, 0xFF24, 0xFF25, 0xFF26, 0xFF27, 
    0xFF28, 0xFF29, 0xFF2A, 0xFF2B, 0xFF2C, 0xFF2D, 0xFF2E, 0xFF2F, 
    0xFF30, 0xFF31, 0xFF32, 0xFF33, 0xFF34, 0xFF35, 0xFF36, 0xFF37, 
    0xFF38, 0xFF39, 0xFF3A, 0xFF3B, 0xFF3C, 0xFF3D, 0xFF3E, 0xFF3F, 
    0xFF40, 0xFF21, 0xFF22, 0xFF23, 0xFF24, 0xFF25, 0xFF26, 0xFF27, 
    0xFF28, 0xFF29, 0xFF2A, 0xFF2B, 0xFF2C, 0xFF2D, 0xFF2E, 0xFF2F, 
    0xFF30, 0xFF31, 0xFF32, 0xFF33, 0xFF34, 0xFF35, 0xFF36, 0xFF37, 
    0xFF38, 0xFF39, 0xFF3A, 0xFF5B, 0xFF5C, 0xFF5D, 0xFF5E, 0xFF5F, 
    0xFF60, 0xFF61, 0xFF62, 0xFF63, 0xFF64, 0xFF65, 0xFF66, 0xFF67, 
    0xFF68, 0xFF69, 0xFF6A, 0xFF6B, 0xFF6C, 0xFF6D, 0xFF6E, 0xFF6F, 
    0xFF70, 0xFF71, 0xFF72, 0xFF73, 0xFF74, 0xFF75, 0xFF76, 0xFF77, 
    0xFF78, 0xFF79, 0xFF7A, 0xFF7B, 0xFF7C, 0xFF7D, 0xFF7E, 0xFF7F, 
    0xFF80, 0xFF81, 0xFF82, 0xFF83, 0xFF84, 0xFF85, 0xFF86, 0xFF87, 
    0xFF88, 0xFF89, 0xFF8A, 0xFF8B, 0xFF8C, 0xFF8D, 0xFF8E, 0xFF8F, 
    0xFF90, 0xFF91, 0xFF92, 0xFF93, 0xFF94, 0xFF95, 0xFF96, 0xFF97, 
    0xFF98, 0xFF99, 0xFF9A, 0xFF9B, 0xFF9C, 0xFF9D, 0xFF9E, 0xFF9F, 
    0xFFA0, 0xFFA1, 0xFFA2, 0xFFA3, 0xFFA4, 0xFFA5, 0xFFA6, 0xFFA7, 
    0xFFA8, 0xFFA9, 0xFFAA, 0xFFAB, 0xFFAC, 0xFFAD, 0xFFAE, 0xFFAF, 
    0xFFB0, 0xFFB1, 0xFFB2, 0xFFB3, 0xFFB4, 0xFFB5, 0xFFB6, 0xFFB7, 
    0xFFB8, 0xFFB9, 0xFFBA, 0xFFBB, 0xFFBC, 0xFFBD, 0xFFBE, 0xFFBF, 
    0xFFC0, 0xFFC1, 0xFFC2, 0xFFC3, 0xFFC4, 0xFFC5, 0xFFC6, 0xFFC7, 
    0xFFC8, 0xFFC9, 0xFFCA, 0xFFCB, 0xFFCC, 0xFFCD, 0xFFCE, 0xFFCF, 
    0xFFD0, 0xFFD1, 0xFFD2, 0xFFD3, 0xFFD4, 0xFFD5, 0xFFD6, 0xFFD7, 
    0xFFD8, 0xFFD9, 0xFFDA, 0xFFDB, 0xFFDC, 0xFFDD, 0xFFDE, 0xFFDF, 
    0xFFE0, 0xFFE1, 0xFFE2, 0xFFE3, 0xFFE4, 0xFFE5, 0xFFE6, 0xFFE7, 
    0xFFE8, 0xFFE9, 0xFFEA, 0xFFEB, 0xFFEC, 0xFFED, 0xFFEE, 0xFFEF
  };

  const UCS4 Halfwidth_and_Fullwidth_FormsFF00::m_lower[] = {
    0xFF00, 0xFF01, 0xFF02, 0xFF03, 0xFF04, 0xFF05, 0xFF06, 0xFF07, 
    0xFF08, 0xFF09, 0xFF0A, 0xFF0B, 0xFF0C, 0xFF0D, 0xFF0E, 0xFF0F, 
    0xFF10, 0xFF11, 0xFF12, 0xFF13, 0xFF14, 0xFF15, 0xFF16, 0xFF17, 
    0xFF18, 0xFF19, 0xFF1A, 0xFF1B, 0xFF1C, 0xFF1D, 0xFF1E, 0xFF1F, 
    0xFF20, 0xFF41, 0xFF42, 0xFF43, 0xFF44, 0xFF45, 0xFF46, 0xFF47, 
    0xFF48, 0xFF49, 0xFF4A, 0xFF4B, 0xFF4C, 0xFF4D, 0xFF4E, 0xFF4F, 
    0xFF50, 0xFF51, 0xFF52, 0xFF53, 0xFF54, 0xFF55, 0xFF56, 0xFF57, 
    0xFF58, 0xFF59, 0xFF5A, 0xFF3B, 0xFF3C, 0xFF3D, 0xFF3E, 0xFF3F, 
    0xFF40, 0xFF41, 0xFF42, 0xFF43, 0xFF44, 0xFF45, 0xFF46, 0xFF47, 
    0xFF48, 0xFF49, 0xFF4A, 0xFF4B, 0xFF4C, 0xFF4D, 0xFF4E, 0xFF4F, 
    0xFF50, 0xFF51, 0xFF52, 0xFF53, 0xFF54, 0xFF55, 0xFF56, 0xFF57, 
    0xFF58, 0xFF59, 0xFF5A, 0xFF5B, 0xFF5C, 0xFF5D, 0xFF5E, 0xFF5F, 
    0xFF60, 0xFF61, 0xFF62, 0xFF63, 0xFF64, 0xFF65, 0xFF66, 0xFF67, 
    0xFF68, 0xFF69, 0xFF6A, 0xFF6B, 0xFF6C, 0xFF6D, 0xFF6E, 0xFF6F, 
    0xFF70, 0xFF71, 0xFF72, 0xFF73, 0xFF74, 0xFF75, 0xFF76, 0xFF77, 
    0xFF78, 0xFF79, 0xFF7A, 0xFF7B, 0xFF7C, 0xFF7D, 0xFF7E, 0xFF7F, 
    0xFF80, 0xFF81, 0xFF82, 0xFF83, 0xFF84, 0xFF85, 0xFF86, 0xFF87, 
    0xFF88, 0xFF89, 0xFF8A, 0xFF8B, 0xFF8C, 0xFF8D, 0xFF8E, 0xFF8F, 
    0xFF90, 0xFF91, 0xFF92, 0xFF93, 0xFF94, 0xFF95, 0xFF96, 0xFF97, 
    0xFF98, 0xFF99, 0xFF9A, 0xFF9B, 0xFF9C, 0xFF9D, 0xFF9E, 0xFF9F, 
    0xFFA0, 0xFFA1, 0xFFA2, 0xFFA3, 0xFFA4, 0xFFA5, 0xFFA6, 0xFFA7, 
    0xFFA8, 0xFFA9, 0xFFAA, 0xFFAB, 0xFFAC, 0xFFAD, 0xFFAE, 0xFFAF, 
    0xFFB0, 0xFFB1, 0xFFB2, 0xFFB3, 0xFFB4, 0xFFB5, 0xFFB6, 0xFFB7, 
    0xFFB8, 0xFFB9, 0xFFBA, 0xFFBB, 0xFFBC, 0xFFBD, 0xFFBE, 0xFFBF, 
    0xFFC0, 0xFFC1, 0xFFC2, 0xFFC3, 0xFFC4, 0xFFC5, 0xFFC6, 0xFFC7, 
    0xFFC8, 0xFFC9, 0xFFCA, 0xFFCB, 0xFFCC, 0xFFCD, 0xFFCE, 0xFFCF, 
    0xFFD0, 0xFFD1, 0xFFD2, 0xFFD3, 0xFFD4, 0xFFD5, 0xFFD6, 0xFFD7, 
    0xFFD8, 0xFFD9, 0xFFDA, 0xFFDB, 0xFFDC, 0xFFDD, 0xFFDE, 0xFFDF, 
    0xFFE0, 0xFFE1, 0xFFE2, 0xFFE3, 0xFFE4, 0xFFE5, 0xFFE6, 0xFFE7, 
    0xFFE8, 0xFFE9, 0xFFEA, 0xFFEB, 0xFFEC, 0xFFED, 0xFFEE, 0xFFEF
  };

  const UCS4 Halfwidth_and_Fullwidth_FormsFF00::m_title[] = {
    0xFF00, 0xFF01, 0xFF02, 0xFF03, 0xFF04, 0xFF05, 0xFF06, 0xFF07, 
    0xFF08, 0xFF09, 0xFF0A, 0xFF0B, 0xFF0C, 0xFF0D, 0xFF0E, 0xFF0F, 
    0xFF10, 0xFF11, 0xFF12, 0xFF13, 0xFF14, 0xFF15, 0xFF16, 0xFF17, 
    0xFF18, 0xFF19, 0xFF1A, 0xFF1B, 0xFF1C, 0xFF1D, 0xFF1E, 0xFF1F, 
    0xFF20, 0xFF21, 0xFF22, 0xFF23, 0xFF24, 0xFF25, 0xFF26, 0xFF27, 
    0xFF28, 0xFF29, 0xFF2A, 0xFF2B, 0xFF2C, 0xFF2D, 0xFF2E, 0xFF2F, 
    0xFF30, 0xFF31, 0xFF32, 0xFF33, 0xFF34, 0xFF35, 0xFF36, 0xFF37, 
    0xFF38, 0xFF39, 0xFF3A, 0xFF3B, 0xFF3C, 0xFF3D, 0xFF3E, 0xFF3F, 
    0xFF40, 0xFF21, 0xFF22, 0xFF23, 0xFF24, 0xFF25, 0xFF26, 0xFF27, 
    0xFF28, 0xFF29, 0xFF2A, 0xFF2B, 0xFF2C, 0xFF2D, 0xFF2E, 0xFF2F, 
    0xFF30, 0xFF31, 0xFF32, 0xFF33, 0xFF34, 0xFF35, 0xFF36, 0xFF37, 
    0xFF38, 0xFF39, 0xFF3A, 0xFF5B, 0xFF5C, 0xFF5D, 0xFF5E, 0xFF5F, 
    0xFF60, 0xFF61, 0xFF62, 0xFF63, 0xFF64, 0xFF65, 0xFF66, 0xFF67, 
    0xFF68, 0xFF69, 0xFF6A, 0xFF6B, 0xFF6C, 0xFF6D, 0xFF6E, 0xFF6F, 
    0xFF70, 0xFF71, 0xFF72, 0xFF73, 0xFF74, 0xFF75, 0xFF76, 0xFF77, 
    0xFF78, 0xFF79, 0xFF7A, 0xFF7B, 0xFF7C, 0xFF7D, 0xFF7E, 0xFF7F, 
    0xFF80, 0xFF81, 0xFF82, 0xFF83, 0xFF84, 0xFF85, 0xFF86, 0xFF87, 
    0xFF88, 0xFF89, 0xFF8A, 0xFF8B, 0xFF8C, 0xFF8D, 0xFF8E, 0xFF8F, 
    0xFF90, 0xFF91, 0xFF92, 0xFF93, 0xFF94, 0xFF95, 0xFF96, 0xFF97, 
    0xFF98, 0xFF99, 0xFF9A, 0xFF9B, 0xFF9C, 0xFF9D, 0xFF9E, 0xFF9F, 
    0xFFA0, 0xFFA1, 0xFFA2, 0xFFA3, 0xFFA4, 0xFFA5, 0xFFA6, 0xFFA7, 
    0xFFA8, 0xFFA9, 0xFFAA, 0xFFAB, 0xFFAC, 0xFFAD, 0xFFAE, 0xFFAF, 
    0xFFB0, 0xFFB1, 0xFFB2, 0xFFB3, 0xFFB4, 0xFFB5, 0xFFB6, 0xFFB7, 
    0xFFB8, 0xFFB9, 0xFFBA, 0xFFBB, 0xFFBC, 0xFFBD, 0xFFBE, 0xFFBF, 
    0xFFC0, 0xFFC1, 0xFFC2, 0xFFC3, 0xFFC4, 0xFFC5, 0xFFC6, 0xFFC7, 
    0xFFC8, 0xFFC9, 0xFFCA, 0xFFCB, 0xFFCC, 0xFFCD, 0xFFCE, 0xFFCF, 
    0xFFD0, 0xFFD1, 0xFFD2, 0xFFD3, 0xFFD4, 0xFFD5, 0xFFD6, 0xFFD7, 
    0xFFD8, 0xFFD9, 0xFFDA, 0xFFDB, 0xFFDC, 0xFFDD, 0xFFDE, 0xFFDF, 
    0xFFE0, 0xFFE1, 0xFFE2, 0xFFE3, 0xFFE4, 0xFFE5, 0xFFE6, 0xFFE7, 
    0xFFE8, 0xFFE9, 0xFFEA, 0xFFEB, 0xFFEC, 0xFFED, 0xFFEE, 0xFFEF
  };

  const unsigned char Halfwidth_and_Fullwidth_FormsFF00::_cat[] = {
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Sc, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Ps, CAT_Pe, CAT_Po, CAT_Sm, CAT_Po, CAT_Pd, CAT_Po, CAT_Po, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Po, CAT_Po, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Po, 
    CAT_Po, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ps, CAT_Po, CAT_Pe, CAT_Sk, CAT_Pc, 
    CAT_Sk, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ps, CAT_Sm, CAT_Pe, CAT_Sm, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Ps, CAT_Pe, CAT_Po, CAT_Pc, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lm, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lm, CAT_Lm, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Po, 
    CAT_Po, CAT_Po, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Po, CAT_Po, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Po, CAT_Po, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Po, CAT_Po, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Sc, CAT_Sc, CAT_Sm, CAT_Sk, CAT_So, CAT_Sc, CAT_Sc, CAT_Po, 
    CAT_So, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Sm, CAT_So, CAT_So, CAT_Po
  };

  const unsigned char Halfwidth_and_Fullwidth_FormsFF00::m_bidir[] = {
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_CS, BIDIR_ET, BIDIR_CS, BIDIR_ES, 
    BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, 
    BIDIR_EN, BIDIR_EN, BIDIR_CS, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, 
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
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON
  };

  const unsigned char Halfwidth_and_Fullwidth_FormsFF00::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_WIDE, DECOMP_CANONICAL, 
    DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_NARROW, DECOMP_CANONICAL
  };

  const UCS4 Halfwidth_and_Fullwidth_FormsFF00::m_decompStr[] = {
    0xFF00u, 0x0021u, 0x0022u, 0x0023u, 
    0x0024u, 0x0025u, 0x0026u, 0x0027u, 
    0x0028u, 0x0029u, 0x002Au, 0x002Bu, 
    0x002Cu, 0x002Du, 0x002Eu, 0x002Fu, 
    0x0030u, 0x0031u, 0x0032u, 0x0033u, 
    0x0034u, 0x0035u, 0x0036u, 0x0037u, 
    0x0038u, 0x0039u, 0x003Au, 0x003Bu, 
    0x003Cu, 0x003Du, 0x003Eu, 0x003Fu, 
    0x0040u, 0x0041u, 0x0042u, 0x0043u, 
    0x0044u, 0x0045u, 0x0046u, 0x0047u, 
    0x0048u, 0x0049u, 0x004Au, 0x004Bu, 
    0x004Cu, 0x004Du, 0x004Eu, 0x004Fu, 
    0x0050u, 0x0051u, 0x0052u, 0x0053u, 
    0x0054u, 0x0055u, 0x0056u, 0x0057u, 
    0x0058u, 0x0059u, 0x005Au, 0x005Bu, 
    0x005Cu, 0x005Du, 0x005Eu, 0x005Fu, 
    0x0060u, 0x0061u, 0x0062u, 0x0063u, 
    0x0064u, 0x0065u, 0x0066u, 0x0067u, 
    0x0068u, 0x0069u, 0x006Au, 0x006Bu, 
    0x006Cu, 0x006Du, 0x006Eu, 0x006Fu, 
    0x0070u, 0x0071u, 0x0072u, 0x0073u, 
    0x0074u, 0x0075u, 0x0076u, 0x0077u, 
    0x0078u, 0x0079u, 0x007Au, 0x007Bu, 
    0x007Cu, 0x007Du, 0x007Eu, 0xFF5Fu, 
    0xFF60u, 0x3002u, 0x300Cu, 0x300Du, 
    0x3001u, 0x30FBu, 0x30F2u, 0x30A1u, 
    0x30A3u, 0x30A5u, 0x30A7u, 0x30A9u, 
    0x30E3u, 0x30E5u, 0x30E7u, 0x30C3u, 
    0x30FCu, 0x30A2u, 0x30A4u, 0x30A6u, 
    0x30A8u, 0x30AAu, 0x30ABu, 0x30ADu, 
    0x30AFu, 0x30B1u, 0x30B3u, 0x30B5u, 
    0x30B7u, 0x30B9u, 0x30BBu, 0x30BDu, 
    0x30BFu, 0x30C1u, 0x30C4u, 0x30C6u, 
    0x30C8u, 0x30CAu, 0x30CBu, 0x30CCu, 
    0x30CDu, 0x30CEu, 0x30CFu, 0x30D2u, 
    0x30D5u, 0x30D8u, 0x30DBu, 0x30DEu, 
    0x30DFu, 0x30E0u, 0x30E1u, 0x30E2u, 
    0x30E4u, 0x30E6u, 0x30E8u, 0x30E9u, 
    0x30EAu, 0x30EBu, 0x30ECu, 0x30EDu, 
    0x30EFu, 0x30F3u, 0x3099u, 0x309Au, 
    0x3164u, 0x3131u, 0x3132u, 0x3133u, 
    0x3134u, 0x3135u, 0x3136u, 0x3137u, 
    0x3138u, 0x3139u, 0x313Au, 0x313Bu, 
    0x313Cu, 0x313Du, 0x313Eu, 0x313Fu, 
    0x3140u, 0x3141u, 0x3142u, 0x3143u, 
    0x3144u, 0x3145u, 0x3146u, 0x3147u, 
    0x3148u, 0x3149u, 0x314Au, 0x314Bu, 
    0x314Cu, 0x314Du, 0x314Eu, 0xFFBFu, 
    0xFFC0u, 0xFFC1u, 0x314Fu, 0x3150u, 
    0x3151u, 0x3152u, 0x3153u, 0x3154u, 
    0xFFC8u, 0xFFC9u, 0x3155u, 0x3156u, 
    0x3157u, 0x3158u, 0x3159u, 0x315Au, 
    0xFFD0u, 0xFFD1u, 0x315Bu, 0x315Cu, 
    0x315Du, 0x315Eu, 0x315Fu, 0x3160u, 
    0xFFD8u, 0xFFD9u, 0x3161u, 0x3162u, 
    0x3163u, 0xFFDDu, 0xFFDEu, 0xFFDFu, 
    0x00A2u, 0x00A3u, 0x00ACu, 0x00AFu, 
    0x00A6u, 0x00A5u, 0x20A9u, 0xFFE7u, 
    0x2502u, 0x2190u, 0x2191u, 0x2192u, 
    0x2193u, 0x25A0u, 0x25CBu, 0xFFEFu
  };

  const unsigned char Halfwidth_and_Fullwidth_FormsFF00::m_lb[] = {
    LB_EX, LB_EX, LB_ID, LB_ID, LB_PR, LB_PO, LB_ID, LB_ID, 
    LB_OP, LB_CL, LB_ID, LB_ID, LB_CL, LB_ID, LB_CL, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_NS, LB_NS, LB_ID, LB_ID, LB_ID, LB_EX, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_OP, LB_ID, LB_CL, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_OP, LB_ID, LB_CL, LB_ID, LB_EX, 
    LB_EX, LB_CL, LB_OP, LB_CL, LB_CL, LB_NS, LB_AL, LB_NS, 
    LB_NS, LB_NS, LB_NS, LB_NS, LB_NS, LB_NS, LB_NS, LB_NS, 
    LB_NS, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_NS, LB_NS, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_EX, 
    LB_EX, LB_EX, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_EX, LB_EX, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_EX, LB_EX, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_EX, LB_EX, LB_AL, LB_AL, LB_AL, LB_EX, LB_EX, LB_EX, 
    LB_PO, LB_PR, LB_ID, LB_ID, LB_ID, LB_PR, LB_PR, LB_EX, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_EX
  };

  const unsigned char Halfwidth_and_Fullwidth_FormsFF00::m_ea[] = {
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, EA_WIDTH_F, 
    EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_H, EA_WIDTH_F
  };

    const std::bitset<240> Halfwidth_and_Fullwidth_FormsFF00::m_Terminal_Punctuation(std::string("000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000"));

    const std::bitset<240> Halfwidth_and_Fullwidth_FormsFF00::m_Hex_Digit(std::string("000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111100000000000000000000000000111111000000011111111110000000000000000"));

    const std::bitset<240> Halfwidth_and_Fullwidth_FormsFF00::m_Diacritic(std::string("000000000000000000000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Halfwidth_and_Fullwidth_FormsFF00);
