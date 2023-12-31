/*$Id: String.cc,v 1.8 2001/04/13 23:03:05 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999,2000 Tobias Hunger <Tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

#include <Babylon/Char.hh>
#include <Babylon/String.hh>

// CONSTRUCTORS:
Babylon::String::String() {
    resize(0);
    m_current_norm = NORM_NONE;
}

Babylon::String::String(const Char uc, Norm norm = NORM_NONE) {
    resize(1);
    (*this)[0] = uc;
    m_current_norm = norm;
}

Babylon::String::String(const UCS4 uc, Norm norm = NORM_NONE) {
    resize(1);
    (*this)[0] = uc;
    m_current_norm = norm;
}

Babylon::String::String(const UTF8_string & s, const Norm norm = NORM_NONE) {
    utf8(s, norm);
}

Babylon::String::String(const char * s, const Norm norm = NORM_NONE) {
    utf8(s, norm);
}

Babylon::String::String(size_t len, Char * data, const Norm norm = NORM_NONE) {
    this->assign(data, len);
}

Babylon::String::String(const String & us) {
    resize(us.length());
    
    Babylon::String::const_iterator   us_it = us.begin();
    Babylon::String::iterator       this_it = this->begin();
    
    while (us_it != us.end() && this_it != this->end()) {
	*this_it = *us_it;
	++this_it; ++us_it;
    }
    m_current_norm = us.norm();
}

Babylon::String::String(const UTF32_string & us, Norm norm = NORM_NONE) {
    resize(us.length());
    
    Babylon::UTF32_string::const_iterator us_it = us.begin();
    Babylon::String::iterator this_it = this->begin();
    
    while (us_it != us.end() && this_it != this->end()) {
	*this_it = *us_it;
	++this_it; ++us_it;
    }
    m_current_norm = norm;
}


// DESTRUCTORS

Babylon::String::~String() {}

void Babylon::String::utf8(const UTF8_string & s, Norm norm = NORM_NONE)
    throw (Trans_Error) {
    m_current_norm = norm;
    erase();
    
    UTF8_string::const_iterator it = s.begin();
    while(it != s.end()) {
	Char t;
	it = t.utf8(s, it);
	*this += t;
    }
}

Babylon::UTF8_string Babylon::String::utf8() const throw (Trans_Error) {
    Babylon::UTF8_string res;

    for(String::const_iterator it = this->begin();
	it != this->end();
	++it)
	res += it->utf8();
    return res;
}

void Babylon::String::utf16(const UTF16_string & in , const Norm norm = NORM_NONE)
    throw (Trans_Error) {
    m_current_norm = norm;
    erase();

    UTF16_string::const_iterator it = in.begin();
    while(it != in.end()) {
	Char t;
	it = t.utf16(in, it);
	*this += t;
    }
}

Babylon::UTF16_string Babylon::String::utf16() const throw (Trans_Error) {
    UTF16_string res;

    for(String::const_iterator it = this->begin();
	it != this->end();
	++it)
	res += it->utf16();
    return res;
}

void Babylon::String::utf32(const UTF32_string & s, const Norm norm = NORM_NONE) {
    erase();
    m_current_norm = norm;
    UTF32_string::const_iterator it = s.begin();
    while(it != s.end()) {
	Char t;
	it = t.utf32(s, it);
	*this += t;
    }
}

Babylon::UTF32_string Babylon::String::utf32() const throw (Trans_Error) {
    UTF32_string res;

    for(String::const_iterator it = this->begin();
	it != this->end();
	++it)
	res += it->utf32();
    return res;
}

void Babylon::String::swap(String & that) {
    std::swap(*this, that);
    std::swap(m_current_norm, that.m_current_norm);
}

void Babylon::String::normalize(const Norm norm) {
    String result;
    if (length() > 0 && norm < NORM_NONE && norm != m_current_norm) {
	Dictionary * dict = Dictionary::instance();
	
	// do I need to decompose?
	if (m_current_norm!=NORM_D || (norm!=NORM_KC && m_current_norm!=NORM_NONE)) {
	    bool compat = (norm & 2); // compatibility bit
	    for(String::const_iterator i = this->begin();
		i != this->end(); ++i) {
		String tmp = dict->recursive_decompose(compat, i->value());
		
		for(String::const_iterator j = tmp.begin();
		    j != tmp.end(); ++j) {
		    Can_Comb_Class c_class = dict->find_char(j->value())->
			comb_class(j->value());
		    String::iterator k = result.end();
		    if (c_class == 0)
			result += *j;
		    else {
			for (; k >= result.begin(); k--)
			    if (dict->find_char((k-1)->value())->
				comb_class((k-1)->value()) <= c_class) break;
			result.insert(k, *j);
		    }
		}
	    }
	}      
	
	// do I need to compose?
	if (m_current_norm != NORM_C && (norm & 1)) {
	    // decomposition skipped?
	    if (result.length() == 0) result = *this;
	    
	    String::iterator starter = result.begin();
	    String::iterator comp_pos = starter + 1;
	    Can_Comb_Class last_class = dict->find_char(starter->value())->
		comb_class(starter->value());
	    if (last_class != 0)
		last_class = Can_Comb_Class(256); // fix for irregular comb sequence
	    
	    for(String::iterator ch = starter + 1;
		ch != result.end(); ++ch) {
		Can_Comb_Class ch_class = dict->find_char(ch->value())->
		    comb_class(ch->value());
		UCS4 composite = dict->find_char(starter->value())->
		    compose(starter->value(), ch->value());
		
		if(composite != 0 && (last_class < ch_class || last_class == 0))
		    *starter = composite;
		else {
		    if(ch_class == 0)
			starter = comp_pos;
		    last_class=ch_class;
		    *comp_pos = *ch;
		    comp_pos++;
		}
	    }
	    result.resize(comp_pos - result.begin());
	} // compose
    }
    if(result.length() != 0) {
	m_current_norm = norm;
	*this = result;
    }
}

Babylon::String Babylon::String::norm(Babylon::Norm norm) const {
    String tmp = *this;
    tmp.normalize(norm);
    return tmp;
}

/*
void Babylon::String::erase() {
    m_current_norm = Babylon::NORM_NONE;
    std::basic_string<Char>::erase();
}
*/
