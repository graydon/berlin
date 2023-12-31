/*$Id: Dictionary.cc,v 1.8 2001/03/31 09:43:06 tobias Exp $
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

#include <Babylon/Dictionary.hh>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <functional>

using namespace Prague;
using namespace Babylon;

// static variables:
Dictionary             *Dictionary::m_dictionary = 0;
Dictionary::Dict_Guard  Dictionary::m_guard;
Mutex                   Dictionary::m_singleton_mutex;


bool Dictionary::is_defined(const UCS4 uc)
    throw (Block_Error) {
    bool result = 0;
    Prague::Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_defined(uc);
    return result;
}


UCS4 Dictionary::uppercase(const UCS4 uc) 
    throw (Block_Error) {
    UCS4 result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->uppercase(uc);
    return result;
}


UCS4 Dictionary::lowercase(const UCS4 uc) 
    throw (Block_Error) {
    UCS4 result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->lowercase(uc);
    return result;
}


UCS4 Dictionary::titlecase(const UCS4 uc) 
    throw (Block_Error) {
    UCS4 result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->titlecase(uc);
    return result;
}


float Dictionary::numeric_value(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    float result;
    Guard<RWLock> guard(m_rw_lock);
    if (!find_char(uc)->is_Numeric(uc)) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_NUMERIC_VALUE);
    }
    result=find_char(uc)->numeric_value(uc);
    return result;
}


int Dictionary::dec_digit_value(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    int result;
    Guard<RWLock> guard(m_rw_lock);
    if (!find_char(uc)->is_Decimal_Digit(uc)) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_DEC_DIGIT_VALUE);
    }
    result=find_char(uc)->dec_digit_value(uc);
    return result;
}

    
int Dictionary::digit_value(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    int result;
    Guard<RWLock> guard(m_rw_lock);
    if (!(find_char(uc)->is_Digit(uc))) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_DIGIT_VALUE);
    }
    result=find_char(uc)->digit_value(uc);
    return result;
} 


std::string Dictionary::blockname(const UCS4 uc) 
    throw (Block_Error) {
    std::string result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->blockname(uc);
    return result;
}

    
Gen_Cat Dictionary::category(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    Gen_Cat result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->category(uc);
    if (result == CAT_MAX) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_CHARACTER);
    }
    return result;
}


Can_Comb_Class Dictionary::comb_class(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    Can_Comb_Class result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->comb_class(uc);
    if (result == CC_MAX) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_CHARACTER);
    }
    return result;
}

    
Bidir_Props Dictionary::bidir_props(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    Bidir_Props result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->bidir_props(uc);
    if (result == BIDIR_MAX) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_CHARACTER);
    }
    return result;
}

    
Char_Decomp Dictionary::decomp_type(const UCS4 uc) 
    throw (Undefined_Property, Block_Error) {
    Char_Decomp result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->decomp_type(uc);
    if (result == DECOMP_MAX) {
	m_rw_lock.unlock();
	throw Undefined_Property(uc, PROP_CHARACTER);
    }
    return result;
}


UTF32_string Dictionary::decompose(const UCS4 uc) 
    throw (Block_Error) {
    UTF32_string result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->decompose(uc);
    return result;
}


UCS4 Dictionary::compose(const UCS4 starter, const UCS4 last) 
    throw (Block_Error) {
    UCS4 result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(starter)->compose(starter, last);
    return result;
}

    
bool Dictionary::must_mirror(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->must_mirror(uc);
    return result;
}


EA_Width Dictionary::EA_width(const UCS4 uc) 
    throw (Block_Error) {
    EA_Width result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->EA_width(uc);
    return result;
}


Line_Break Dictionary::linebreak(const UCS4 uc) 
    throw (Block_Error) {
    Line_Break result;
    Guard<RWLock> guard(m_rw_lock);
    result=find_char(uc)->linebreak(uc);
    return result;
}

// Properties:

bool Dictionary::is_White_space(const UCS4 uc) 
    throw (Block_Error)  {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_White_space(uc);
    return result;
}


bool Dictionary::is_Bidi_Control(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Bidi_Control(uc);
    return result;
}


bool Dictionary::is_Join_Control(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Join_Control(uc);
    return result;
}


bool Dictionary::is_Dash(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Dash(uc);
    return result;
}


bool Dictionary::is_Hyphen(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Hyphen(uc);
    return result;
}


bool Dictionary::is_Quotation_Mark(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Quotation_Mark(uc);
    return result;
}


bool Dictionary::is_Terminal_Punctuation(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Terminal_Punctuation(uc);
    return result;
}


bool Dictionary::is_Other_Math(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Other_Math(uc);
    return result;
}


bool Dictionary::is_Hex_Digit(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Hex_Digit(uc);
    return result;
}


bool Dictionary::is_Other_Alphabetic(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Other_Alphabetic(uc);
    return result;
}


bool Dictionary::is_Ideographic(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Ideographic(uc);
    return result;
}


bool Dictionary::is_Diacritic(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Diacritic(uc);
    return result;
}


bool Dictionary::is_Extender(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Extender(uc);
    return result;
}


bool Dictionary::is_Other_Uppercase(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Other_Uppercase(uc);
    return result;
}


bool Dictionary::is_Other_Lowercase(const UCS4 uc) 
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Other_Lowercase(uc);
    return result;
}


bool Dictionary::is_Noncharacter_Code_Point(const UCS4 uc)
    throw (Block_Error) {
    bool result;
    Guard<RWLock> guard(m_rw_lock);
    result = find_char(uc)->is_Noncharacter_Code_Point(uc);
    return result;
}    

UCS4 Dictionary::first_letter_of_block(const UCS4 uc)
    throw () {
    UCS4 result = UC_MAX_DEFINED;
    if (uc >= UC_MAX_DEFINED)
	return result;

    std::vector<Data>::const_iterator b;
    Data tmp(uc, uc);
    Guard<RWLock> guard(m_rw_lock);
    b = lower_bound(m_data.begin(), m_data.end(), tmp, DataLess());
    if (b->m_start <= uc && b->m_end >= uc)
	result = b->m_start;
    return result;
}

UCS4 Dictionary::last_letter_of_block(const UCS4 uc)
    throw () {
    UCS4 result = UC_MAX_DEFINED;
    if (uc >= UC_MAX_DEFINED)
	return result;

    std::vector<Data>::const_iterator b;
    Data tmp(uc, uc);
    Guard<RWLock> guard(m_rw_lock);
    b = lower_bound(m_data.begin(), m_data.end(), tmp, DataLess());
    if (b->m_start <= uc && b->m_end >= uc)
	result = b->m_end;
    return result;
}

UCS4 Dictionary::start_of_next_block(const UCS4 uc) 
    throw () {
    UCS4 result = UC_MAX_DEFINED;
    if (uc >= UC_MAX_DEFINED)
	return result;

    std::vector<Data>::const_iterator b;
    Data tmp(uc, uc);
    Guard<RWLock> guard(m_rw_lock);
    b = lower_bound(m_data.begin(), m_data.end(), tmp, DataLess()); 
    if (b != m_data.end()) {
	if (b->m_end >= uc)
	    ++b;
	if (b != m_data.end())
	    result = b->m_start;
    }
    return result;
}

Dictionary * Dictionary::instance() {
    // Create Dictionary just once
    {
	Prague::Guard<Mutex> guard(m_singleton_mutex);
	if (!m_dictionary) m_dictionary = new Dictionary;
    }
    return m_dictionary;
} // instance


UTF32_string Dictionary::recursive_decompose(const bool compat, const UCS4 uc)
    throw (Block_Error) {
    UTF32_string decomp;
    UTF32_string result;
    
    if (decomp_type(uc) == DECOMP_COMPAT && !compat) {
	decomp.resize(1); decomp[0] = uc;
	return decomp;
    }
   
    {
	Guard<RWLock> guard(m_rw_lock);
	decomp = decompose(uc);
    }
    if(decomp[0] != uc && !(compat && decomp_type(uc) == DECOMP_COMPAT))
	for (UTF32_string::const_iterator i = decomp.begin();
	     i != decomp.end();
	     i++)
	    result += recursive_decompose(compat, *i);
    else
	result = decomp;
    return result;
}


Dictionary::Block * Dictionary::find_char(const UCS4 uc)
    throw (Block_Error) {
    // Gets only called after the dictionary is rlocked!

    // Binary search version:
    std::vector<Data>::iterator p = m_data.begin();
    
    // ASCII happens so often that this speeds things up a bit
    if (uc > p->m_end) {
	// binary search for non-ASCII characters
	++p; // advance past ASCII
	Data tmp(uc, uc);
	p = lower_bound(p, m_data.end(), tmp, DataLess());
    }
    
    if (p == m_data.end() || p->m_start > uc) { // uc belongs to no block
	return m_undef_block->get();
    }
    
    if (p->m_block == 0) { // need to dynamically load the relevant script
	p->m_block = new Prague::Plugin<Dictionary::Block>(p->m_file);
	if(p->m_block == 0 || *(p->m_block) == 0) {
	    // Failed to load the plugin for this block
	    throw Block_Error(p->m_start, p->m_end, p->m_block->error());
	}
    }

    return p->m_block->get();
} // Dictionary::find_char


void Dictionary::update(const std::string & scanDir) {
    Guard<RWLock, WLock_Trait<RWLock> > guard(m_rw_lock);
    clean();
    
    Prague::Directory dir(scanDir, 0);
    
    // start scanning the directory:
    for (Prague::Directory::const_iterator dir_it = dir.begin();
	 dir_it != dir.end();
	 ++dir_it) {
	std::string name = (*dir_it)->long_name();
	
	if ( !((*dir_it)->is(Prague::Directory::reg)) )
	    continue;
	
	Prague::Plugin<Dictionary::Block> * block =
	    new Prague::Plugin<Dictionary::Block>(name);
	
	// new throws en exception when running out of memory,
	// so this is not necesarry:
	// if (block == 0)
	//    continue;
	
	if (*block == 0)
	    continue;
	
	UCS4 start = (*block)->first_letter();
	UCS4 end   = (*block)->last_letter();

	if ((*block)->is_undef_block()) {
	    if(m_undef_block == 0) {
		m_undef_block = block;
		// don't delete the block!
	    } else
		delete block;
	} else {
	    Data current(start, end);
	    current.m_file = name;
	    current.m_can_remove = 0;
	    current.m_block = 0;

	    m_data.push_back(current);
	    delete block;
	}
    }      

    std::vector<Data> (m_data).swap(m_data); // shrink vector
    sort(m_data.begin(), m_data.end(), DataLess()); // sort it

    // Sanity tests:
    if (m_undef_block == 0) {
	throw Block_Error(0, 0xFFFFFFFF, "No undef-block defined.");
    }

    if (m_data.size() == 0) {
	throw Block_Error(0, 0xFFFFFFFF, "No block defined.");
    }
} // update_dictionary


Dictionary::Dictionary() {
    {
	Guard<RWLock, WLock_Trait<RWLock> > guard(m_rw_lock);
	m_version.resize(1);
	m_version[0] = UC_NULL;
	m_undef_block = 0;
    }
    char *env = getenv("BABYLON_PATH");
    if (!env) {
	std::cerr << "Please set environment variable BABYLON_PATH first" << std::endl;
	exit(-1);
    }
    
    update(std::string(env));
} // Dictionary::Dictionary


Dictionary::~Dictionary() {
    Guard<RWLock, WLock_Trait<RWLock> > guard(m_rw_lock);
    clean();
} // Dictionary::~Dictionary()


void Dictionary::clean() {
    // gets only called in writelocked functions!
    for (std::vector<Data>::const_iterator i = m_data.begin();
	 i != m_data.end();
	 ++i)
	if (i->m_block)
	    delete i->m_block;
    
    m_data.clear();
    delete m_undef_block;
};


Dictionary::Block::Block() {}


Dictionary::Block::~Block() {}


void Dictionary::Block::clean() {}
