/*$Id: Dictionary.hh,v 1.8 2001/03/31 09:46:01 tobias Exp $
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

#ifndef _Dictionary_hh_
#define _Dictionary_hh_

#include <Babylon/defs.hh>
#include <Prague/Sys/Plugin.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/Directory.hh>
#include <Prague/Sys/DLL.hh>
#include <Prague/Filter/gzstream.hh>

namespace Babylon {

    //. Stores character data.
    class Dictionary {

    struct Dict_Guard {
	~Dict_Guard() { delete Dictionary::m_dictionary; }
    };
    friend struct Dict_Guard;
    
    public:
    	class Block {
	    // This class is subclassed by plugin libraries.
	    // No method bodies here!!

	    // These Blocks are build for speed, not safety!
	    // The Dictionary has to make sure, that it is
	    // asking the right block about defined properties.

	    // It can do so by calling the appropriate is_*

	public:
	    Block();
	    virtual ~Block();
	    virtual void clean();
	    virtual bool is_undef_block() const = 0; // returns 1 if this
	                                             // block is meant to handle
	                                             // all undefined characters
	                                             // 
	                                             // The first undef_block
	                                             // found will be used!
	
	    // Query Functions:
	    virtual bool is_defined(const UCS4)  const = 0;
	    virtual UCS4 uppercase(const UCS4) const = 0;
	    virtual UCS4 lowercase(const UCS4) const = 0;
	    virtual UCS4 titlecase(const UCS4) const = 0;

	    virtual float numeric_value(const UCS4) const = 0;
	    virtual bool is_Numeric(const UCS4) const = 0;
	    virtual int dec_digit_value(const UCS4) const = 0;
	    virtual bool is_Decimal_Digit(const UCS4) const = 0;
	    virtual int digit_value(const UCS4) const = 0;
	    virtual bool is_Digit(const UCS4) const = 0;

	    virtual std::string blockname(const UCS4) const = 0;
	    virtual Gen_Cat category(const UCS4) const = 0;
	    virtual Can_Comb_Class comb_class(const UCS4) const = 0;
	    virtual Bidir_Props bidir_props(const UCS4) const = 0;
	    virtual Char_Decomp decomp_type(const UCS4) const = 0;
	    virtual UTF32_string decompose(const UCS4) const = 0;
	    virtual UCS4 compose(const UCS4, const UCS4) = 0;
	    virtual bool must_mirror(const UCS4) const = 0;
	    virtual EA_Width EA_width(const UCS4) const = 0;
	    virtual Line_Break linebreak(const UCS4) const = 0;

	    // Properties:
	    virtual bool is_White_space(const UCS4) const = 0;
	    virtual bool is_Bidi_Control(const UCS4) const = 0;
	    virtual bool is_Join_Control(const UCS4) const = 0;
	    virtual bool is_Dash(const UCS4) const = 0;
	    virtual bool is_Hyphen(const UCS4) const = 0;
	    virtual bool is_Quotation_Mark(const UCS4) const = 0;
	    virtual bool is_Terminal_Punctuation(const UCS4) const = 0;
	    virtual bool is_Other_Math(const UCS4) const = 0;
	    virtual bool is_Hex_Digit(const UCS4) const = 0;
	    virtual bool is_Other_Alphabetic(const UCS4) const = 0;
	    virtual bool is_Ideographic(const UCS4) const = 0;
	    virtual bool is_Diacritic(const UCS4) const = 0;
	    virtual bool is_Extender(const UCS4) const = 0;
	    virtual bool is_Other_Uppercase(const UCS4) const = 0;
	    virtual bool is_Other_Lowercase(const UCS4) const = 0;
	    virtual bool is_Noncharacter_Code_Point(const UCS4) const = 0;

	    virtual UCS4 first_letter() const = 0;
	    virtual UCS4 last_letter() const = 0;
	protected:
	
	private:
	}; // class Block
    
	//. Scans a directory for modules.
	void update(const std::string &);

	//. Finds the current dictionary.
	//. If no dictionary exists it will create one.
	static Dictionary * instance();

	// Queries for the datastructures stored in the Dictionary:

	//. Returns the first letter of the block (aka script)
	//. the given character belongs to. It returns
	//. UC_MAX_DEFINED if the character does not belong
	//. to a block.
	UCS4 first_letter_of_block(const UCS4) throw ();

	//. Returns the last letter of the block (aka script)
	//. the given character belongs to. It returns
	//. UC_MAX_DEFINED if the character does not belong
	//. to a block
	UCS4 last_letter_of_block(const UCS4) throw ();

	//. Returns the first letter of the next block (aka script)
	//. defined block after the given character.
	//. It returns UC_MAX_DEFINED if there is no block after
	//. the character.
	UCS4 start_of_next_block(const UCS4) throw();

    private:
	// Query functions:
	bool is_defined(const UCS4 uc)
	    throw (Block_Error);    
	UCS4 uppercase(const UCS4 uc)
	    throw (Block_Error);
	UCS4 lowercase(const UCS4 uc)
	    throw (Block_Error);
	UCS4 titlecase(const UCS4 uc)
	    throw (Block_Error);
    
	float numeric_value(const UCS4 uc)
	    throw (Undefined_Property, Block_Error);
	bool is_Numeric(const UCS4 uc);
	int dec_digit_value(const UCS4 uc)
	    throw (Undefined_Property, Block_Error);
	bool is_Decimal_Digit(const UCS4 uc);
	int digit_value(const UCS4 uc)
	    throw (Undefined_Property, Block_Error);
	bool is_Digit(const UCS4 uc);

	std::string blockname(const UCS4 uc)
	    throw (Block_Error);
    
	Gen_Cat category(const UCS4 uc)
	    throw (Undefined_Property, Block_Error);
	Can_Comb_Class comb_class(const UCS4 uc)
	    throw (Undefined_Property, Block_Error);
	Bidir_Props bidir_props(const UCS4 uc)
	    throw (Undefined_Property, Block_Error);
	Char_Decomp decomp_type(const UCS4 uc) 
	    throw (Undefined_Property, Block_Error);
	UTF32_string decompose(const UCS4 uc)
	    throw (Block_Error);
    
	UTF32_string recursive_decompose(const bool compat, const UCS4 uc)
	    throw (Block_Error);
    
	UCS4 compose(const UCS4 starter, const UCS4 last)
	    throw (Block_Error);
    
	bool must_mirror(const UCS4 uc)
	    throw (Block_Error);
    
	EA_Width EA_width(const UCS4 uc)
	    throw (Block_Error);
	Line_Break linebreak(const UCS4 uc)
	    throw (Block_Error);

	// Properties:
	bool is_White_space(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Bidi_Control(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Join_Control(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Dash(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Hyphen(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Quotation_Mark(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Terminal_Punctuation(const UCS4 uc) 
	    throw (Block_Error);
	bool is_Other_Math(const UCS4 uc) 
	    throw (Block_Error);
        bool is_Hex_Digit(const UCS4 uc) 
            throw (Block_Error);
        bool is_Other_Alphabetic(const UCS4 uc) 
            throw (Block_Error);
        bool is_Ideographic(const UCS4 uc) 
            throw (Block_Error);
        bool is_Diacritic(const UCS4 uc) 
            throw (Block_Error);
        bool is_Extender(const UCS4 uc) 
            throw (Block_Error);
        bool is_Other_Uppercase(const UCS4 uc) 
            throw (Block_Error);
        bool is_Other_Lowercase(const UCS4 uc) 
            throw (Block_Error);
        bool is_Noncharacter_Code_Point(const UCS4)
	    throw (Block_Error);

	struct Data {
	    UCS4 m_start;
	    UCS4 m_end;
	    std::string m_file;
	    int operator < (const Data & data) const {return m_start < data.m_start;}
	    bool m_can_remove;
	    Prague::Plugin<Dictionary::Block> * m_block;

	    Data(UCS4 start, UCS4 end) {
		m_start = start;
		m_end = end;
		m_file = "";
		m_can_remove = 0;
		m_block = 0;
	    }
	}; // struct Data

	class DataLess {
	public:
	    bool operator() (const Data & d1, const Data & d2) {
		return d1.m_end < d2.m_start;
	    }
	}; // class DataLess

	Prague::Plugin<Dictionary::Block> * m_undef_block;
    
	Block * find_char(const UCS4)
	    throw (Block_Error);
    
	Dictionary();
	Dictionary(const Dictionary &) {}
	~Dictionary();
	void clean();
    
	static Dictionary * m_dictionary;
	static Dict_Guard  m_guard;
	static Prague::Mutex m_singleton_mutex;
    
	std::vector<Data> m_data;
    
	UTF32_string m_version;
    
	Prague::RWLock m_rw_lock;
    
	// friends:
	friend class Babylon::Char;
	friend class Babylon::String;
    }; // class Dictionary
    
} // namespace Babylon

#endif // _Dictionary_hh_
