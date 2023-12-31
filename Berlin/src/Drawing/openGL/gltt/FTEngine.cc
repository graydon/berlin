/*
 * gltt graphics library
 * Copyright (C) 1998-1999 Stephane Rehel
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
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "Drawing/openGL/gltt/FTEngine.h"

#include <freetype.h>

/////////////////////////////////////////////////////////////////////////////

// static
FTEngine* FTEngine::static_engine= 0;

/////////////////////////////////////////////////////////////////////////////

FTEngine::FTEngine()
{
  engine= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTEngine::~FTEngine()
{
  if( engine != 0 )
    {
    TT_Done_FreeType(*engine);

    delete engine;
    engine= 0;
    }
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTEngine::init()
{
  if( engine != 0 )
    return GLTT_TRUE;

  engine= new TT_Engine;

  if( TT_Init_FreeType( engine ) )
    {
    delete engine;
    engine= 0;
    return GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

// static
FTEngine* FTEngine::getStaticEngine()
{
  if( static_engine == 0 )
    {
    static_engine= new FTEngine;
    if( ! static_engine->init() )
      {
      delete static_engine;
      static_engine= 0;
      }
    }

  return static_engine;
}

/////////////////////////////////////////////////////////////////////////////
