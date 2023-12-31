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

#include "Drawing/openGL/gltt/FTInstance.h"
#include "Drawing/openGL/gltt/FTFace.h"
#include "Drawing/openGL/gltt/FTEngine.h"

#include <freetype.h>

/////////////////////////////////////////////////////////////////////////////

FTInstance::FTInstance( FTFace* _face )
{
  face= _face;

  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

FTInstance::~FTInstance()
{
  if( instance != 0 )
    {
    TT_Done_Instance(*instance);

    delete instance;
    instance= 0;
    }

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTInstance::create()
{
  if( instance != 0 )
    return GLTT_TRUE;

  if( face == 0 )
    return GLTT_FALSE;
  if( face->getFace() == 0 )
    return GLTT_FALSE;

  instance= new TT_Instance;

  if( TT_New_Instance( *face->getFace(), instance ) )
    {
    delete instance;
    instance= 0;
    return GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTInstance::setResolutions( int x_resolution, int y_resolution )
{
  if( instance == 0 )
    return GLTT_FALSE;

  TT_Error err= TT_Set_Instance_Resolutions( *instance,
                                             x_resolution, y_resolution );

  return err == 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTInstance::setPointSize( int point_size )
{
  if( instance == 0 )
    return GLTT_FALSE;

  TT_Error err= TT_Set_Instance_PointSize( *instance, point_size );

  return err == 0;
}

/////////////////////////////////////////////////////////////////////////////

int FTInstance::getHeight() const
{
  if( instance == 0 )
    return 0;

  TT_Instance_Metrics imetrics;
  TT_Error err= TT_Get_Instance_Metrics( *instance, &imetrics );
  if( err )
    return 0;

  return (imetrics.pointSize * imetrics.y_resolution) / (72*64);
}

/////////////////////////////////////////////////////////////////////////////

// Contributed by Karl Anders Oygard <Karl.Oygard@fou.telenor.no>
int FTInstance::getDescender() const
{
  if( instance == 0 || face == 0 )
    return 0;

  TT_Instance_Metrics imetrics;
  TT_Face_Properties properties;
  TT_Error err;

  err= TT_Get_Face_Properties( *face->getFace(), &properties );
  if( err )
    return 0;

  err= TT_Get_Instance_Metrics( *instance, &imetrics );
  if( err )
    return 0;

  return (properties.horizontal->Descender * imetrics.y_ppem) /
         properties.header->Units_Per_EM;
}

/////////////////////////////////////////////////////////////////////////////
