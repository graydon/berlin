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

#ifndef __FTInstance_h
#define __FTInstance_h

#ifndef __GLTTboolean_h
#include "Drawing/openGL/gltt/GLTTboolean.h"
#endif

struct TT_Instance_;
typedef struct TT_Instance_ TT_Instance;

class FTFace;

/////////////////////////////////////////////////////////////////////////////

class FTInstance
{
protected:
  FTFace* face;
  TT_Instance* instance;

public:
  FTInstance( FTFace* _face );

  virtual ~FTInstance();

  GLTTboolean create();

  FTFace* getFace() const
    {
    return face;
    }

  TT_Instance* getInstance() const
    {
    return instance;
    }

  GLTTboolean setResolutions( int x_resolution, int y_resolution );

  GLTTboolean setPointSize( int point_size );

  int getHeight() const;
  int getDescender() const;
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTInstance_h
