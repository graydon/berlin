/*$Id: DrawingKit3DBase.hh,v 1.1 2000/09/08 14:46:18 nickelm Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Niklas Elmqvist <elm@3dwm.org>
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

#ifndef _DrawingKit3DBase_hh
#define _DrawingKit3DBase_hh

#include <Warsaw/config.hh>
#include <Warsaw/DrawingKit3D.hh>
#include <Warsaw/Raster.hh>

#include <stack>

class DrawingKit3DBase : public virtual POA_Warsaw::DrawingKit3D {

 private:

    // Which parts of the state was stored?
    enum g3dstate {
	st3d_material,
	st3d_textures,
	st3d_tex_mode,
	st3d_fog_mode,
	st3d_last 
    };
    
    // 3D drawing state
    struct Draw3DState {
	unsigned long flags;
	MaterialAttr_var saved_material;
	Rasters_var saved_textures;
	TextureMode saved_texmode;
	FoggingMode saved_fogmode;
	// need lights and lots of other stuff here as well
    };
    
 public:
    
    // Save and restore the current drawing state. Some drawing
    // targets may be able to do this themselves, so we keep these
    // functions virtual.
    virtual void saveState() { DrawState3D st; states.push(st); }
    virtual void restoreState(); 
    
    virtual MaterialAttr material() = 0;
    virtual void material(const MaterialAttr&) = 0;
    virtual Rasters* textures() = 0;
    virtual void textures(const Rasters&) = 0;
    virtual TextureMode texMode() = 0;
    virtual void texMode(TextureMode) = 0;
    virtual FoggingMode fogMode() = 0;
    virtual void fogMode(FoggingMode) = 0;
    virtual void drawTriangles(const Vertices &, const Vertices &,
			       const TexCoords &) = 0;
    virtual void drawLines(const Vertices &, const TexCoords &) = 0;
    virtual void drawPoints(const Vertices &) = 0;

    virtual void flush() = 0;

 protected:

    // -- Helper functions (should be able to keep these protected)
    virtual void setMaterial(const MaterialAttr&) = 0;
    virtual void setTextures(const Rasters&) = 0;
    virtual void setTexMode(TextureMode) = 0;
    virtual void setFogMode(FoggingMode) = 0;
	
 private:

    // Drawing state stack
    stack<DrawState3D> states;
};

//  -- Inline class methods

inline void DrawingKit3DBase::restoreState()
{
    // Sanity check
    if (states.empty()) return;

    enum g3dstate {
	st3d_material,
	st3d_textures,
	st3d_texmode,
	st3d_fogmode,
	st3d_last 
    };

    // We need to restore _only_ those states that were changed 
    DrawState3D prev = states.top();
    if (prev.flags & (1 << st3d_material)) 
}

// This code is unashamedly lifted from the DrawingKitBase.hh...
#define REMEMBER(state,ty,val) \
 if (!(states.empty() || states.top().flags & (1 << st_## state)))\
{ \
  DrawState &st = states.top(); \
  ty tmp(val); \
  st.saved_## state = tmp; \
  st.flags |= (1 << st_## state); \
}

inline DrawingKit3DBase::material(const MaterialAttr &m)
{
    REMEMBER(material, MaterialAttr_var, material());
    setMaterial(m);
}

inline DrawingKit3DBase::textures(const Rasters &rs)
{
    REMEMBER(textures, Rasters_var, textures());
    setTextures(rs);
}

inline DrawingKit3DBase::texMode(TextureMode tm) 
{
    REMEMBER(tex_mode, TextureMode, texMode());
    setTexMode(tm);
}

inline DrawingKit3DBase::fogMode(FoggingMode fm)
{
    REMEMBER(fog_mode, FoggingMode, fogMode());
    setFogMode(fm);
}

#endif /* DrawingKit3DBase.hh */
