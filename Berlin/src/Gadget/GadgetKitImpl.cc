/*$Id: GadgetKitImpl.cc,v 1.10 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include "Gadget/GadgetKitImpl.hh"
#include <Warsaw/View.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/BoundedValue.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/PickTraversal.hh>
#include <Warsaw/resolve.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/ViewImpl.hh>
#include <Berlin/MonoGraphic.hh>
#include <Berlin/RefCountVar.hh>
#include <cmath>

using namespace Warsaw;

// class AlphaAdjuster : public virtual POA_View, public MonoGraphic
// {
//  public:
//   virtual void traverse(Traversal_ptr traversal) { traversal->visit(Graphic_var(_this()));}
//   virtual void draw(DrawTraversal_ptr traversal)
//   {
//     DrawingKit_var kit = traversal->kit();
//     kit->saveState();
//     Color color = kit->foreground();
//     color.alpha *= alpha;
//     kit->foreground(color);
//     MonoGraphic::traverse(traversal);
//     kit->restoreState();    
//   }
//   virtual void pick(PickTraversal_ptr traversal) { MonoGraphic::traverse(traversal);}

//   virtual void update(const CORBA::Any &any) { any >>= alpha; needRedraw();}
//  private:
//   Coord alpha;
// };

class RGBAdjuster : public virtual ViewImpl,
		    public MonoGraphic
{
 public:
  RGBAdjuster(BoundedValue_ptr r, BoundedValue_ptr g, BoundedValue_ptr b)
    : red(RefCount_var<BoundedValue>::increment(r)),
      green(RefCount_var<BoundedValue>::increment(g)),
      blue(RefCount_var<BoundedValue>::increment(b))
    {
      color.red = red->value();
      color.green = green->value();
      color.blue = blue->value();
    } 
  virtual void traverse(Traversal_ptr traversal) { traversal->visit(Graphic_var(_this()));}
  virtual void draw(DrawTraversal_ptr traversal)
  {
    DrawingKit_var drawing = traversal->drawing();
    drawing->save();
    Color tmp = drawing->foreground();
    color.alpha = tmp.alpha;
    drawing->foreground(color);
    MonoGraphic::traverse(traversal);
    drawing->restore();    
  }
  virtual void pick(PickTraversal_ptr traversal) { MonoGraphic::traverse(traversal);}

  virtual void update(const CORBA::Any &)
    {
      color.red = red->value();
      color.green = green->value();
      color.blue = blue->value();
      need_redraw();
    }
 private:
  RefCount_var<BoundedValue> red, green, blue;
  Color color;
};

class AlphaAdjuster : public virtual ViewImpl,
		      public MonoGraphic
{
 public:
  AlphaAdjuster(BoundedValue_ptr v) : alpha(v->value()) {}
  virtual void traverse(Traversal_ptr traversal) { traversal->visit(Graphic_var(_this()));}
  virtual void draw(DrawTraversal_ptr traversal)
  {
    DrawingKit_var drawing = traversal->drawing();
    drawing->save();
    Color color = drawing->foreground();
    color.alpha *= alpha;
    drawing->foreground(color);
    MonoGraphic::traverse(traversal);
    drawing->restore();    
  }
  virtual void pick(PickTraversal_ptr traversal) { MonoGraphic::traverse(traversal);}

  virtual void update(const CORBA::Any &any) { any >>= alpha; need_redraw();}
 private:
  Coord alpha;
};

class LightingAdjuster : public virtual ViewImpl,
			 public MonoGraphic
{
 public:
  LightingAdjuster(BoundedValue_ptr r, BoundedValue_ptr g, BoundedValue_ptr b)
    : red(RefCount_var<BoundedValue>::increment(r)),
      green(RefCount_var<BoundedValue>::increment(g)),
      blue(RefCount_var<BoundedValue>::increment(b))
    {
      color.red = red->value();
      color.green = green->value();
      color.blue = blue->value();
    } 
  virtual void traverse(Traversal_ptr traversal) { traversal->visit(Graphic_var(_this()));}
  virtual void draw(DrawTraversal_ptr traversal)
  {
    DrawingKit_var drawing = traversal->drawing();
    drawing->save();
    Color tmp = drawing->lighting();
    /*
     * how is the light attribute concatenated, subtractive, additive ? -stefan
     */
    tmp.red *= color.red;
    tmp.green *= color.green;
    tmp.blue *= color.blue;
    drawing->lighting(tmp);
    MonoGraphic::traverse(traversal);
    drawing->restore();    
  }
  virtual void pick(PickTraversal_ptr traversal) { MonoGraphic::traverse(traversal);}

  virtual void update(const CORBA::Any &)
    {
      color.red = red->value();
      color.green = green->value();
      color.blue = blue->value();
      need_redraw();
    }
 private:
  RefCount_var<BoundedValue> red, green, blue;
  Color color;
};

class RotationAdjuster : public virtual ViewImpl,
			 public MonoGraphic
{
 public:
  RotationAdjuster(Axis a) : axis(a) {}
  virtual void update(const CORBA::Any &any)
    {
      Graphic_var child = body(); if (CORBA::is_nil(child)) return;
      Transform_var transformation = child->transformation(); if (CORBA::is_nil(transformation)) return;
      Coord phi;
      any >>= phi;
      transformation->load_identity();
      transformation->rotate(phi, axis);
      need_resize();
    }
 private:
  Axis axis;
};

class ZoomAdjuster : public virtual ViewImpl,
		     public MonoGraphic
{
 public:
  virtual void update(const CORBA::Any &any)
    {
      Graphic_var child = body(); if (CORBA::is_nil(child)) return;
      Transform_var transformation = child->transformation(); if (CORBA::is_nil(transformation)) return;
      Coord scale;
      any >>= scale;
      Vertex s;
      s.x = s.y = s.z = exp(scale);
      transformation->load_identity();
      transformation->scale(s);
      need_resize();
    }
};

GadgetKitImpl::GadgetKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
GadgetKitImpl::~GadgetKitImpl() {}
void GadgetKitImpl::bind(ServerContext_ptr context)
{
  KitImpl::bind(context);
  Warsaw::Kit::PropertySeq props;
  props.length(0);
  command = resolve_kit<CommandKit>(context, "IDL:Warsaw/CommandKit:1.0", props);
  figure = resolve_kit<FigureKit>(context, "IDL:Warsaw/FigureKit:1.0", props);
}

Graphic_ptr GadgetKitImpl::rgb(Graphic_ptr body, BoundedValue_ptr r, BoundedValue_ptr g, BoundedValue_ptr b)
{
  RGBAdjuster *adjuster = new RGBAdjuster(r, g, b);
  activate(adjuster);
  r->attach(Observer_var(adjuster->_this()));
  g->attach(Observer_var(adjuster->_this()));
  b->attach(Observer_var(adjuster->_this()));
  adjuster->body(body);
  return adjuster->_this();
}

Graphic_ptr GadgetKitImpl::alpha(Graphic_ptr g, BoundedValue_ptr value)
{
  AlphaAdjuster *adjuster = new AlphaAdjuster(value);
  activate(adjuster);
  value->attach(Observer_var(adjuster->_this()));
  adjuster->body(g);
  return adjuster->_this();
}

Graphic_ptr GadgetKitImpl::lighting(Graphic_ptr body, BoundedValue_ptr r, BoundedValue_ptr g, BoundedValue_ptr b)
{
  LightingAdjuster *adjuster = new LightingAdjuster(r, g, b);
  activate(adjuster);
  r->attach(Observer_var(adjuster->_this()));
  g->attach(Observer_var(adjuster->_this()));
  b->attach(Observer_var(adjuster->_this()));
  adjuster->body(body);
  return adjuster->_this();
}

Graphic_ptr GadgetKitImpl::rotator(Graphic_ptr g, BoundedValue_ptr value, Axis axis)
{
  RotationAdjuster *adjuster = new RotationAdjuster(axis);
  activate(adjuster);
  value->attach(Observer_var(adjuster->_this()));
  Graphic_var transformer = figure->transformer(g);
  adjuster->body(transformer);
  return adjuster->_this();
}

Graphic_ptr GadgetKitImpl::zoomer(Graphic_ptr g, BoundedValue_ptr value)
{
  ZoomAdjuster *adjuster = new ZoomAdjuster();
  activate(adjuster);
  value->attach(Observer_var(adjuster->_this()));
  Graphic_var transformer = figure->transformer(g);
  adjuster->body(transformer);
  return adjuster->_this();
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "GadgetKitImpl"};
  return new KitFactoryImpl<GadgetKitImpl>("IDL:Warsaw/GadgetKit:1.0", properties, 1);
} 
