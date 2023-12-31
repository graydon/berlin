#include "Drawing/openGL/GLFont.hh"
#include "Drawing/openGL/gltt/GLTTPixmapFont.h"
#include "Drawing/openGL/gltt/FTFace.h"
#include "Berlin/Logger.hh"
#include <iostream>

// this is an awful hack to make gltt start working. the correct
// thing to do is fix gltt. there's no freakin' point in having unicode
// in your system if the font rasterizer chops off the high bits :(

static string ASCIIFY(const Unistring &u) {
  string tmp;
  for (unsigned long i = 0; i < u.length(); i++) {
    tmp += (char)(u[i]);
  }
  return tmp;
}


GLFont::GLFont(const Text::FontDescriptor &fd, const Style::Spec &sty) 
  throw (Text::NoSuchFontException) :
  myDescriptor(fd) {
  
  string txt = ASCIIFY(fd.name);
  face = new FTFace();
  if( ! face->open(txt.c_str()) ) throw Text::NoSuchFontException();
  font = new GLTTPixmapFont(face); 
  if( ! font->create(fd.pointsize) ) throw Text::NoSuchFontException();
  Logger::log(Logger::text) << "GLFont::GLFont() : instantiated " << txt.c_str() << " at " << fd.pointsize << " pt" << endl;
  for (unsigned long i = 0; i < sty.length(); i++) {    
    Color *tmp;
    if (sty[i].a == Style::fillcolor) {
      sty[i].val >>= tmp;
      myFontColor[0] = tmp->red;
      myFontColor[1] = tmp->green;
      myFontColor[2] = tmp->blue;
      myFontColor[3] = tmp->alpha;
    }
  }    
}

GLFont::~GLFont() {}

void GLFont::drawText(const Unistring &u, const Vertex &p) {
  // ... OpenGL initialization commands... 

  glColor4d(myFontColor[0],myFontColor[1],myFontColor[2],myFontColor[3]);
  // this goes into the GLDrawable constructor -stefan
  //   glEnable(GL_BLEND);
  //   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  string txt = ASCIIFY(u);
  font->output( p.x, p.y, txt.c_str()); 
}


void GLFont::acceptFontVisitor(Text::FontVisitor_ptr v){
  v->visitBaseFont(this->_this());
  CORBA::release(v);
}

void GLFont::allocateText(const Unistring &u, Graphic::Requisition &r){
    string txt = ASCIIFY(u);
    r.x.natural = font->getWidth(txt.c_str());
    r.x.defined = true;
    r.y.natural = font->getHeight();
    r.y.natural = true;
}

CORBA::Boolean  GLFont::canDrawText(const Unistring &u){
  // do more elaborate checking here!
  return true;
}

void GLFont::getDescriptor(Text::FontDescriptor &desc){
  desc = myDescriptor;
}

FeatureValueList *GLFont::queryFeature(FeatureType ft) { return new FeatureValueList(); }
void GLFont::setFeature(FeatureType ft, FeatureValue fv) {}
