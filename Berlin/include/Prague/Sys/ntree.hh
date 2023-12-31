/*$Id: ntree.hh,v 1.1 1999/09/08 19:04:05 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca>
 * http://www.berlin-consortium.org
 *
 * this code was originally written by
 * Author: Kevin S. Van Horn (kevin.s.vanhorn@iname.com)
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
#ifndef _ntree_hh
#define _ntree_hh

#include <list>
#include <assert.h>

template<class T> class _ntree_node;
template<class T> class _ntree_node_child_iterator;
template<class T> class _const_ntree_node_child_iterator;
template<class T> class _ntree_node_up_iterator;
template<class T> class _const_ntree_node_up_iterator;

//General notes:  No storage is shared between ntrees.  If part of one
//ntree t1 is assigned to or inserted into another ntree t2, that part is either
//copied (constructors, assignment, insert) or removed from ntree t1
//(assign_cut, insert_cut).

template <class T>
class ntree
{
public:
  typedef ntree<T> self;
  typedef _ntree_node<T> node;
  typedef _ntree_node_child_iterator<T> child_iterator;
  typedef _const_ntree_node_child_iterator<T> const_child_iterator;
  typedef _ntree_node_up_iterator<T> up_iterator;
  typedef _const_ntree_node_up_iterator<T> const_up_iterator;

  ntree();                    // empty ntree
  ntree(const ntree &);        // does a deep copy
  ntree(const node &);        // does a deep copy of subntree rooted at node
  ntree(const T &);           // ntree with one node
  ~ntree();

  ntree &operator = (const ntree &);  // does a deep copy
  ntree &operator = (const node &);  // does a deep copy of subntree
  ntree &operator = (const T &);     // ntree with one node

  ntree &assign_cut(ntree &);       
  //Remove t's entire ntree structure (setting t to empty ntree) and
  //assign to this object.
  ntree &assign_cut(node &, child_iterator);
  //Remove subntree of node x pointed at by p, and assign it to this object.
  //REQUIRE: p points to child node of x

  node &root();
  const node &root() const;
  //Return ref to root node of ntree.
  //REQUIRE: ntree not empty.

  bool empty() const;              // Test if ntree is empty
  void swap(ntree &);               // O(1) time swap of values
  void erase();                    // Make ntree empty
private:
  node *root_;
  void destruct(node *);
  friend class _ntree_node<T>;
};

template <class T>
class _ntree_node
{
public:
  typedef _ntree_node_up_iterator<T> up_iterator;
  typedef _const_ntree_node_up_iterator<T> const_up_iterator;
  typedef _ntree_node_child_iterator<T> child_iterator;
  typedef _const_ntree_node_child_iterator<T> const_child_iterator;
  typedef _ntree_node<T> self;
  typedef list<self *>::size_type size_type;

  T value;
  
  up_iterator up_begin();       //Return iterator pointing at this node
  up_iterator up_end();         //Return past-the-end value for ancestors
  child_iterator child_begin(); //Return iterator pointing at first child
  child_iterator child_end();   //Return past-the-end value for children
  
  const_up_iterator up_begin() const;
  const_up_iterator up_end() const;
  const_child_iterator child_begin() const;
  const_child_iterator child_end() const;

  void erase_child(child_iterator);
  //Remove a child.  REQUIRE: p points to a child of this node.
  
  size_type num_children() const;  //Return number of children.
  
  child_iterator insert(child_iterator, const ntree<T> &);
  //Insert a copy of t as a child of this node at position p.
  //Return pointer to new child.
  //REQUIRE: !t.empty().
  //REQUIRE: p points at a child of the node or equals this->child_end().
  
  child_iterator push_back(const ntree<T> &t) { return insert(child_end(), t);}
  child_iterator push_front(const ntree<T> &t) { return insert(child_begin(), t);}
  child_iterator insert(child_iterator, const self &);
  //Insert a copy of the subntree rooted at x as a child of this node at
  //position p.  Return a pointer to new child.
  //REQUIRE: p points at a child of the node or equals this->child_end().
  
  child_iterator push_back(const self &x) { return insert(child_end(), x);}
  child_iterator push_front(const self &x) { return insert(child_begin(), x);}

  child_iterator insert(child_iterator, const T &);
  //Insert a single-node ntree with value x as a child of this node at
  //position p.  Return a pointer to new child.
  //REQUIRE: p points at a child of the node or equals this->child_end().
    
  child_iterator push_back(const T &x) { return insert(child_end(), x);}
  child_iterator push_front(const T &x) { return insert(child_begin(), x);}
  child_iterator insert_cut(child_iterator, ntree<T> &);
  //Make ntree t be empty and insert its old value as a child of this
  //node at position p.  Return pointer to new child.
  //REQUIRE: !t.empty().
  //REQUIRE: p points at a child of the node or equals this->child_end().

  child_iterator push_back_cut(ntree<T> &t) { return insert_cut(child_end(), t);}
  child_iterator push_front_cut(ntree<T> &t) { return insert_cut(child_begin(), t);}
  child_iterator insert_cut(child_iterator, self &, child_iterator);
  //Remove subntree of node x pointed at by q, and insert it as a child
  //of this node at position p.  Returns pointer to new child.
  //REQUIRE: q points at a child of x.
  //REQUIRE: p points at a child of the node or equals this->child_end().
  
  child_iterator push_back_cut(self &x, child_iterator q) { return insert_cut(child_end(), x, q);}
  child_iterator push_front_cut(self &x, child_iterator q) { return insert_cut(child_begin(), x, q);}
private:
  self *parent;
  list<self *> children;

  _ntree_node(const T &, self * = 0);
  _ntree_node(const self &);                   // unimplemented
  _ntree_node &operator = (const self &);      // unimplemented
  ~_ntree_node() {};                           // users can't delete ptr to ntree_node
  static void destruct(self *p);
  static self *copy(const self *, self * = 0);
  self *cut_out(child_iterator, self * = 0);
  bool has_ancestor(const self *);
  friend class ntree<T>;
  friend class _ntree_node_child_iterator<T>;
  friend class _const_ntree_node_child_iterator<T>;
  friend class _ntree_node_up_iterator<T>;
  friend class _const_ntree_node_up_iterator<T>;
};

template <class T>
class _ntree_node_up_iterator : public forward_iterator<_ntree_node<T>, ptrdiff_t>
{
public:
  typedef _ntree_node_up_iterator<T> self;
  typedef _ntree_node<T> node;

  _ntree_node_up_iterator();  // past-the-end iterator value
  // default copy and assign automatically generated
  
  self &operator++();
  self operator++(int);
  
  node &operator*() const;
  node * operator->() const;
  
  friend bool operator == <>(self, self);
  friend bool operator < <>(self, self);
  //STL templates provide !=, <=, >, >=
private:
  node *ptr;
  _ntree_node_up_iterator(node *);  // iterator pointing at *p
  friend class _ntree_node<T>;
  friend class _const_ntree_node_up_iterator<T>;
};

template <class T>
class _const_ntree_node_up_iterator : public forward_iterator<_ntree_node<T>, ptrdiff_t>
{
public:
  typedef _const_ntree_node_up_iterator<T> self;
  typedef _ntree_node<T> node;
  typedef _ntree_node_up_iterator<T> up_iterator;

  _const_ntree_node_up_iterator();  // past-the-end iterator value
  _const_ntree_node_up_iterator(const up_iterator &);  // conversion
  // default copy and assign automatically generated
  
  self &operator++();
  self operator++(int);
  
  const node &operator*() const;
  const node *operator->() const;
  
  friend bool operator == <>(self, self);
  friend bool operator < <>(self, self);
  //STL templates provide !=, <=, >, >=
private:
  const node *ptr;
  _const_ntree_node_up_iterator(const node *);  // iterator pointing at *p
  friend class _ntree_node<T>;
};

template <class T>
class _ntree_node_child_iterator : public bidirectional_iterator<_ntree_node<T>, ptrdiff_t>
{
public:
  typedef _ntree_node_child_iterator<T> self;
  typedef _ntree_node<T> node;

  _ntree_node_child_iterator();
  //generate default assignment, copy ctor

  self& operator++();
  self operator++(int);
  self& operator--();
  self operator--(int);
  node& operator*() const;
  node * operator->() const;
  friend bool operator == <>(self x, self y);
  friend bool operator < <> (self x, self y);
  //STL templates provide !=, <=, >, >=

private:
  typedef list<node *>::iterator iter_t;
  iter_t it;
  _ntree_node_child_iterator(const iter_t& i);
  friend class _ntree_node<T>;
  friend class _const_ntree_node_child_iterator<T>;
};

template <class T>
class _const_ntree_node_child_iterator : public bidirectional_iterator<_ntree_node<T>, ptrdiff_t>
{
public:
  typedef _const_ntree_node_child_iterator<T> self;
  typedef _ntree_node_child_iterator<T> child_iterator;
  typedef _ntree_node<T> node;

  _const_ntree_node_child_iterator();
  _const_ntree_node_child_iterator(const child_iterator&);
  //generate default assignment, copy ctor

  self &operator++();
  self operator++(int);
  self &operator--();
  self operator--(int);
  const node &operator*() const;
  const node *operator->() const;
  friend bool operator == <>(self, self);
  friend bool operator < <>(self, self);
  //STL templates provide !=, <=, >, >=
private:
  typedef list<node *>::const_iterator iter_t;
  iter_t it;
  _const_ntree_node_child_iterator(const iter_t& i);
  friend class _ntree_node<T>;
};

//ntree<T> inlines

template <class T>
inline ntree<T>::ntree() : root_(0) {}

template <class T>
inline ntree<T>::ntree(const self &t) : root_(node::copy(t.root_)) {}

template <class T>
inline ntree<T>::ntree(const node &x) : root_(node::copy(&x)) {}

template <class T>
inline ntree<T>::ntree(const T &x) : root_(new node(x)) {}

template <class T>
inline void ntree<T>::destruct(node *p) { _ntree_node<T>::destruct(p);}

template <class T>
inline ntree<T>::~ntree() { destruct(root_);}

template <class T>
inline ntree<T> &ntree<T>::operator = (const self &t)
{ 
  if (&t != this)
    {
      destruct(root_);
      root_ = node::copy(t.root_);
      return *this;
    }
}

template <class T>
inline ntree<T> &ntree<T>::operator = (const node &x)
{
  node *tmp = root_;
  root_ = node::copy(&x);
  destruct(tmp);
  return *this;
}

template <class T>
inline ntree<T>& ntree<T>::operator = (const T &x)
{
  node *tmp = root_;
  root_ = new node(x);
  destruct(tmp);
  return *this;
}

template <class T>
inline ntree<T> &ntree<T>::assign_cut(self &t)
{
  if (&t != this)
    {
      destruct(root_);
      root_ = t.root_;
      t.root_ = 0;
      return *this;
    }
}

template <class T>
inline ntree<T> &ntree<T>::assign_cut(node &x, child_iterator p)
{
  assert(p != x.child_end() && p->parent == &x);
  node * tmp = x.cut_out(p);
  destruct(root_);
  root_ = tmp;
  return *this;
}

template <class T>
inline void ntree<T>::erase() { destruct(root_); root_ = 0;}

template <class T>
inline _ntree_node<T> &ntree<T>::root() { assert(root_); return *root_;}

template <class T>
inline const _ntree_node<T> &ntree<T>::root() const { assert(root_); return *root_;}

template <class T>
inline bool ntree<T>::empty() const { return root_ == 0;}

template <class T>
inline void ntree<T>::swap(self &t)
{
  node *tmp = root_;
  root_ = t.root_;
  t.root_ = tmp;
}

//ntree_node_up_iterator<T> inlines

template <class T>
inline _ntree_node_up_iterator<T>::_ntree_node_up_iterator() : ptr(0) {}

template <class T>
inline _ntree_node_up_iterator<T> &_ntree_node_up_iterator<T>::operator++()
{
  assert(ptr);
  ptr = ptr->parent;
  return *this;
}

template <class T>
inline _ntree_node_up_iterator<T> _ntree_node_up_iterator<T>::operator++(int)
{ 
  assert(ptr);
  _ntree_node_up_iterator tmp = *this;
  ptr = ptr->parent;
  return tmp;
}

template <class T>
inline _ntree_node<T> &_ntree_node_up_iterator<T>::operator*() const { assert(ptr); return *ptr;}

template <class T>
inline _ntree_node<T> *_ntree_node_up_iterator<T>::operator->() const { assert(ptr); return ptr;}

template <class T>
inline bool operator==(_ntree_node_up_iterator<T> x, _ntree_node_up_iterator<T> y) { return x.ptr == y.ptr;}

template <class T>
inline bool operator < (_ntree_node_up_iterator<T> x, _ntree_node_up_iterator<T> y) { return x.ptr < y.ptr;}

template <class T>
inline _ntree_node_up_iterator<T>::_ntree_node_up_iterator(node *p) : ptr(p) {}

//const_ntree_node_up_iterator<T> inlines

template <class T>
inline _const_ntree_node_up_iterator<T>::_const_ntree_node_up_iterator() : ptr(0) {}

template <class T>
inline _const_ntree_node_up_iterator<T>::_const_ntree_node_up_iterator(const up_iterator &ui) : ptr(ui.ptr) {}

template <class T>
inline _const_ntree_node_up_iterator<T> &_const_ntree_node_up_iterator<T>::operator++()
{ 
  assert(ptr);
  ptr = ptr->parent;
  return *this;
}

template <class T>
inline _const_ntree_node_up_iterator<T> _const_ntree_node_up_iterator<T>::operator++(int)
{ 
  assert(ptr);
  up_iterator tmp = *this;
  ptr = ptr->parent;
  return tmp;
}

template <class T>
inline const _ntree_node<T> &_const_ntree_node_up_iterator<T>::operator*() const { assert(ptr); return *ptr;}

template <class T>
inline const _ntree_node<T> *_const_ntree_node_up_iterator<T>::operator->() const { assert(ptr); return ptr;}

template <class T>
inline bool operator==(_const_ntree_node_up_iterator<T> x, _const_ntree_node_up_iterator<T> y) { return x.ptr == y.ptr;}

template <class T>
inline bool operator<(_const_ntree_node_up_iterator<T> x, _const_ntree_node_up_iterator<T> y) { return x.ptr < y.ptr;}

template <class T>
inline _const_ntree_node_up_iterator<T>::_const_ntree_node_up_iterator(const node *p) : ptr(p) {}
    
//ntree_node<T> inlines

template <class T>
inline _ntree_node_up_iterator<T> _ntree_node<T>::up_begin() { return up_iterator(this);}

template <class T>
inline _const_ntree_node_up_iterator<T> _ntree_node<T>::up_begin() const { return const_up_iterator(this);}

template <class T>
inline _ntree_node_up_iterator<T> _ntree_node<T>::up_end() { return up_iterator();}

template <class T>
inline _const_ntree_node_up_iterator<T> _ntree_node<T>::up_end() const { return up_iterator();}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::child_begin() { return child_iterator(children.begin());}

template <class T>
inline _const_ntree_node_child_iterator<T> _ntree_node<T>::child_begin() const { return const_child_iterator(children.begin());}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::child_end() { return child_iterator(children.end());}

template <class T>
inline _const_ntree_node_child_iterator<T> _ntree_node<T>::child_end() const { return const_child_iterator(children.end());}

template <class T>
inline void _ntree_node<T>::erase_child(child_iterator p)
{
  assert(p != child_end() && p->parent == this);
  destruct(&*p);
  children.erase(p.it);
}

template <class T>
inline _ntree_node<T>::size_type _ntree_node<T>::num_children() const { return children.size();}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::insert(child_iterator p, const ntree<T>& t)
{
  assert(p == child_end() || p->parent == this);
  assert(t.root_);
  return children.insert(p.it, copy(t.root_, this));
}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::insert(child_iterator p, const self& x)
{
  assert(p == child_end() || p->parent == this);
  return children.insert(p, copy(&x, this));
}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::insert(child_iterator p, const T& x)
{
  assert(p == child_end() || p->parent == this);
  return children.insert(p.it, new _ntree_node<T>(x, this));
}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::insert_cut(child_iterator p, ntree<T>& t)
{
  assert(!t.empty());
  assert(p == child_end() || p->parent == this);
  assert(!has_ancestor(t.root_));
  self *tmp = t.root_;
  tmp->parent = this;
  t.root_ = 0;
  return children.insert(p.it, tmp);
}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node<T>::insert_cut(child_iterator p, self &x, child_iterator q)
{
  assert(p == child_end() || p->parent == this);
  assert(!has_ancestor(&*q));
  if (p == q) return p;
  return children.insert(p.it, x.cut_out(q, this));
}

template <class T>
inline _ntree_node<T>::_ntree_node(const T &v, self *p) : value(v), parent(p), children() {}

//ntree_node_child_iterator<T> inlines

template <class T>
inline _ntree_node_child_iterator<T>::_ntree_node_child_iterator() {}

template <class T>
inline _ntree_node_child_iterator<T> &_ntree_node_child_iterator<T>::operator++() { return ++it, *this;}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node_child_iterator<T>::operator++(int) { self tmp = *this; ++it; return tmp;}

template <class T>
inline _ntree_node_child_iterator<T> &_ntree_node_child_iterator<T>::operator--() { return --it, *this;}

template <class T>
inline _ntree_node_child_iterator<T> _ntree_node_child_iterator<T>::operator--(int) { self tmp = *this; --it; return tmp;}

template <class T>
inline _ntree_node<T> &_ntree_node_child_iterator<T>::operator*() const { assert(*it); return **it;}

template <class T>
inline _ntree_node<T> *_ntree_node_child_iterator<T>::operator->() const { return *it;}

template <class T>
inline bool operator==(_ntree_node_child_iterator<T> x, _ntree_node_child_iterator<T> y) { return x.it == y.it;}

template <class T>
inline bool operator<(_ntree_node_child_iterator<T> x, _ntree_node_child_iterator<T> y) { return x.it < y.it;}

template <class T>
inline _ntree_node_child_iterator<T>::_ntree_node_child_iterator(const iter_t &i) : it(i) {}

//const_ntree_node_child_iterator<T> inlines

template <class T>
inline _const_ntree_node_child_iterator<T>::_const_ntree_node_child_iterator() {}

template <class T>
inline _const_ntree_node_child_iterator<T>::_const_ntree_node_child_iterator(const child_iterator &ci) : it(ci.it) {}

template <class T>
inline _const_ntree_node_child_iterator<T> &_const_ntree_node_child_iterator<T>::operator++() { return ++it, *this;}

template <class T>
inline _const_ntree_node_child_iterator<T> _const_ntree_node_child_iterator<T>::operator++(int)
{ self tmp = *this; ++it; return tmp;}

template <class T>
inline _const_ntree_node_child_iterator<T> &_const_ntree_node_child_iterator<T>::operator--() { return --it, *this;}

template <class T>
inline _const_ntree_node_child_iterator<T> _const_ntree_node_child_iterator<T>::operator--(int)
{ self tmp = *this; --it; return tmp;}

template <class T>
inline const _ntree_node<T> &_const_ntree_node_child_iterator<T>::operator*() const { assert(*it); return **it;}

template <class T>
inline const _ntree_node<T> *_const_ntree_node_child_iterator<T>::operator->() const { return *it;}

template <class T>
inline bool operator==(_const_ntree_node_child_iterator<T> x, _const_ntree_node_child_iterator<T> y) { return x.it == y.it;}

template <class T>
inline bool operator<(_const_ntree_node_child_iterator<T> x, _const_ntree_node_child_iterator<T> y) { return x.it < y.it;}

template <class T>
inline _const_ntree_node_child_iterator<T>::_const_ntree_node_child_iterator(const iter_t &i) : it(i) {}

// start of ntree.cc
// #include "ntree.h"
//
template <class T>
void _ntree_node<T>::destruct(self *p)
{
  if (!p) return;
  child_iterator i;
  for (i = p->child_begin(); i != p->child_end(); ++i)
    destruct(&*i), delete &*i;
}

template <class T>
_ntree_node<T> *_ntree_node<T>::copy(const self * p, self * par)
{
  if (!p) return 0;
  self * res = new self(p->value, par);
  list<self *>::const_iterator i, iend = p->children.end();
  for (i = p->children.begin(); i != iend; ++i)
    res->children.push_back(copy(*i, res));
  return res;
}

template <class T>
_ntree_node<T> *_ntree_node<T>::cut_out(child_iterator p, self * par)
{
  assert(p != child_end() && p->parent == this);
  self * res = &*p;
  children.erase(p.it);
  res->parent = par;
  return res;
}

template <class T>
bool _ntree_node<T>::has_ancestor(const self * p)
{ 
  const self * q = this;
  while (q)
    if (q == p) return true;
    else q = q->parent;
  return false;
}

#endif /* _ntree_h */
