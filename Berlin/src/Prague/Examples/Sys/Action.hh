#ifndef _Action_hh
#define _Action_hh

class Action
{
public:
  virtual ~Action(){}
  virtual void execute() = 0;
};

#endif /* _Action_hh */
