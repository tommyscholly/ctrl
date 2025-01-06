// The first standard library of ctrl
// Functions that are prefixed with ctrl_ are functions that are meant for
// internal use only inside the compiler

#ifndef _CTRL_STD_H
#define _CTRL_STD_H

struct ctrl_string {
  char *str;
  int len;
};

struct ctrl_string *ctrl_make_string(const char *str);

void print_string(struct ctrl_string *str);
// void print_string(const char *str);

#endif
