// The first standard library of ctrl
// Functions that are prefixed with ctrl_ are functions that are meant for
// internal use only inside the compiler

#ifndef _CTRL_STD_H
#define _CTRL_STD_H

#include <stdio.h>
struct ctrl_string {
  char *str;
  int len;
};

struct ctrl_string *ctrl_make_string(const char *str, size_t len);

struct ctrl_string *ctrl_concat_string(struct ctrl_string *str1,
                                       struct ctrl_string *str2);

void print_string(struct ctrl_string *str);
// void print_string(const char *str);
void print_int(int i);

#endif
