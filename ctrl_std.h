// The first standard library of ctrl
// Functions that are prefixed with ctrl_ are functions that are meant for
// internal use only inside the compiler

#ifndef _CTRL_STD_H
#define _CTRL_STD_H

#include <stdio.h>
struct ctrl_string {
  int len;
  char *str;
};

struct ctrl_array {
  int len;
  void *data;
};

// what we can cast to for generic sizeable types
struct sizeable {
  int len;
};

struct ctrl_string *ctrl_make_string(const char *str, size_t len);

struct ctrl_string *ctrl_concat_string(struct ctrl_string *str1,
                                       struct ctrl_string *str2);

int ctrl_size_of(struct sizeable *s);

struct ctrl_array *ctrl_make_array(int len, size_t element_size);

void print_string(struct ctrl_string *str);
// void print_string(const char *str);
void print_int(int i);

#endif
