#include "ctrl_std.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct ctrl_string *ctrl_make_string(const char *str, size_t len) {
  struct ctrl_string *new_str = malloc(sizeof(struct ctrl_string));
  new_str->len = len;
  new_str->str = malloc(len + 1);
  strncpy(new_str->str, str, len);

  return new_str;
}

struct ctrl_string *ctrl_concat_string(struct ctrl_string *str1,
                                       struct ctrl_string *str2) {
  struct ctrl_string *new_str = malloc(sizeof(struct ctrl_string));
  new_str->len = str1->len + str2->len;
  new_str->str = malloc(new_str->len + 1);
  strcpy(new_str->str, str1->str);
  strcat(new_str->str, str2->str);

  return new_str;
}

void print_string(struct ctrl_string *str) { printf("%s\n", str->str); }
// void print_string(const char *str) { printf("%s\n", str); }

void print_int(int i) { printf("%d\n", i); }
