#include "ctrl_std.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct ctrl_string *ctrl_make_string(const char *str) {
  struct ctrl_string *new_str = malloc(sizeof(struct ctrl_string));
  new_str->len = strlen(str);
  new_str->str = malloc(new_str->len + 1);
  strcpy(new_str->str, str);
  return new_str;
}

void print_string(struct ctrl_string *str) { printf("%s\n", str->str); }
// void print_string(const char *str) { printf("%s\n", str); }
