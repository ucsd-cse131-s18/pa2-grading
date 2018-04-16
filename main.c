#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define TRUE 0xFFFFFFFE
#define FALSE 0x7FFFFFFE

extern int our_code_starts_here() asm("our_code_starts_here");
extern int print(int val) asm("print");
extern void error(int val) asm("error");
extern int input() asm("input");
int input_val;

int print(int val) {
  if (val == 0xFFFFFFFE) {
    printf("true\n");
  } else if (val == 0x7FFFFFFE) {
    printf("false\n");
  } else if ((val & 1) == 1) {
    printf("%d\n", val>>1);
  } else {
    printf("Unknown value: %#010x\n", val);
  }
  return val;
}

void error(int error_code) {
  if(error_code == 1)
    fprintf(stderr, "error: expected an int\n");
  else if(error_code == 2)
    fprintf(stderr, "error: expected a boolean\n");
  else
    fprintf(stderr, "error: overflow of arithmetic operation\n");

  exit(123456);
}

int input() {
    return input_val;
}

int parse_input(char* input) {
  int val;
  if (strcmp("true", input)) {
    val = TRUE;
  } else if (strcmp("false", input)) {
    val = FALSE;
  } else if (sscanf(input, "%d", &val) > 0) {
    val = val << 1 | 1;
  } else {
    val = FALSE;
  }
  return val;
}

int main(int argc, char** argv) {
  input_val = argc > 1 ? parse_input(argv[1]) : FALSE;
  int result = our_code_starts_here();
  print(result);
  return 0;
}
