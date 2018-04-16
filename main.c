#include <stdio.h>
#include <stdlib.h>
#define TRUE 0xFFFFFFFE
#define FALSE 0x7FFFFFFE

extern int our_code_starts_here() asm("our_code_starts_here");
extern void error(int val) asm('error');

void error(int error_code) {
  if(error_code == 1)
    fprintf(stderr, "Error: expected a number\n");
  else if (error_code == 2)
    fprintf(stderr, "Error: expected a boolean\n");
  else if (error_code == 3)
    fprintf(stderr, "Error: overflow\n");
  else if (error_code == 4)
    fprintf(stderr, "Error: input must be a boolean or a number\n");
  exit(123456);
}

int main(int argc, char** argv) {
  int input_val = FALSE;
  if (argc > 1) {
    if (strcmp("true", argv[1])) {
      input_val = TRUE;
    } else if (strcmp("false", argv[1])) {
      input_val = FALSE;
    } else if (sscanf(argv[1], "%d", &input_val) > 0) {
      input_val = input_val << 1 | 1;
    } else {
      error(4);
    }
  }
  int result = our_code_starts_here();
  print(result);
  return 0;
}
