#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#define TRUE 0xFFFFFFFE
#define FALSE 0x7FFFFFFE

const long int INT_MIN = - (1 << 30);
const long int INT_MAX = (1 << 30) - 1;

extern int our_code_starts_here(int input_val) asm("our_code_starts_here");
extern void error(int val) asm("error");

int print(int val) {
  if (val == TRUE) {
    printf("true\n");
  } else if (val == FALSE) {
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
    fprintf(stderr, "Error: expected a number\n");
  else if (error_code == 2)
    fprintf(stderr, "Error: expected a boolean\n");
  else if (error_code == 3)
    fprintf(stderr, "Error: overflow\n");
  else if (error_code == 4)
    fprintf(stderr, "Error: input must be a boolean or a number\n");
  else if (error_code == 5)
    fprintf(stderr, "Error: input is not a representable number\n");
  exit(123456);
}

int main(int argc, char** argv) {
  int input_val;
  // FILL IN YOUR CODE FROM HERE
  char * endptr;
  extern int errno;
  
  input_val = FALSE;
  if (argc > 1) {
    if (!strcmp("true", argv[1])) {
      input_val = TRUE;
    } else if (!strcmp("false", argv[1])) {
      input_val = FALSE;
    } else {
      endptr = (char*) &argv[1];
      errno = 0;
      long r = strtol(argv[1], &endptr, 10);
      if (*endptr != '\0') {
        error(4);
      }
      else if ( errno ) {
        error(5);
      }
      input_val = r << 1 | 1;
    }
  }
  // YOUR CODE ENDS HERE
  int result = our_code_starts_here(input_val);
  print(result);
  return 0;
}
