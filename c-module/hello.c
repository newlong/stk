
#include <stdio.h>
#include "dragonprint.h"
//int print_some(){
//  printf("sdfsdfsd\n");
//  return 9;
//}

//int print_second(int count){
//  fprintf(stdout, "sdfsdfie %d\n", count);
//  return count+1;
//}

//#include <stdio.h>
int main(int argc, char *argv){
  print_some();
  int count = 5;
  fprintf(stdout, "main program\n", count);
  iconv_to_utf8("sdfsdfsdf", "gbk");
  //  printf(s);
  return 0;
}
