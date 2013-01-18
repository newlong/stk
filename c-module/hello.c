
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
  //iconv_to_utf8("sdfsdfsdf", "gbk");

  char inbuf[255] = "ҪθĩФlsdf dfsdfsdf";
  //char outbuf[255];
  char *outbuf;

  char *result_cnt = iconv_utf8(inbuf, outbuf, "gbk");
  //printf("main result cnt:%s\n", result_cnt);
  printf("out buffer:%s\n", outbuf);
  //for(int i = 0; i < strlen(outbuf); i++){
  //	printf("%d", outbuf[i]);
  //}

  //  printf(s);
  return 0;
}
