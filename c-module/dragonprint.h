#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iconv.h>

int print_some(){
  printf("sdfsdfsd\n");
  return 9;
}

int print_second(int count){
  fprintf(stdout, "sdfsdfie %d\n", count);
  return count+1;
}

// 转换字符串内容编码为utf8编码格式

int iconv_to_utf8(char *content, char *from_encode){
  iconv_t cd = iconv_open("utf8", from_encode);
  printf("cd value:%d\n", (int)cd);
  if(cd <= 0) return -1;
  
  char inbuf[255] = "ҪθĩФlsdf dfsdfsdf";
  char outbuf[255];

  char *pin = inbuf;
  char *pout = outbuf;
  size_t inlen = strlen(inbuf)+1;
  size_t outlen = 255;

  int convert_val = iconv(cd, &pin, &inlen, &pout, &outlen);
  printf("convert_val:%d\n", (int)convert_val);
  printf("out char:%s\n", outbuf);
  if(convert_val < 0) return -1;
  iconv_close(cd);
  printf("out char:%s\n", outbuf);
 //
 // //int len = strlen(content);
 // int len = 9;
 // printf("%d", len);
 // //  char *tocontent = (char*)malloc(sizeof(char*));
 // //memset(tocontent, 0, strlen(tocontent));
 // char **pin = &content;
 // 
 // int outlen = 255;
 // char outcontent[255];
 // memset(outcontent, 0 , outlen);
 // //  char *tmp_out = *outcontent;
 // //  char **pout = &tmp_out;
 //
 // //  int t =  iconv(cd, pin, &len, pout, &outlen);
 // int t =  iconv(cd, pin, &len, **outcontent, &outlen);
 // if(t == 0)  return 0;
 // iconv_close(cd);
  //  puts(*pout);
  //  printf("%s\n", *pout);
  return 0;
}
