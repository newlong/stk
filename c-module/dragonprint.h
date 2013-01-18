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
  return 0;
}

char* iconv_utf8(char *content,char *out_cnt, char *from_encode){
  iconv_t cd = iconv_open("utf8", from_encode);
  printf("cd value:%d\n", (int)cd);
  if(cd <= 0) return *content;
  
  size_t inlen = strlen(content)+1;
  printf("content length:%d\n", inlen);
  int outint = strlen(content)+1;
  outint = outint * 2;
  size_t outlen = outint;
  char outbuf[outint];
  char *pin = content;
  char *pout = outbuf;
  
  int convert_val = iconv(cd, &pin, &inlen, &pout, &outlen);
  printf("convert_val:%d\n", (int)convert_val);
  printf("out char:%s\n", outbuf);
  //if(convert_val < 0) return -1;
  iconv_close(cd);
  printf("out char:%s\n", outbuf);

  int count = strlen(outbuf) > strlen(out_cnt) ? strlen(out_cnt) : strlen(outbuf);
  strcpy(out_cnt, outbuf);
  printf("out_cnt:%s \n", out_cnt);
  return out_cnt;
}
