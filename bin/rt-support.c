#include <stdio.h>
#include <stdlib.h>

int getint(){
  char buffer[32];
  int res;
  
  if(fgets(buffer, 32, stdin) == NULL) return 0;

  return (int)strtol(buffer, NULL, 10);
}

void print(int n){
  printf("%d ",n);
}