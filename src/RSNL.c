#include <Rinternals.h>
#include <stdio.h>
#include "lda-estimate.h"

////////////////////////////////////////////////////////////////////////
// Code for viewable system

#define ENV_ADDR_STR_LEN 64 // this is too big but safe 

SEXP
viewable_env_addr (SEXP env)
{
  char addr[ENV_ADDR_STR_LEN];
  sprintf(addr, "%p", env);
  
  SEXP string;
  PROTECT(string = allocVector(STRSXP, 1));
  SET_STRING_ELT(string, 0, mkChar(addr));
  UNPROTECT(1);
  return string;
}

SEXP
viewable_get_pointer (SEXP obj)
{
  return R_MakeExternalPtr(obj, R_NilValue, R_NilValue);
}

SEXP
viewable_get_object (SEXP ptr)
{
  return ((SEXP) R_ExternalPtrAddr(ptr));
}
