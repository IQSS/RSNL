# A Penn Tree Bank compliant tokenizer

### CLASS DEF

setClass("PTBTokenizer",
    representation(
      "Tokenizer"           # extends the tokenizer virtual class
    )
)

### CONSTRUCTION

PTBTokenizer  <- function()
{
  return(new("PTBTokenizer"))
}

### overridden tokenize method

# Tokenize a character object
setMethod("tokenize", signature(object="character",
                                tokenizer="PTBTokenizer",
                                index="missing"), 
  function(object, tokenizer, index)
    unlist(lapply(object,
      function (x) .jcall("PTBTokenizerBridge",
                          "[S", "PTBtokenize", x))))

### SHOW

setMethod("show", "PTBTokenizer",
  function (object)
    cat("A tokenizer that conforms to Penn Treebank conventions.\n"))
