# Default object types for various operations.  That is, this file
# constains versions of various generic methods with missing functor
# (to steal a C++ term) fields.  These methods set the default
# behavior of the generic functions that handle tokenization,
# stemming, and a wide array of other operations.

# Make PTBTokenizer the default tokenizer
setMethod("tokenize", signature(tokenizer="missing"),
  function(object, tokenizer, index)
    if (missing(index))
      tokenize(object, PTBTokenizer())
    else
      tokenize(object, PTBTokenizer(), index))

# Make Snowball the default stemmer
setMethod("stem", signature(stemmer="missing"),
  function (object, stemmer, tokenizer, index)
    if (missing(index))
      if (missing(tokenizer))
        stem(object, SnowballStemmer())
      else
        stem(object, SnowballStemmer(), tokenizer)
    else
      if (missing(tokenizer))
        stem(object, SnowballStemmer(), index=index)
      else
        stem(object, SnowballStemmer(), tokenizer, index))

# Make Tolower the default TokenTransform
setMethod("transformTokens", signature(transform="missing"),
  function (object, transform, tokenizer, index)
    if (missing(index))
      if (missing(tokenizer))
        transformTokens(object, TolowerTokenTransform())
      else
        transformTokens(object, TolowerTokenTransform(), tokenizer)
    else
      if (missing(tokenizer))
        transformTokens(object, TolowerTokenTransform(), index=index)
      else
        transformTokens(object, TolowerTokenTransform(), tokenizer, index))

# Make an english stopword filter the default TokenFilter
setMethod("filterTokens", signature(filter="missing"),
  function (object, filter, tokenizer, index)
    if (missing(index))
      if (missing(tokenizer))
        filterTokens(object, StopFilter())
      else
        filterTokens(object, StopFilter(), tokenizer)
    else
      if (missing(tokenizer))
        filterTokens(object, StopFilter(), index=index)
      else
        filterTokens(object, StopFilter(), tokenizer, index))

# Make an english pos tagger default
setMethod("tag", signature(tagger="missing"),
  function (object, tagger, tokenizer, index)
    if (missing(index))
      if (missing(tokenizer))
        tag(object, MaxentTagger())
      else
        tag(object, MaxentTagger(), tokenizer)
    else
      if (missing(tokenizer))
        tag(object, MaxentTagger(), index=index)
      else
        tag(object, MaxentTagger(), tokenizer, index))
