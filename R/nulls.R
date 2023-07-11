# A series of null class types to help in lazy loading

setClass("DocumentTermMatrix")
setClass("TermDocumentMatrix")

setClass("nNULL")
setClass("NULLtable", representation("nNULL", "table"))
setClass("NULLcharacter", representation("nNULL", "character"))
setClass("NULLlist", representation("nNULL", "list"))
setClass("NULLDocumentTermMatrix", representation("nNULL", "DocumentTermMatrix"))
