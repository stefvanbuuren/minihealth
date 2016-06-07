# movie = c("Thunderball", "Goldfinger")
# rating = c(4,5)
# dfJoe = data.frame(movie = movie, rating = rating)
#
# movie = c("Manhattan", "Interiors", "Radio Days", "Bananas")
# rating = c(5, 4, 3, 5)
# dfBob = data.frame(movie = movie, rating = rating)
#
# setClass("BorrowedStuff", representation(stuff = "data.frame", from="character"))
#
# JoesStuff = new("BorrowedStuff", from = "Joe", stuff = dfJoe)
#
# BobsStuff = new("BorrowedStuff", from = "Bob", stuff = dfBob)
#
# sillyFunction = function(x){
#   x + 1
# }
#
# sillyFunction(1)
# sillyFunction(1:10)
#
# whatStuff = new("BorrowedStuff", from = c("Joe", "Bob"), stuff = c(dfJoe, dfBob))
# whatStuff = new("BorrowedStuff", from = c("Joe", "Bob"), stuff = list(dfJoe, dfBob))
#
# setMethod("c", signature(x = "BorrowedStuff"), function(x, ...){
#   elements = list(x, ...)
#
#   stuffList = list()
#   for (i in 1:length(elements)){
#     stuffList[i] = new("BorrowedStuff", from = slot(elements[[i]], "from"), stuff = slot(elements[[i]], "stuff"))
#   }
#
#   class(stuffList) = "BorrowedStuff"
#
#   stuffList
#
# })
#
# whatStuff = c(JoesStuff, BobsStuff)
# whatStuff[[1]]@stuff
#
# someStuff = whatStuff[[1]]
#
#
#
# # ----
#
# .MyClass <- setClass("MyClass", representation(a="numeric", b="character"))
#
# setMethod("[", c("MyClass", "numeric", "missing"), function(x, i, j, ...) {
#   do.call(initialize, c(x, sapply(slotNames(x), function(y) slot(x, y)[i],
#                                   simplify=FALSE)))
# })
#
# setMethod("length", "MyClass", function(x) length(x@a))
#
# #different
# as.list.MyClass <-function(x) {
#   lapply(seq_along(x), function(i) x[i])
# }
# setMethod("as.list", "MyClass", as.list.MyClass)
#
# #test
# myobj <- .MyClass(a=1:4, b=letters[1:4])
# lapply(myobj, function(i) rep(i@b, i@a))
#
