#' replaceNulltoNaList
#'
#' Replace Null values contained in a list with NA values
#'
#' @param my.list is a list
#' @param FUN is a function to be parsed (See example)
#'
#' @return A data.frame containing the records considered correct.
#' @export
#' @examples 
#' list.NA<- list("a" = 2.5, "b" = NA, "c" = 1:3)
#' List.new<-replaceNulltoNaList(list.NA , function(x)if(is.null(x))NA else x)

replaceNulltoNaList <- function (my.list, FUN, ...) 
  {
      if (is.list(my.list)) {
          for (i in seq_along(my.list)) {
              my.list[i] <- list(replaceNulltoNaList(my.list[[i]], FUN, ...))
          }
          my.list
      }
      else FUN(my.list, ...)
  } 

