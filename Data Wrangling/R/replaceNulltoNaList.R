#Replace Null values contained in a list with NA values

replaceNulltoNaList <- function (my.list, FUN, ...) 
  {
      if (is.list(my.list)) {
          for (i in seq_along(my.list)) {
              my.list[i] <- list(replaceNaList(my.list[[i]], FUN, ...))
          }
          my.list
      }
      else FUN(my.list, ...)
  } 

