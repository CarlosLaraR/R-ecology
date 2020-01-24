#' insensitive_join
#'
#' Case-insensitive joins for dplyr (inner_join, left_join, full_join, semi_join_anti_join). It has only be tested with inner_join.
#'
#' @param fun is a join function defined in dplyr
#'
#' @return A data.frame containing the records considered correct.
#' @export
#' @examples 
#' insentivie_join (fun=inner_join) (df.X, df.Y, by=list(x="field.X", y="field.Y")

insensitive_join <- function(fun,...) { 
  require(dplyr)
  new_fun <- fun
  body(new_fun) <- substitute({
    by <- common_by(by, x, y)
    tmp_by_x <- paste0("_", by$x, "_")
    tmp_by_y <- paste0("_", by$y, "_")
    for (i in seq_along(by$x)) {
      x[[tmp_by_x[[i]]]] <- tolower(x[[by$x[[i]]]])
      y[[tmp_by_y[[i]]]] <- tolower(y[[by$y[[i]]]])
      y[[by$y[[i]]]] <- NULL
    }
    res <- fun(x, y, list(x = tmp_by_x, y = tmp_by_y))
    res[tmp_by_x] <- list(NULL)
    res
  })
  new_fun
}

#Usage:  insentivie_join (fun=inner_join) (df.X, df.Y, by=list(x="field.X", y="field.Y")

