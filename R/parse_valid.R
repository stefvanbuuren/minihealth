parse_valid <- function(valid) {

  mess <- list(required = character(0),
               supplied = character(0))
  if (valid) return(mess)

  # extract error information
  w <- attr(valid, "error")

  # For required errors
  mess$required <- w[w$keyword == "required", "message"]

  # For anyOf errors
  val.err <- t(simplify2array(w[w$keyword == "anyOf", "data"]))
  if (ncol(val.err) >= 1L) {
    user.warning <- data.frame()
    for (i in 1L:nrow(val.err)) {
      user.warning[i, "bdsnummer"] <- val.err[i, 1L][[1L]]
      user.warning[i, "supplied"] <- ifelse(is.null(val.err[i, 2L][[1L]]),
                                            NA, as.character(val.err[i, 2L][[1L]]))
      user.warning[i, "supplied type"] <- ifelse(is.null(val.err[i, 2L][[1L]]),
                                                 NA, mode(val.err[i, 2L][[1L]]))
    }
    mess$supplied <- merge(user.warning, minihealth::bds_lexicon, by = "bdsnummer") %>%
      select(one_of(c("bdsnummer", "description", "expected", "supplied", "supplied type")))
  }

  mess
}
