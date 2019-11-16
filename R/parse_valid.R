
parse_valid <- function(valid) {

  # default message
  errors <- list(required = "All required fields present",
                 supplied = "All supplied values supported")

  # extract error information
  warnings <- valid %>%
    attr("error")

  # For required errors
  if("required" %in% warnings$keyword){
    errors[["required"]] <- warnings %>%
      filter(keyword == "required") %>%
      select(message) %>%
      unname
  }

  # For anyOf errors
  if("anyOf" %in% warnings$keyword){
    val.err <- warnings %>%
      filter(.data$keyword %in% "anyOf") %>%
      select(.data$data) %>%
      simplify2array()

    # convert from list to data.frame
    user.warning <- data.frame()
    for(i in seq_along(val.err)) {
      user.warning[i, "bdsnummer"] <- val.err[i, 1]$data$Bdsnummer
      user.warning[i, "supplied"] <- ifelse(is.null(val.err[i, 1]$data$Waarde),
                                            NA, as.character(val.err[i, 1]$data$Waarde))
      user.warning[i, "supplied type"] <- ifelse(is.null(val.err[i, 1]$data$Waarde),
                                                 NA, mode(val.err[i, 1]$data$Waarde))
    }

    errors[["supplied"]] <- merge(user.warning, bds_lexicon, by = "bdsnummer") %>%
      select(bdsnummer, description, expected, supplied, `supplied type`)
  }

  # For type errors outside of bdsnumber values
  # to be added.

  # Add errors as attribute
  attr(valid, "errors") <- errors
  return(valid)
}
