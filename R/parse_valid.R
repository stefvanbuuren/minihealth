
parse_valid <- function(valid) {

  # extract error information
  warnings <- valid %>%
    attr("error") %>%
    filter(.data$keyword %in% c("anyOf", "required")) %>%
    select(.data$data) %>%
    simplify2array()

  # convert from list to data.frame
  user.warning <- data.frame()
  for(i in seq_along(warnings)) {
    user.warning[i, "Bdsnummer"] <- warnings[i, 1]$data$Bdsnummer
    user.warning[i, "Waarde"] <- ifelse(is.null(warnings[i, 1]$data$Waarde),
                                        NA, as.character(warnings[i, 1]$data$Waarde))
    user.warning[i, "Mode"] <- ifelse(is.null(warnings[i, 1]$data$Waarde),
                                      NA, mode(warnings[i, 1]$data$Waarde))
  }

  attr(valid, "errors") <- user.warning
  return(valid)
}
