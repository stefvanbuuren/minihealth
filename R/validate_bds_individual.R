#' Validate JSON file
#'
#' This function takes data from a JSON source and compares it to a JSON schema
#' object. A \code{boolean} is returned indicating whether or not the provided
#' JSON data matches the schema.
#' @param txt a JSON string, URL or file to be compared to the schema.
#' @param verbose include an error message when validation fails.
#' @param schema the schema to evaluate by. options are "default" and "string".
#' @return A \code{boolean} with optional \code{errors} attribute.
#' @author Arjan Huizing 2019
#' @export
validate_bds_individual <- function(txt = NULL, verbose = TRUE,
                                    schema = "default"){

  path <- file.path(path.package("minihealth"), "json")
  bds_schema <- switch(schema,
                       "default" = file.path(path, "bds_schema.json"),
                       "string"  = file.path(path, "bds_schema_str.json"))
  valid <- jsonvalidate::json_validate(txt, bds_schema, engine = "ajv",
                                       verbose = verbose)

  if(valid | !verbose){
    return(valid)
  } else{
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
}
