#' Validate JSON file
#'
#' This function takes data from a JSON source and compares it to a JSON schema
#' object. A \code{boolean} is returned indicating whether or not the provided
#' JSON data matches the schema.
#' @param txt a JSON string, URL or file to be compared to the schema.
#' @param verbose include an error message when validation fails.
#' @return A \code{boolean} with optional \code{errors} attribute.
#' @importFrom jsonvalidate json_validate
#' @importFrom dplyr "%>%" filter select
#' @author Arjan Huizing 2019
#' @export
validate_bds_individual <- function(txt = NULL, verbose = TRUE){

  bds_schema <- file.path(path.package("minihealth"), "json", "bds_schema")
  valid <- jsonvalidate::json_validate(txt, bds_schema, engine = "ajv", verbose = verbose)

  if(valid | !verbose){
    return(valid)
  } else{

    # extract error information
    warnings <- valid %>% attr("error") %>%
      filter(keyword %in% c("anyOf", "required")) %>%
      select(data) %>% simplify2array

    # convert from list to data.frame
    user.warning <- data.frame()
    for(i in 1:length(warnings)){
      user.warning[i, "Bdsnummer"] <- warnings[i,1]$data$Bdsnummer
      user.warning[i, "Waarde"] <- ifelse(is.null(warnings[i,1]$data$Waarde),
                                          NA, as.character(warnings[i,1]$data$Waarde))
      user.warning[i, "Supplied.class"] <- ifelse(is.null(warnings[i,1]$data$Waarde),
                                          NA, class(warnings[i,1]$data$Waarde))
    }

    attr(valid, "errors") <- user.warning
    return(valid)
  }
}
