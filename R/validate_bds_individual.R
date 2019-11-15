#' Validate JSON file
#'
#' This function takes data from a JSON source and compares it to a JSON schema
#' object. A \code{boolean} is returned indicating whether or not the provided
#' JSON data matches the schema.
#' @param txt a JSON string, URL or file to be compared to the schema.
#' @param schema the schema to evaluate by. options are "default" and "string".
#' @param verbose include an error message when validation fails.
#' @return A \code{boolean} with optional \code{errors} attribute.
#' @author Arjan Huizing 2019
#' @export
validate_bds_individual <- function(txt = NULL, schema = "default",
                                    verbose = TRUE) {

  # define schema file
  path <- file.path(path.package("minihealth"), "json")
  bds_schema <- file.path(path, "bds_schema.json")
  if (schema == "string") bds_schema <- file.path(path, "bds_schema_str.json")

  json_validate(txt, bds_schema, engine = "ajv", verbose = verbose)
}
