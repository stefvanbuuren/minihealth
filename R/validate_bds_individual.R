#' Validate JSON file
#'
#' This  function takes data from a JSON source and compares it to a JSON schema
#' object. A \code{boolean} is returned indicating whether or not the provided
#' JSON data matches the schema.
#' @param txt A JSON string, URL or file to be compared to the schema.
#' @param schema A JSON string, URL or file with the schema to evaluate
#' by. The default (\code{NULL}) loads the built-in schema
#' \code{"bds_schema_str.json"}.
#' @param verbose include an error message when validation fails.
#' @return A \code{boolean} with optional \code{errors} attribute.
#' @author Arjan Huizing 2019
#' @export
validate_bds_individual <- function(txt = NULL, schema = NULL,
                                    verbose = TRUE) {
  if (is.null(schema))
    schema <- system.file("json", "bds_schema_str.json", package = "minihealth")

  json_validate(txt, schema, engine = "ajv", verbose = verbose)
}
