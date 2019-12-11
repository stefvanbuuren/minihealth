
convert_ddi_gsed <- function(d, r) {

  # if (length(d$Contactmomenten) == 0L) return(NULL)

  # prepare the output matrices
  bds <- sort(unique(minihealth::bds_gsed$bds))
  items <- minihealth::bds_gsed$lex_gsed[minihealth::bds_gsed$lex_gsed != ""]
  agedays <- r$dom - r$dob
  w <- matrix(NA, nrow = length(agedays), ncol = 1L + length(bds) + length(items),
              dimnames = list(NULL, c("agedays", bds, items)))
  w[, "agedays"] <- agedays

  # extract ddi data from bds-message
  # and convert to 0/1 scores
  for (i in bds) w[, as.character(i)] <- extract_field(d, i)
  for (item in items) {
    n <- which(minihealth::bds_gsed$lex_gsed == item)
    type <- minihealth::bds_gsed[n, "type"]
    bds  <- minihealth::bds_gsed[n, c("bds", "bdsr", "bdsl")]
    w[, item] <- switch(type,
      g1 = recode(w[, as.character(bds[1L])], `1` = 1L, `2` = 0L),
      m1 = recode(w[, as.character(bds[1L])], `1` = 1L, `2` = 0L, `3` = 1L),
      g2 = {
        bdsr <- w[, as.character(bds[2L])]
        bdsl <- w[, as.character(bds[3L])]
        pass <- as.numeric(bdsr == 1L & bdsl == 1L)
        pass[is.na(bdsr) & is.na(bdsl)] <- NA
        pass},
      m2 = {
        bdsr <- w[, as.character(bds[2L])]
        bdsl <- w[, as.character(bds[3L])]
        pass <- as.numeric(bdsr %in% c(1L, 3L) & bdsl %in% c(1L, 3L))
        pass[is.na(bdsr) & is.na(bdsl)] <- NA
        pass},
      stop("Unrecognized type ", type, " for item ", item)
    )
  }
  w[, c("agedays", items)]
}
