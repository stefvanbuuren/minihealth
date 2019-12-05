check_ranges <- function(d) {

  e <- catch_cnd(dob <- ymd(extract_field2(d, 20L, "ClientGegevens", "Elementen")))
  if (!is.null(e)) warning("BDS  20 (Date of birth): Cannot parse")

  e <- catch_cnd(dobm <- ymd(extract_field3(d, 63L, "ClientGegevens", "Groepen", "Elementen")))
  if (!is.null(e)) message("BDS  63 (Date of birth caregiver): Cannot parse", appendLF = FALSE)

  ga <- extract_field2(d, 82L, "ClientGegevens", "Elementen")
  if (!is.na(ga) & (ga < 50 | ga > 350)) message("BDS  82 (Gestational age in days): Outside range 50-350", appendLF = FALSE)

  bw <- extract_field2(d, 110L, "ClientGegevens", "Elementen")
  if (!is.na(bw) & (bw < 300 | bw > 8000)) message("BDS 110 (Birth weight in grammes): Outside range 300-8000", appendLF = FALSE)

  hgtm <- extract_field2(d, 238L, "ClientGegevens", "Elementen")
  if (!is.na(hgtm) & (hgtm < 800 | hgtm > 3000)) message("BDS 238 (Height mother in mm): Outside range 800-3000", appendLF = FALSE)

  hgtf <- extract_field2(d, 240L, "ClientGegevens", "Elementen")
  if (!is.na(hgtf) & (hgtf < 800 | hgtf > 3000)) message("BDS 240 (Height father in mm): Outside range 800-3000", appendLF = FALSE)

  hdc <- wgt <- hgt <- dom <- NULL

  if (length(d$Contactmomenten) > 0L) {
    e <- catch_cnd(dom <- ymd(d$Contactmomenten[[1L]]))
    if (!is.null(e)) warning("Date of visit: Cannot parse: ", as.character(d$Contactmomenten[[1L]]))

    hgt <- extract_field(d, 235L)
    wgt <- extract_field(d, 245L)
    hdc <- extract_field(d, 252L)

    if (any(!is.na(hgt) & (hgt < 100 | hgt > 3000))) message("BDS 235 (Height in mm): Outside range 100-2500", appendLF = FALSE)
    if (any(!is.na(wgt) & (wgt < 100 | wgt > 300000))) message("BDS 245 (Weight in grammes): Outside range 100-300000", appendLF = FALSE)
    if (any(!is.na(hdc) & (hdc < 100 | hdc > 900))) message("BDS 252 (Head circumference in mm): Outside range 100-900", appendLF = FALSE)
  }

  list(dob = dob,
       dobm = dobm,
       ga = ga,
       bw = bw,
       hgtm = hgtm,
       hgtf = hgtf,
       dom = dom,
       hgt = hgt,
       wgt = wgt,
       hdc = hdc)
}
