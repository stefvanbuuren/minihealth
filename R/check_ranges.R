check_ranges <- function(d) {

  e <- catch_cnd(dob <- ymd(extract_field2(d, 20L, "ClientGegevens", "Elementen")))
  if (!is.null(e)) abort("BDS  20 (Date of birth): Cannot parse")

  e <- catch_cnd(dobm <- ymd(extract_field3(d, 63L, "ClientGegevens", "Groepen", "Elementen")))
  if (!is.null(e)) message("BDS  63 (Date of birth caregiver): Cannot parse")

  ga <- extract_field2(d, 82L, "ClientGegevens", "Elementen")
  if (!is.na(ga) & (ga < 50 | ga > 350)) message("BDS  82 (Gestational age in days): Outside range 50-350")

  bw <- extract_field2(d, 110L, "ClientGegevens", "Elementen")
  if (!is.na(bw) & (bw < 300 | bw > 8000)) message("BDS 110 (Birth weight in grammes): Outside range 300-8000")

  hgtm <- extract_field2(d, 238L, "ClientGegevens", "Elementen")
  if (!is.na(hgtm) & (hgtm < 800 | hgtm > 3000)) message("BDS 238 (Height mother in mm): Outside range 800-3000")

  hgtf <- extract_field2(d, 240L, "ClientGegevens", "Elementen")
  if (!is.na(hgtf) & (hgtf < 800 | hgtf > 3000)) message("BDS 240 (Height father in mm): Outside range 800-3000")

  hdc <- wgt <- hgt <- dom <- NULL

  if (length(d$Contactmomenten) > 0L) {
    e <- catch_cnd(dom <- ymd(d$Contactmomenten[[1L]]))
    if (!is.null(e)) abort("Date of visit: Cannot parse")

    hgt <- extract_field(d, 235L)
    wgt <- extract_field(d, 245L)
    hdc <- extract_field(d, 252L)

    if (any(!is.na(hgt) & (hgt < 100 | hgt > 3000))) message("BDS 235 (Height in mm): Outside range 100-2500")
    if (any(!is.na(wgt) & (wgt < 100 | wgt > 300000))) message("BDS 245 (Weight in grammes): Outside range 100-300000")
    if (any(!is.na(hdc) & (hdc < 100 | hdc > 900))) message("BDS 252 (Head circumference in mm): Outside range 100-900")
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
