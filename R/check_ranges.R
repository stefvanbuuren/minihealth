check_ranges <- function(d) {

  e <- catch_cnd(dob <- ymd(extract_field2(d, 20L, "ClientGegevens", "Elementen")))
  if (!is.null(e)) abort("Invalid date of birth (BDS 20)")

  e <- catch_cnd(dobm <- ymd(extract_field3(d, 63L, "ClientGegevens", "Groepen", "Elementen")))
  if (!is.null(e)) message("Invalid date of birth mother (BDS 63)")

  ga <- extract_field2(d, 82L, "ClientGegevens", "Elementen")
  if (!is.na(ga) & (ga < 50 | ga > 350)) message("Gestational age (in days) outside range 50-350 (BDS 82)")

  bw <- extract_field2(d, 110L, "ClientGegevens", "Elementen")
  if (!is.na(bw) & (bw < 300 | bw > 8000)) message("Birth weight (in grammes) outside range 300-8000 (BDS 110)")

  hgtm <- extract_field2(d, 238L, "ClientGegevens", "Elementen")
  if (!is.na(hgtm) & (hgtm < 800 | hgtm > 3000)) message("Height mother (in mm) outside range 800-3000 (BDS 238)")

  hgtf <- extract_field2(d, 240L, "ClientGegevens", "Elementen")
  if (!is.na(hgtf) & (hgtf < 800 | hgtf > 3000)) message("Height father (in mm) outside range 800-3000 (BDS 240)")

  hdc <- wgt <- hgt <- dom <- NULL

  if (length(d$Contactmomenten) > 0L) {
    e <- catch_cnd(dom <- ymd(d$Contactmomenten[[1L]]))
    if (!is.null(e)) abort("Invalid date of visit")

    hgt <- extract_field(d, 235L)
    wgt <- extract_field(d, 245L)
    hdc <- extract_field(d, 252L)

    if (any(!is.na(hgt) & (hgt < 100 | hgt > 3000))) message("Height (in mm) outside range 100-2500 (BDS 235)")
    if (any(!is.na(wgt) & (wgt < 100 | wgt > 300000))) message("Weight (in grammes) outside range 100-300000 (BDS 245)")
    if (any(!is.na(hdc) & (hdc < 100 | hdc > 900))) message("Head circumference (in mm) outside range 100-900 (BDS 252)")
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
