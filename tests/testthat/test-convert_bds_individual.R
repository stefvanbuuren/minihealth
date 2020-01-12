context("convert_bds_individual")

empty <- new("individual")
js1 <- minihealth::convert_individual_bds(empty)

# test the empty object
test_that("handles the empty individual object",
           expect_warning(is.individual(convert_bds_individual(js1)),
          "should have required property 'Elementen'"))

minimal <- new("individual", sex = "male", dob = as.Date("20181231", "%Y%m%d"))
js2 <- minihealth::convert_individual_bds(minimal)

# test the minimal object
test_that("handles the minimal individual object",
          expect_message(convert_bds_individual(js2),
                         "Missing 'Contactmomenten'",
                         fixed = TRUE))

# test the below minimal (without sex field)
below <- new("individual", dob = as.Date("20181231", "%Y%m%d"))
js3 <- minihealth::convert_individual_bds(below)
test_that("handles the below minimal object",
          expect_warning(convert_bds_individual(js3),
                         "verplicht BDS nummer ontbreekt: 19"))

jtf <- system.file("extdata", "test", paste0("test", 1:24, ".json"), package = "jamestest")

test_that("test1.json (client3.json) passes convert_individual_bds()",
          expect_s4_class(convert_bds_individual(jtf[1]), "individual"))

test_that("test2.json (missing Referentie) PASSES",
          expect_s4_class(convert_bds_individual(jtf[2]), "individual"))

test_that("test3.json (missing OrganisatieCode) WARNS",
          expect_warning(convert_bds_individual(jtf[3]),
                       "should have required property 'OrganisatieCode'"))

test_that("test4.json (wrong type) WARNS",
          expect_warning(convert_bds_individual(jtf[4]),
                       ".OrganisatieCode should be integer"))

test_that("test5.json (missing ClientGegevens) WARNS",
          expect_warning(convert_bds_individual(jtf[5]),
                       "should have required property 'ClientGegevens'"))

test_that("test6.json (Missing ContactMomenten) WARNS",
          expect_message(convert_bds_individual(jtf[6]),
                         "Missing 'Contactmomenten'",
                         fixed = TRUE))

test_that("test7.json (Missing Referentie & OrganisatieCode) WARNS",
          expect_warning(convert_bds_individual(jtf[7]),
                       "should have required property 'OrganisatieCode'"))

test_that("test8.json (Invalid JSON) WARNS",
          expect_warning(convert_bds_individual(jtf[8]),
                         "lexical error: invalid char in json text."))

test_that("test9.json (Bdsnummer 19 missing) returns warning",
          expect_warning(convert_bds_individual(jtf[9]),
                         "verplicht BDS nummer ontbreekt: 19"))

test_that("test10.json (Bdsnummer 20 missing) return warning",
          expect_warning(convert_bds_individual(jtf[10]),
                         "verplicht BDS nummer ontbreekt: 20"))

test_that("test11.json (Bdsnummer 82 missing) return message",
          expect_message(convert_bds_individual(jtf[11]),
                         "BDS 82 (Zwangerschapsduur in dagen) heeft geen waarde",
                         fixed = TRUE))

test_that("test12.json (Bdsnummer 91 missing) PASSES",
          expect_s4_class(convert_bds_individual(jtf[12]), "individual"))

test_that("test13.json (Bdsnummer 110 missing) returns message",
          expect_message(convert_bds_individual(jtf[13]),
                         "BDS 110 (Geboortegewicht in grammen: heeft geen waarde",
                         fixed = TRUE))

test_that("test14.json (empty file) returns message",
          expect_warning(convert_bds_individual(jtf[14]), "premature EOF"))

test_that("test15.json (Bdsnummer 19 numeric) returns message",
          expect_message(convert_bds_individual(jtf[15]),
                       '[{"bdsnummer":19,"description":"Sex of child","expected":"one of: 0, 1, 2, 3","supplied":"2","supplied_type":"numeric"},{"bdsnummer":62,"description":"Caretaker relation","expected":"one of: 01, 02, 03, 04, 05, 06, 07, 08, 98","supplied":"1","supplied_type":"numeric"}]'))

test_that("test16.json (Bdsnummer 20 numeric) PASSES",
          expect_silent(convert_bds_individual(jtf[16])))

test_that("test17.json (Bdsnummer 82 numeric) PASSES",
          expect_silent(convert_bds_individual(jtf[17])))

test_that("test18.json (Bdsnummer 91 numeric) produces message",
          expect_message(convert_bds_individual(jtf[18]),
                         '[{"bdsnummer":91,"description":"Smoking during pregnancy","expected":"one of: 1, 2, 99","supplied":"1","supplied_type":"numeric"}]'))

test_that("test19.json (Bdsnummer 110 numeric) PASSES",
          expect_silent(convert_bds_individual(jtf[19])))

test_that("test20.json (missing Groepen) produces message",
           expect_message(convert_bds_individual(jtf[20]),
                          "Missing 'ClientGegevens$Groepen'",
                          fixed = TRUE))

test_that("test21.json (minimal data) WARNS",
          expect_message(convert_bds_individual(jtf[21]),
                         "Missing 'Contactmomenten'",
                         fixed = TRUE))

test_that("test22.json (range checking) PASSES",
          expect_s4_class(convert_bds_individual(jtf[22]), "individual"))

test_that("test23.json (multiple messages) produces message",
          expect_message(convert_bds_individual(jtf[23])))

test_that("test24.json (new DDI fields) PASSES",
          expect_silent(convert_bds_individual(jtf[24])))

fn  <- system.file("extdata", "smocc", "Laura_S.json", package = "jamestest")
js  <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Laura_S.json is silent with GA in days",
          expect_silent(convert_bds_individual(js)))
