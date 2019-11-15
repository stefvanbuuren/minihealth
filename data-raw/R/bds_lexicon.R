#' Create the BDS lexicon
#' @export
create_bds_lexicon <- function(){
 bds_lexicon <- data.frame(
            bdsnummer = c(19, 20, 62, 63, 66, 71, 82, 91, 110, 235, 245, 252,
                           238, 240, 510),
             description = c("Sex of child",
                             "Date of birth",
                             "Caretaker relation",
                             "Caretaker date of birth",
                             "Caretaker education",
                             "Caretaker birth country",
                             "Gestational age",
                             "Smoking during pregnancy",
                             "Birth weight",
                             "Length/height",
                             "Body weight",
                             "Head circumference",
                             "Height biological mother",
                             "Height biological father",
                             "Passive smoking"),
             expected = c("one of: 0, 1, 2, 3",
                          "yyyymmdd",
                          "one of: 01, 02, 03, 04, 05, 06, 07, 08, 98",
                          "yyyymmdd",
                          "one of: 01, 02, 03, 04, 05, 06, 07, 08, 98, 00",
                          "4-digit code",
                          "in days",
                          "one of: 1, 2, 99",
                          "in grammes",
                          "in milimeters",
                          "in grammes",
                          "in milimeters",
                          "in milimeters",
                          "in milimeters",
                          "one of: 01, 02, 03, 04")
            )
}
