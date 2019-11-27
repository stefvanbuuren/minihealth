throw_messages <- function(df) {
  if (nrow(df) == 0L) return()
  for (i in 1:nrow(df)) {
    st <- paste0("BDS ", format(df[i, 1L], width = 3, justify = "right")," (",
                 df[i, 2L], "): Supplied: ",
                 df[i, 4L], ", Supplied type: ",
                 df[i, 5L])
    message(st)
  }
}
