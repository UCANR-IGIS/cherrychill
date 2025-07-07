## THESE ARE SHINY UTILITY FUNCTIONS, MOSTLY TO CREATE CUSTOMIZED UI ELEMENTS

## Utility function to add an additional class to the outer-most div of a shiny-tag
shinytag_add_class <- function(x, additional_class) {
  if (!inherits(x, "shiny.tag")) stop("x should be of class shiny.tag")
  x$attribs$class <- paste(x$attribs$class, additional_class)
  x
}

