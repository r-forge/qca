.onAttach <- function(...) {
    
    meta <- packageDescription("QCA")
    year <- sub("-.*", "", meta$Date)
    title <- meta$Title
    note <- sprintf("R Package Version %s.", meta$Version)
    
    msg <- paste("Dusa, Adrian, and Alrik Thiem.", sprintf("%s.", year),
                 sprintf("%s.", title), note,
                  "URL: http://cran.r-project.org/package=QCA")
    
    msg <- paste(strwrap(msg, indent = 2, exdent = 2), collapse = "\n")
    
    packageStartupMessage("\nPlease cite the QCA package as:\n\n", msg, "\n\nA BibTeX entry is provided by:\n  citation(\"QCA\")\n")
}
