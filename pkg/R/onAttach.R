.onAttach <- function(...) {
    
    meta <- packageDescription("QCA")
    year <- sub("-.*", "", meta$Date)
    note <- sprintf("R package version %s", meta$Version)
    
    msg <- paste("Dusa, Adrian and Alrik Thiem", sprintf("(%s).", year),
                 "QCA: A Package for Qualitative Comparative Analysis.",
                  paste(note, ".", sep = ""),
                  "URL: http://cran.r-project.org/package=QCA")
    msg <- paste(strwrap(msg, indent=2, exdent=2), collapse="\n")
    packageStartupMessage("\nPlease cite the QCA package as:\n", msg, "\n\nA complete BibTeX reference is provided by:\n  citation(\"QCA\")\n")
}
