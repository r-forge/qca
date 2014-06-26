.onAttach <- function(...) {
    msg <- "Dusa, Adrian and Alrik Thiem (2014). QCA: A Package for Qualitative Comparative Analysis. R package version 1.1-3. URL http://CRAN.R-project.org/package=QCA"
    msg <- paste(strwrap(msg, indent=2, exdent=2), collapse="\n")
    packageStartupMessage("\nPlease cite the QCA package as:\n", msg, "\n\nA complete BibTeX reference is provided by:\n  citation(\"QCA\")\n")
}
