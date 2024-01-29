# Wrapper function to allow mocking in tests
.readline <- function (...) base::readline(...)

# Similar to utils::menu(), but defaults to selecting everything, and allows
# comma separation and ranges (using colons or hyphens)
# Currently lacks column formatting, since strings are generally quite long
.menu <- function (choices)
{
    choices <- as.character(choices)
    nChoices <- length(choices)
    
    if (nChoices < 1L)
        return (integer(0))
    
    digits <- as.integer(floor(log10(nChoices)) + 1)
    numbers <- sprintf(paste0("%",digits,"d: "), seq_len(nChoices))
    
    cat(paste0("\n", numbers, choices))
    cat("\n\nType <Enter> to select everything, 0 for nothing, or indices separated by spaces or commas")
    selection <- .readline("\nSelection: ")
    
    if (selection == "")
        selection <- seq_len(nChoices)
    else if (selection == "0")
        selection <- integer(0)
    else
    {
        # Split into elements separated by commas or spaces, and resolve ranges
        parts <- unlist(strsplit(selection, "[, ]+", perl=TRUE))
        selection <- as.integer(unlist(lapply(parts, function (str) {
            str <- sub("(\\d)-(\\d)", "\\1:\\2", str, perl=TRUE)
            eval(parse(text=str))
        })))
    }
    
    return (selection)
}
