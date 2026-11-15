.state <- new.env(parent=emptyenv())

# Read a line of user input. Interactive sessions read through the R console,
# which may be a GUI. Otherwise, a script run from a terminal (e.g., with
# Rscript) can still read from it directly; the connection is kept open, so
# that input typed ahead isn't lost between questions. Returns character(0) at
# the end of input, or if there is nobody to ask. Mocked for testing
.readline <- function (prompt = "")
{
    if (interactive())
        return (base::readline(prompt))
    
    cat(prompt)
    if (isatty(stdin()))
    {
        flush(stdout())
        if (is.null(.state$stdin))
            .state$stdin <- file("stdin", open="r")
        line <- readLines(.state$stdin, n=1L, warn=FALSE)
    }
    else
        line <- character(0)
    
    # Without an answer, nothing has ended the prompt's line
    if (length(line) == 0L)
        cat("\n")
    return (line)
}

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
    
    # Repeat until we get a valid answer
    repeat
    {
        # The end of the input is equivalent to <Enter>
        selection <- .readline("\nSelection: ")
        if (length(selection) == 0L)
            return (seq_len(nChoices))
        selection <- trimws(selection)
        
        if (selection == "")
            return (seq_len(nChoices))
        else if (selection == "0")
            return (integer(0))
        
        # Split into elements separated by commas and/or spaces, each of which
        # must be an index or a range of indices
        parts <- strsplit(selection, "[,\\s]+", perl=TRUE)[[1]]
        parts <- parts[parts != ""]
        matches <- regmatches(parts, regexec("^(\\d+)(?:[-:](\\d+))?$", parts, perl=TRUE))
        valid <- lengths(matches) > 0L
        if (length(parts) == 0L || !all(valid))
        {
            message("Invalid selection: ", paste(parts[!valid], collapse=", "))
            next
        }
        
        # Convert to integer bounds; overlong digit strings become NA
        bounds <- lapply(matches, function (match) {
            bounds <- suppressWarnings(as.integer(match[2:3]))
            if (match[3] == "") bounds[1] else bounds
        })
        if (!all(unlist(bounds) %in% seq_len(nChoices)))
        {
            message("Indices must be between 1 and ", nChoices)
            next
        }
        
        selection <- unlist(lapply(bounds, function (b) seq(b[1], b[length(b)])))
        return (unique(selection))
    }
}
