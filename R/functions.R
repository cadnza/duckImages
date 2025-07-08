# Get function to robustly convert list to XML ----
.listToXml <- function(inst_list) {
    formatList <- function(x) {
        x <- lapply(
            x,
            function(a) {
                if (is.null(a)) {
                    warning(
                        "No results returned; DuckDuckGo is misbehaving. Try again later."
                    )
                    return(NA)
                }
                a <- as.list(a)
                if (is.null(names(a))) {
                    names(a) <- rep("x", length(a))
                }
                if (length(a) > 1) {
                    a <- formatList(a)
                }
                return(a)
            }
        )
        return(x)
    }
    inst_list <- list(root = inst_list)
    final <- xml2::as_xml_document(formatList(inst_list))
    return(final)
}

# Get function to search images ----
imageSearch <- function(
    searchTerm,
    page = 1,
    type = NA, #c("photo","clipart","gif","transparent","line"),
    lay = NA #c("Square","Tall","Wide")
) {
    # Set base URL ----
    duckBase <- "https://duckduckgo.com/"

    # Set image parameters ----
    duckImages <- "iax=images&ia=images&iar=images"

    # Validate arguments ----
    if (length(type) > 1 | length(lay) > 1) {
        stop("\"type\" and \"lay\" must each be of max length 1.")
    }

    # Format arguments as parameters ----
    formatString <- NULL
    if (!is.na(type)) {
        formatString <- trimws(
            paste(
                formatString,
                paste0("type:", type),
                collapse = ","
            )
        )
    }
    if (!is.na(lay)) {
        formatString <- trimws(
            paste(
                formatString,
                paste0("layout:", lay),
                collapse = ","
            )
        )
    }
    if (!is.null(formatString)) {
        formatString <- paste(
            c(
                paste0("iaf=", formatString),
                paste0("f=", formatString)
            ),
            collapse = "&"
        )
    }

    # Format standard parameters ----
    standard <- paste(c(duckImages, formatString), collapse = "&")

    # Get link for VQD extraction ----
    linkA <- utils::URLencode(
        glue::glue(
            "{duckBase}?q={searchTerm}&{standard}"
        )
    )

    # Get VQD ----
    htmlWorking <- xml2::read_html(linkA)
    vqd <- stringr::str_match_all(
        as.character(htmlWorking),
        "(?<=vqd=')[^']+"
    )[[1]][1, 1]

    # Get link for image search API ----
    link <- utils::URLencode(
        glue::glue(
            "{duckBase}i.js?q={searchTerm}&l=en-US&o=json&vqd={vqd}&f=,,,,,&p={page}&{standard}"
        )
    )

    # Call image search API ----
    h <- curl::new_handle()
    uAgent <- paste(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
        "AppleWebKit/537.36 (KHTML, like Gecko)",
        "Chrome/104.0.5112.102 Safari/537.36"
    )
    curl::handle_setopt(h, useragent = uAgent)
    curl::handle_setheaders(
        h,
        "User-agent" = uAgent,
        "authority" = "duckduckgo.com"
    )

    # Format image URLs ----
    tempCurlFile <- tempfile(fileext = ".json")
    canProceed <- TRUE
    tryCatch(
        curl::curl_download(link, destfile = tempCurlFile, handle = h),
        error = function(x) {
            canProceed <<- FALSE
        }
    )
    if (!canProceed) {
        return(NA)
    }
    resJson <- jsonlite::read_json(tempCurlFile)
    file.remove(tempCurlFile)
    resXml <- .listToXml(resJson)
    images <- xml2::xml_text(xml2::xml_find_all(resXml, "//image"))

    # Return ----
    return(images)
}
