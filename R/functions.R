# Get function to robustly convert list to XML ----
.listToXml <- function(inst_list){
	formatList <- function(x){
		x <- lapply(
			x,
			function(a){
				if(is.null(a))
					return(NA)
				a <- as.list(a)
				if(is.null(names(a)))
					names(a) <- rep("x",length(a))
				if(length(a)>1)
					a <- formatList(a)
				return(a)
			}
		)
		return(x)
	}
	inst_list <- list(root=inst_list)
	final <- xml2::as_xml_document(formatList(inst_list))
	return(final)
}

# Get function to search images ----
imageSearch <- function(
		searchTerm,
		page=1,
		type=NA, #c("photo","clipart","gif","transparent","line"),
		lay=NA #c("Square","Tall","Wide")
){

	# Set base URL ----
	duckBase <- "https://duckduckgo.com/"

	# Set image parameters ----
	duckImages <- "iax=images&ia=images&iar=images"

	# Validate arguments ----
	if(length(type)>1|length(lay)>1)
		stop("\"type\" and \"lay\" must each be of max length 1.")

	# Format arguments as parameters ----
	formatString <- NULL
	if(!is.na(type))
		formatString <- trimws(
			paste(
				formatString,
				paste0("type:",type),
				collapse=","
			)
		)
	if(!is.na(lay))
		formatString <- trimws(
			paste(
				formatString,
				paste0("layout:",lay),
				collapse=","
			)
		)
	if(!is.null(formatString))
		formatString <- paste(
			c(
				paste0("iaf=",formatString),
				paste0("f=",formatString)
			),
			collapse="&"
		)

	# Format standard parameters ----
	standard <- paste(c(duckImages,formatString),collapse="&")

	# Get link for VQD extraction
	linkA <- utils::URLencode(
		paste0(
			duckBase,
			"?",
			paste(
				paste0("q=",searchTerm),
				standard,
				sep="&"
			)
		)
	)

	# Get VQD ----
	htmlWorking <- xml2::read_html(linkA)
	vqd <- stringr::str_match_all(
		as.character(htmlWorking),
		"(?<=vqd=').+(?=')"
	)[[1]][1,1]

	# Get link for image search API ----
	link <- utils::URLencode(
		paste0(
			duckBase,
			"i.js",
			"?",
			paste(
				paste0("q=",searchTerm),
				"l=ru-ru",
				"o=json",
				paste("vqd",vqd,sep="="),
				"f=,,,,,",
				paste("p",page,sep="="),
				standard,
				sep="&"
			)
		)
	)

	# Call image search API ----
	h <- curl::new_handle()
	curl::handle_setopt(h,useragent="DuckImageSearch")
	curl::handle_setheaders(
		h,
		"User-agent"="DuckImageSearch",
		"authority"="duckduckgo.com"
	)

	# Format image URLs ----
	tempCurlFile <- tempfile(fileext=".json")
	canProceed <- TRUE
	tryCatch(
		curl::curl_download(link,destfile=tempCurlFile,handle=h),
		error=function(x)
			canProceed <<- FALSE
	)
	if(!canProceed)
		return(NA)
	resJson <- jsonlite::read_json(tempCurlFile)
	file.remove(tempCurlFile)
	resXml <- .listToXml(resJson)
	images <- xml2::xml_text(xml2::xml_find_all(resXml,"//image"))

	# Return ----
	return(images)
}
