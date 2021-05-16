\name{imageSearch}
\alias{imageSearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DuckDuckGo Image Search}
\description{
Image search in DuckDuckGo.
}
\usage{
imageSearch(
	searchTerm,
	page=1,
	type=c("photo","clipart","gif","transparent","line"),
	lay=c("Square","Tall","Wide")
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{searchTerm}{
  The term to search.
  }
  \item{page}{
  The page to return (run an image search in DuckDuckGo for example).
  }
  \item{type}{
  The type of image to return.
  }
  \item{lay}{
  The image layout to return.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a character vector representing the URLs of returned images.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
\href{https://www.linkedin.com/in/jon-dayley-371398102}{Jon Dayley}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
imageSearch(
	"oranges",
	page=2,
	type="photo",
	lay="Wide"
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
