\name{Gen3Submission-class}
\Rdversion{1.1}
\docType{class}
\alias{Gen3Submission-class}
\alias{Gen3Submission}

\title{Class \code{"Gen3Submission"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Gen3Submission")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{endpoint}:}{Object of class \code{character} ~~ }
    \item{\code{auth_provider}:}{Object of class \code{Gen3AuthHelper} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{query(query_txt, variables)}:}{ ~~ }
    \item{\code{export_node(program, project, node_type, fileformat, filename)}:}{ ~~ }
    \item{\code{export_record(program, project, uuid, fileformat, filename)}:}{ ~~ }
    \item{\code{delete_record(program, project, uuid)}:}{ ~~ }
    \item{\code{submit_record(program, project, json_parameter)}:}{ ~~ }
    \item{\code{get_graphql_schema()}:}{ ~~ }
    \item{\code{get_dictionary_all()}:}{ ~~ }
    \item{\code{get_dictionary_node(node_type)}:}{ ~~ }
    \item{\code{delete_program(program)}:}{ ~~ }
    \item{\code{delete_project(program, project)}:}{ ~~ }
    \item{\code{create_project(program, json_parameter)}:}{ ~~ }
    \item{\code{create_program(json_parameter)}:}{ ~~ }
    \item{\code{export_tsv(filename, output)}:}{ ~~ }
    \item{\code{export_json(filename, output)}:}{ ~~ }
  }
}
