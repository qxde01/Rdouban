\encoding{UTF-8}
\name{user_movie_status}
\alias{user_movie_status}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
豆瓣用户的观影信息
}
\description{
豆瓣用户的观影信息，包括一看影视记录、发表影评等。
}
\usage{
user_movie_status(userid,verbose=TRUE,front=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{userid}{豆瓣用户id}
  \item{verbose}{若为\code{TRUE}则显示详细过程信息,否则不显示，默认\code{TRUE}}
  \item{front}{\code{TRUE},则下载电影海报图片,否则不下载}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{一个list，各项为：

  \item{collect_tags }{\code{data.frame},已观看影视的标签}
  \item{collect_df}{\code{data.frame},已观看影视的信息}
  \item{do_tags}{\code{data.frame},正在观看影视的标签}
  \item{do_df}{\code{data.frame},正在观看影视的信息}
  \item{wish_tags}{\code{data.frame},想观看影视的标签}
  \item{wish_df}{\code{data.frame},想观看影视的信息}
  \item{reviews}{\code{data.frame},发表的影评,各列同\code{get_movie_reviews}}
  \item{collect_images}{\code{list},已观看影视的海报图片,分辨率为\code{60x80}}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
qxde01<\email{qxde01@gmail.com}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## http://movie.douban.com/people/qxde01
\dontrun{qxde<-user_movie_status(userid="qxde01")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ douban }
\keyword{ movie }% __ONLY ONE__ keyword per line
