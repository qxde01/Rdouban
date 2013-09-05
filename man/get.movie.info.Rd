\encoding{UTF-8}
\name{get.movie.info}
\alias{get.movie.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
获取豆瓣电影的基本信息
}
\description{
获取豆瓣电影的基本信息,API V2接口。
}
\usage{
get.movie.info(movieid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{movieid}{豆瓣电影的ID,由数字组成,可通过每部电影的豆瓣主页查询.}
}

\value{
  \item{title}{电影名称}
  \item{author }{作者}
  \item{rating }{评分：最小值、评分人数、平均值、最大值}
  \item{summary }{内容简介}
  \item{tags }{一个data.frame,常用标签，两列分别是标签名称和标签频数}
  \item{href}{影视主页网址}
  \item{image }{封面图片网址}
  \item{attribute }{一个list,电影其他信息,比如演员、上映时间等}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  <\email{qxde01@gmail.com}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## http://movie.douban.com/subject/5308265/
\dontrun{get.movie.info(movieid='5308265')}
}
\keyword{movie}

