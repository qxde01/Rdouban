\encoding{UTF-8}
\name{get.music.info}
\alias{get.music.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
获取豆瓣音乐的专辑信息
}
\description{
获取豆瓣音乐的专辑信息,API V2接口。
}
\usage{
get.music.info(musicid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{musicid}{豆瓣音乐专辑ID号 }
}

\value{
  \item{title}{音乐专辑名称}
  \item{author }{作者}
  \item{rating }{评分：最小值、评分人数、平均值、最大值}
  \item{summary }{内容简介}
  \item{tags }{一个data.frame,常用标签，两列分别是标签名称和标签频数}
  \item{songs}{专辑所包括的歌曲名称}
  \item{href}{音乐专辑主页网址}
  \item{image }{封面图片网址}
  \item{attribute }{一个list,专辑的其他信息,比如出版公司、出版时间等}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  <\email{qxde01@@gmail.com}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
## http://music.douban.com/subject/3843530/
\dontrun{get.music.info(musicid='3843530')}
}
\keyword{music}
\keyword{douban}
