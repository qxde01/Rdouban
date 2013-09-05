\encoding{UTF-8}
\name{get.book.info}
\alias{get.book.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{豆瓣图书基本信息官方API}
\description{ 获取豆瓣图书基本信息,包括作者、简介、豆瓣评分等.}
\usage{
get.book.info(bookid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{bookid}{豆瓣书籍的id号,一串数字,如20326626,可通过图书的豆瓣主页查询}
}

\value{
  \item{title}{书名}
  \item{author }{作者}
  \item{rating }{评分：最小值、评分人数、平均值、最大值}
  \item{summary }{内容简介}
  \item{tags }{一个data.frame,常用标签，两列分别是标签名称和标签频数}
  \item{href}{书籍主页网址}
  \item{image }{封面图片网址}
  \item{attribute }{书籍其他信息,比如出本社、价格、页数等}
}
\references{}
\author{
  <\email{qxde01@gmail.com}>
}

\examples{
\dontrun{x<-get.book.info(bookid='2567698')}
}
\keyword{book}
\keyword{douban}