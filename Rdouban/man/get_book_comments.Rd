\encoding{UTF-8}
\name{get_book_comments}
\alias{get_book_comments}
\title{获取豆瓣图书的评论文章}
\description{获取豆瓣图书的评论文章.}
\usage{get_book_comments(bookid,n=50,...)
}

\arguments{
  \item{bookid}{ 豆瓣书籍的id号,一串数字,如20326626}\item{n}{获取评论的数量,默认n=50}
}

\value{一个列表list, 包括:
  \item{book_title}{一个字符串,书籍名称} \item{comments_amount}{一个整数,评论文章总数}\item{comment_info}{一个data.frame,包含8列:\code{titles}评论文章名称,\code{comments}评论文章内容,\code{time}评论发表时间,\code{authors}作者昵称, \code{rating}作者评分,\code{useful}该评论被赞为有用的数量,\code{unuseful}该评论被批评为无用的数量,\code{author_url}作者主页, \code{comments_url}该评论文章的主页}
}
\note{若实际短评数小于n，则获取的是实际数，若实际数，则获取到的数量是\code{ceiling(n/20)*20}.}

\author{qxde01<\email{qxde01@@gmail.com}>
}

\seealso{
 \code{\link{get_book_info}}
}
\examples{
\dontrun{get_book_comments(bookid='2120492',n=50)}
}
\keyword{comments}