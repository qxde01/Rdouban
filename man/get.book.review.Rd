\encoding{UTF-8}
\name{get.book.review}
\alias{get.book.review}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
获取豆瓣图书的评论文章.
}
\description{
获取豆瓣图书的评论文章,部分使用了官方API V1.
}
\usage{
get.book.review(bookid,results=100,verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bookid}{ 豆瓣书籍的id号,一串数字,如20326626}
  \item{results}{获取评论的数量,默认results=100}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{一个data.frame，各列为：
 \item{title }{评论文章名称}
 \item{review_id }{评论文章的豆瓣ID}
 \item{review }{评论文章详细内容}
 \item{author}{文章作者昵称}
 \item{author_id }{作者豆瓣ID}
 \item{published }{文章发表时间}
 \item{ updated}{文章最后一次修改时间}
 \item{comments }{文章被评论的数量}
 \item{ useless}{文章被赞的次数}
 \item{votes }{文章被推荐次数}
 \item{rating }{作者对书籍的评分}
}

\author{
qxde01<\email{qxde01@gmail.com}>
}

\examples{

## http://book.douban.com/subject/2567698/reviews
\dontrun{x<-get.book.review(bookid=2567698,results=55)}
}

\keyword{comment}
\keyword{review}
\keyword{douban}
