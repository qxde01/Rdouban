\encoding{UTF-8} 
\name{get_book_reviews}
\alias{get_book_reviews}
\title{获取豆瓣图书的评论文章}
\description{获取豆瓣图书的评论文章.}
\usage{get_book_reviews(bookid,results=100,fresh=10,count=10,verbose=TRUE,...)
}
\arguments{
  \item{bookid}{ 豆瓣书籍的id号,一串数字,如2567698}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认10}
  \item{count}{每页review_uri数量,默认25，最大25}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
\value{一个data.frame，各列为：
 \item{review_uri }{评论主页URL}
 \item{title }{评论文章名称}
  \item{published }{文章发表时间}
 \item{author}{文章作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{review }{评论文章详细内容}
 \item{rating }{作者对书籍的评分}
  \item{useful}{被评为useful的次数}
  \item{unuseful}{被评为unuseful的次数}  
}

\note{若results不是count的倍数，则获取到的数量是\code{ceiling(results/count)*count}.}
\author{qxde01<\email{qxde01@@gmail.com}>
}
\seealso{
 \code{\link{get_book_notes}}
}
\examples{
## http://book.douban.com/subject/2567698/reviews
\dontrun{out<-get_book_reviews(bookid=2567698,results=500)}
}
\keyword{comment}
\keyword{review}
\keyword{douban}
