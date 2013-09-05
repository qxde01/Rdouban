\encoding{UTF-8}
\name{get_book_notes}
\alias{get_book_notes}
\title{获取豆瓣图书的读书笔记}
\description{获取豆瓣图书的读书笔记.}
\usage{get_book_notes(bookid,results=100,fresh=10,count=10,verbose=TRUE,...)}
\arguments{
  \item{bookid}{ 豆瓣书籍的id号,一串数字,如2567698}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认10}
  \item{count}{每页note_uri数量,默认10，最大10}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
\value{一个data.frame，各列为：
 \item{note_uri }{笔记主页URL}
 \item{title }{笔记标题}
  \item{published }{笔记发表时间}
 \item{author}{作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{note }{笔记详细内容}
 \item{rating }{作者对书籍的评分}
  \item{readers}{被阅读的次数}
  \item{collectors}{被收藏的次数}

}
\note{若results不是count的倍数，则获取到的数量是\code{ceiling(results/count)*count}.}
\author{
  <\email{qxde01@gmail.com}>
}

\seealso{\code{\link{get_book_reviews}}
}
\examples{
## http://book.douban.com/subject/1291204/annotation
\dontrun{out<-get_book_notes(bookid='1291204',results=50)}
}
\keyword{note}
\keyword{douban}
