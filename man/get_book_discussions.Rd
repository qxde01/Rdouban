\encoding{UTF-8}
\name{get_book_discussions} 
\alias{get_book_discussions}
\title{获取豆瓣图书的话题讨论内容
}
\description{获取豆瓣图书话题讨论内容.
}
\usage{
get_book_discussions(bookid,results=100,fresh=10,count=20,verbose=TRUE)
}
\arguments{
  \item{bookid}{ 豆瓣书籍的id号,一串数字,如2567698}
  \item{results}{获取话题讨论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认10}
  \item{count}{每页discussion_uri数量,默认20，最大20}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
\value{一个data.frame,各列为:
 \item{discussion_uri }{话题主页URL}
 \item{title }{话题标题题}
  \item{published }{话题发表的时间}
 \item{author}{作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{discussion }{话题详细内容}
  \item{useful }{被赞为有用的次数}
  \item{unuseful}{被评为无用的次数}
}
\note{}
\author{
qxde01<\email{qxde01@gmail.com}>
}
\seealso{

}
\examples{
## http://book.douban.com/subject/1291204/discussion/
\dontrun{b<-get_book_discussions(bookid=1291204,results=100)}
}
\keyword{discussion}
