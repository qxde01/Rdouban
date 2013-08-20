\encoding{UTF-8}
\name{get_movie_reviews}
\alias{get_movie_reviews} 
\title{获取豆瓣电影的长篇影评
}
\description{获取豆瓣电影和电视剧的长篇影评.
}
\usage{get_movie_reviews(movieid,results=100,fresh=10,verbose=TRUE,...)
}
\arguments{
  \item{movieid}{豆瓣电影的id号}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认10}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}

}
\value{一个data.frame，各列为：
 \item{review_uri }{影评主页URL}
 \item{title }{评论文章名称}
  \item{published }{文章发表时间}
 \item{author}{文章作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{review }{评论文章详细内容}
 \item{rating }{作者对电影的评分}
  \item{useful}{被评为useful的次数}
  \item{unuseful}{被评为unuseful的次数}  
}
\author{
qxde01<\email{qxde01@@gmail.com}>
}
\note{每页获取影评URL的个数为20,如果results不是20的倍数，则获取的实际影评数量是\code{ceiling(results/20)*20}.}
\seealso{
 \code{\link{get_movie_comments}}
}
\examples{
## http://movie.douban.com/subject/5308265/reviews
\dontrun{x<-get_movie_reviews(movieid=5308265,results=50)}
}
\keyword{review}
\keyword{comment}
