\encoding{UTF-8}
\name{get_movie_comments}
\alias{get_movie_comments} 
\title{获取豆瓣电影的短评}
\description{获取豆瓣电影的短评.}
\usage{get_movie_comments(movieid,results=100,fresh=10,verbose=TRUE,...)}
\arguments{
  \item{movieid}{ 豆瓣电影的id号}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认20}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}

\value{一个data.frame，各列为：
  \item{author}{文章作者昵称}
  \item{author_uri }{作者豆瓣URI}
  \item{published }{短评发表时间}
  \item{votes }{短评被赞为有用的次数}
  \item{comment}{短评内容}
  \item{rating}{作者对电影的评分}
 
}
\note{每页获取短评个数为20,如果results不是20的倍数，则获取的实际影评数量是\code{ceiling(results/20)*20}.}
\author{
qxde01<\email{qxde01@gmail.com}>
}
\seealso{
 \code{\link{get_movie_reviews}}
}
\examples{
## http://movie.douban.com/subject/5308265/comments
\dontrun{x=get_movie_comments(movieid=5308265,results=100)}
}
\keyword{comment}
\keyword{movie}