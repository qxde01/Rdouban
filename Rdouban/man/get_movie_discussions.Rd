\encoding{UTF-8}
\name{get_movie_discussions}
\alias{get_movie_discussions}
\title{获取关于某个豆瓣电影的话题讨论内容
}
\description{获取关于某个豆瓣电影的话题讨论内容.
}
\usage{
get_movie_discussions(movieid,n=100,verbose=TRUE,...)
}

\arguments{
  \item{movieid}{豆瓣电影或电视剧的ID,由数字组成的字符串,可通过每部电影或电视剧的豆瓣主页查询.}
  \item{n}{获取话题的数量,默认n=100.}
  \item{verbose}{当\code{verbose=True}时,显示函数的执行过程信息,\code{verbose=False}时不显示,默认为\code{True}.}
}

\value{一个data.frame,共7列:
  \item{title }{话题标题}
  \item{dicussion }{话题内容}
  \item{nickname }{发表话题的作者昵称}
  \item{time }{话题发表的时间}
  \item{useful }{被赞为有用的次数}
  \item{unuseful}{被赞为无用的次数}
  \item{discussion_url }{话题网址}
  
}

\note{若实际话题数小于n,则获取的是实际数,若实际数大于n,则获取到的数量是\code{ceiling(n/20)*20}.}

\author{
qxde01<\email{qxde01@@gmail.com}>
}


\seealso{
\code{\link{get_movie_comments,get_movie_reviews}}
}
\examples{
## http://movie.douban.com/subject/5308265/discussion/
\dontrun{get_movie_discussions(movieid=5308265,n=100)}
}

\keyword{discussion}
