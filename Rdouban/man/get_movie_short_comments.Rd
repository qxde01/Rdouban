\encoding{UTF-8}
\name{get_movie_short_comments}
\alias{get_movie_short_comments}

\title{
获取豆瓣电影的短评}

\description{获取豆瓣电影和电视剧的短评.}

\usage{get_movie_short_comments(movieid,n=100,...)}

\arguments{
  \item{movieid}{豆瓣电影和电视剧的ID,由数字组成的字符串,可通过每部电影或电视剧的豆瓣主页查询.}
  \item{n}{获取短评的数量,默认n=100.}
}

\value{
\item{title}{电影的名称}  
\item{short_comments_amount}{电影短评的总数量}
\item{short_comments}{短评信息,一个data.frame,共5列:\code{comments}短评内容,\code{comments_time}短评发表时间,\code{useful}短评被赞为有用的次数,\code{authors}作者昵称,\code{authors_url}作者主页.}  
}

\note{若实际短评数小于n,则获取的是实际数,若实际数大于n,则获取到的数量是\code{ceiling(n/20)*20}.}

\author{
qxde01<\email{qxde01@@gmail.com}>
}

\seealso{
 \code{\link{get_movie_long_comments}}
}

\examples{
## http://movie.douban.com/subject/5308265/comments
\dontrun{get_movie_short_comments(movieid=5308265,n=70)}
}

\keyword{comment}

