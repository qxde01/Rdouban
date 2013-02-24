\encoding{UTF-8}
\name{get_movie_reviews}
\alias{get_movie_reviews}

\title{获取豆瓣电影的长篇影评
}
\description{获取豆瓣电影和电视剧的长篇影评.
}
\usage{get_movie_reviews(movieid=5308265,n=50,...)
}

\arguments{
  \item{movieid}{豆瓣电影和电视剧的ID,由数字组成的字符串,可通过每部电影或电视剧的豆瓣主页查询.}
  \item{n}{获取长篇影评的数量,默认n=50.}
}

\value{
\item{movie_title}{电影的名称}  
\item{reviews_amount}{电影影评的总数量}
\item{reviews_info}{影评信息,一个data.frame,共9列:
  \code{title}影评标题,
  \code{review}影评内容,
  \code{time}影评发表时间,
  \code{nickname}作者昵称,
  \code{rating}评分信息,
  \code{useful}影评被赞为有用的次数,
  \code{useful}影评被赞为无用的次数,
  \code{reviews_url}影评主页,
  \code{authors_url}作者主页.}  
}
\author{
qxde01<\email{qxde01@@gmail.com}>
}
\note{若实际影评数小于n,则获取的是实际数,若实际数大于n,则获取到的数量是\code{ceiling(n/20)*20}.}

\seealso{
 \code{\link{get_movie_comments}}
}
\examples{
## http://movie.douban.com/subject/5308265/reviews
\dontrun{get_movie_reviews(movieid=5308265,n=70)}
}

\keyword{review}
\keyword{comment}
