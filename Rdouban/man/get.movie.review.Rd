\encoding{UTF-8}
\name{get.movie.review}
\alias{get.movie.review}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
通过移动网页获取豆瓣电影的长篇评论文章.
}
\description{
通过移动网页m.douban.com获取豆瓣电影的长篇评论文章.
}
\usage{
get.movie.review(movieid,results=100,fresh=20,verbose=TRUE)
}

\arguments{
  \item{movieid}{ 豆瓣电影的id号}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认20}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{一个data.frame，各列为：
 \item{review_id }{评论文章的豆瓣ID}
 \item{title }{评论文章名称}
 \item{review }{评论文章详细内容}
 \item{author}{文章作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{published }{文章发表时间}
 \item{votes }{文章被推荐次数}
 \item{rating }{作者对电影的评分}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
qxde01<\email{qxde01@@gmail.com}>
}
\note{
连续获取会被豆瓣阻止,出现 Forbidden 错误.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## http://m.douban.com/movie/subject/11627047/reviews
\dontrun{longmen<-get.movie.review(movieid=11627047,fresh=20,results=300)}
}

\keyword{movie}
\keyword{review}
\keyword{douban}
