\encoding{UTF-8}
\name{get.movie.comment}
\alias{get.movie.comment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
通过移动网页获取豆瓣电影的短评.
}
\description{
通过移动网页m.douban.com获取豆瓣电影的短评。
}
\usage{
get.movie.comment(movieid,results=100,fresh=20,verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{movieid}{ 豆瓣电影的id号}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认20}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{author}{作者昵称}
 \item{author_id }{作者豆瓣ID}
 \item{comment }{短评内容}
 \item{rating }{作者对电影的评分}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
qxde01<\email{qxde01@@gmail.com}>
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## http://m.douban.com/movie/subject/11627047/comments
\dontrun{cmt<-get.movie.comment(movieid=11627047,fresh=20,results=100,verbose=TRUE)}
}

\keyword{movie}
\keyword{comment}
\keyword{douban}

