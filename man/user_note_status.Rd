\encoding{UTF-8}
\name{user_note_status}
\alias{user_note_status}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
获取豆瓣用户的日记
}
\description{
获取豆瓣用户的日记. 
}
\usage{
user_note_status(userid,count=10,verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{userid}{豆瓣用户id}
  \item{count}{每页显示的日记篇数，默认10}
  \item{verbose}{若为\code{TRUE}则显示详细过程信息,否则不显示，默认\code{TRUE}}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{一个data.frame，各列为：

  \item{title }{日记的标题}
  \item{published}{日记发表时间}
  \item{note}{日记内容}
  \item{liked}{被喜欢的次数}
  \item{recommend}{被推荐的次数}
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
qxde01<\email{qxde01@gmail.com}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## http://www.douban.com/people/qxde01/notes
## 作者没发表过日记，没有任何内容
\dontrun{qxde<-user_note_status(userid="qxde01")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ douban }
\keyword{ note}% __ONLY ONE__ keyword per line
