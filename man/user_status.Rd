\encoding{UTF-8}
\name{user_status}
\alias{user_status}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
获取豆瓣用户的说说
}
\description{
获取豆瓣用户的说说,最多能获取最近的200条
}
\usage{
user_status(usrid,results=200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usrid}{豆瓣用户ID}
  \item{results}{获取用户说说的数量,最多可获取最近200条}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

一个数据框(data.frame),各列为：
 \item{data_sid }{数据ID}
 \item{data_object_kind }{说说类别}
 \item{data_target_type}{说说来源类别}
 \item{txt }{说说、分享内容}
 \item{created_at}{说说发表时间}
 \item{saying }{说说内容}
 \item{reply }{回应的次数}
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
## http://www.douban.com/people/qxde01
\dontrun{qxde<-user_status(userid='qxde01',results=100)}
}
  
\keyword{ douban }


