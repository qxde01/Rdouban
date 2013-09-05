\encoding{UTF-8}
\name{user_book_viz}
\alias{user_book_viz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
豆瓣用户阅读统计的可视化函数
}
\description{
豆瓣用户阅读统计的可视化函数,统计阅读量、标签云图、封面拼图等。
}
\usage{
user_book_viz(x, YEAR = "2013", stopwords = stopwords, back = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
有函数\code{user_book_status}产生的数据
}
  \item{YEAR}{
年份，比如"2013",如果为NULL，则统计所有数据
}
  \item{stopwords}{
\code{Package:Rwordseg}分词时使用的中文停止词
}
  \item{back}{
如果为TRUE，则为图片添加背景图
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
产生9幅png格式的统计图
}
\references{
%% ~put references to the literature/web site here ~
}

\author{
qxde01<\email{qxde01@gmail.com}>
}

\examples{
## http://movie.douban.com/people/qxde01
\dontrun{
   qxde<-user_book_status(userid="qxde01")
   data(stopwords)
   user_book_viz(x=qxde,YEAR="2012",stopwords=stopwords,back=TRUE)
 }
}

\keyword{ douban }
\keyword{ book }%
