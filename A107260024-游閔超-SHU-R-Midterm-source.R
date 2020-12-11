# 2020/12/11(五), 109學年第一學期 資料科學應用 R期中考
#
# 學號:A107260024        姓名:游閔超 
#
# 本檔案為各題之程式碼檔，無執行結果


# ex1(a)
study<- function(x,y){
  # x<-c(13:17)
  # y<-c(8:12)
  a <-matrix(0, 25, 5)
  for(x in 13:17){
    for(y in 8:12){
      U <-sqrt(x)*sqrt(y)
      Tuition <- 400*x+600*y
      cat(x,y, tuition, U)
    }
     cat("\n")
  }
}
study()
data.frame(x,y, U, Tuition)
list(Eng.hr=x, Comp.hr=y, Tuition=Tuition, U=U)






library(readxl)
readxl_example()
#ex2(a)
xlsx_file<- "Score-109.xlsx"
excel_sheets(xlsx_file)
mydata<-read_excel(xlsx_file,sheet="score",na="NA",skip=1)
x2<-as.data.frame(head(mydata, 5))
y2<-as.data.frame(tail(mydata, 5))
x2
y2

#ex2(b)
mydata[is.na(mydata)] <- 0
ssl <- which(mydata[,2] < 60 & mydata[,3] < 60)
mydata[ssl,]

# ex2(c)
x <- sum(mydata[,2])/75
y <- sum(mydata[,3])/75
my.cor <-for(i in 1:75){
  kk <- (mydata[i,2] - x)*(mydata[i,3] - y)
  gg <- (mydata[i,2] - x)*2*0.5
  mm <- (mydata[i,3] - y)*2*0.5
  pp <- kk/(gg*mm)
  pp
}


# ex2(d)
cor(mydata[,2:3])















