opinion_change_D<-function(x1,x2,k,miu){
  # k 意见交换的阈值
  # miu 收敛参数，一般处于(0,1/2]
  if(abs(x1-x2)<k){
    move<-miu*abs(x1-x2)
    if(x1>x2){
      x1<-x1-move
      x2<-x2+move
    }else{
      x1<-x1+move
      x2<-x2-move
    }
  }
  return(c(x1,x2))
}
opinion_change_HK<-function(b){
  return(mean(b))
}