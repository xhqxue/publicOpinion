get_in_degree<-function(g,n=1,mode="in"){
  k<-ego(g,n,seq(length(V(g))),mode = mode)
  # 以下注释为不含自身的进度
  # for (i in seq(length(V(g)))) {
  #   k[[i]]<-k[[i]][-which(k[[i]]==i)]
  # }
  return(k)
}
# 得到每个节点的进度节点的数量
get_in_num<-function(k){
  b<-NULL
  for(i in seq(k)){
    b<-c(b,length(k[[i]]))
  }
  return(b)
}
# 得到接受概率
get_acc<-function(num_node,d,p_set){
  p_acc<-matrix(rep(0,num_node*num_node),nrow = num_node)
  for(i in seq(num_node)){
    p_acc[i,]<-(1/(1+(d[i]/d)))*p_set
  }
  return(p_acc)
}

# 得到个体在接受rumor后的发布概率
get_p_send<-function(i,t,time_step,id_all){
  # 自接受之日起，之后的时间
  if(t==0.5){
    t=0
  }
  all_t<-c((t+1):time_step)-t
  p<-abs(exp(-1*id_all[i,1]*all_t)*sin(id_all[i,2]*all_t+id_all[i,3]))
  return(p)
}
# 使用的ra模型
ra_mode<-function(op_1,op_2,u_1,bg){
  if(abs(op_1-op_2)>u_1){
    return(op_1)
  }else{
    return(op_1+bg*abs(op_1-op_2))
  }
}
op_psum<-function(m){
  return(sum(m[which(m>0)]))
}
op_nsum<-function(m){
  return(sum(m[which(m<0)]))
}
op_pnum<-function(m){
  return(length(which(m>0)))
}
op_nnum<-function(m){
  return(length(which(m<0)))
}





