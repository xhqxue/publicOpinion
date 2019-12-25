

main<-function(k1){
  # 传播过程----------
  # 初始接受的点
  
  num_node<-k1$num_node
  b<-k1$b
  b_u<-k1$b_u
  a<-k1$a
  a_int<-k1$a_int
  d<-k1$d
  p_send<-k1$p_send
  p_net2<-k1$p_net2
  k_all<-k1$k_all
  k_all4<-k1$k_all4
  p_acc<-k1$p_acc
  time_step<-k1$time_step
  id_all<-k1$id_all
  
  # set.seed(100)
  kk<-sample(seq(num_node),size = 10)  
  # 初始成员的观点opinion
  b[kk,1]<-runif(10,-1,1)
  # kk<-c(1,2,3,4,10,14,56,108)
  # acc_i   个体是否接受rumor,0为不接受，
  #        0.5为初始点，其他数字为接收时间点
  acc_i<-rep(0,num_node)
  acc_i[kk]<-0.5
  # acc_n 接受的次数
  acc_n<-rep(0,num_node)
  acc_n[kk]<-1
  # 个体是否发布，0为未发布，其他数字为发布的时间点
  spend_i<-rep(0,num_node)
  # 得到初始节点之后时间点的发布概率
  for(i in kk){
    p_send[i,]<-get_p_send(i,0,time_step,id_all)
  }
  # sender_t[t] t时间的传播者数量
  sender_t<-rep(0,time_step)
  
  for(t in seq(time_step-10)){
    # 接受谣言的个体
    ra_i<-which(acc_i>0)
    # # 未发布谣言的个体
    # us_i<-which(spend_i==0)
    us_i<-seq(num_node)
    # 取交集，得到发布的个体
    spend_t<-intersect(ra_i,us_i)
    # 发布者数量的计数
    s_t<-0
    for(ii in spend_t){
      # 是否发布
      # browser()
      if(runif(1)<p_send[ii,t]){
        s_t<-s_t+1
        spend_i[ii]<-t
        # 选取网络层数
        s_net<-which(p_net2[ii,]>runif(1))[1]
        # 收集友邻
        aaa<-as.numeric(k_all[[s_net]][[ii]])
        # 随机数
        pp<-runif(length(aaa))
        # 接受rumor的个体
        # aci<-which(pp<p_acc[[s_net]][aaa,ii])
        aci<-aaa[which(pp<p_acc[[s_net]][aaa,ii])]
        ra_ii<-which(acc_i>0)
        # 接受的次数
        acc_n[aci]<-acc_n[aci]+1
        acc_i[setdiff(aci,ra_ii)]<-t
        for(ia in setdiff(aci,ra_ii)){
          p_send[ia,c((t+1):time_step)]<-get_p_send(ia,t,time_step,id_all)
        }
      }
    }
    sender_t[t]<-s_t
    # 得到观点opinion
    if(t>1){
      ra_ii<-which(acc_i>0)
      ra_it<-which(acc_i<t)
      ra_ii<-intersect(ra_ii,ra_it)
      for (i_o in ra_ii) {
        # 计算得到所有的邻居
        # n_o1<-as.numeric(k_all[[1]][[i_o]])
        # n_o2<-as.numeric(k_all[[2]][[i_o]])
        # n_o3<-as.numeric(k_all[[3]][[i_o]])
        # n_o<-union(n_o1,union(n_o2,n_o3))
        n_o<-k_all4[[i_o]]
        # 邻居且接受rumor的
        n_o<-intersect(n_o,ra_ii)
        if(b[i_o,(t-1)]==0){
          b[i_o,t]<-id_all[i_o,1]*runif(1,-1,1)+(1-id_all[i_o,1])*mean(b[n_o,(t-1)])
          # b[i_o,t]<-ifelse(runif(1)<(1/(1+id_all[i_o,1])), ifelse(runif(1)<0.5,max(b[n_o,(t-1)]),min(b[n_o,(t-1)])), mean(b[n_o,(t-1)])) 
          # b[i_o,t]<-mean(b[n_o,(t-1)])/(1+id_all[i_o,1]) 
        }else{
          # n_oo<-intersect(as.numeric(k_all[[1]][[i_o]]),ra_ii)
          # op_now<-sum(p_acc[[1]][i_o,n_oo]*b[i_o,(t-1)])
          # n_oo<-intersect(as.numeric(k_all[[2]][[i_o]]),ra_ii)
          # op_now<-sum(p_acc[[1]][i_o,n_oo]*b[i_o,(t-1)])+op_now
          # n_oo<-intersect(as.numeric(k_all[[3]][[i_o]]),ra_ii)
          # op_now<-sum(p_acc[[1]][i_o,n_oo]*b[i_o,(t-1)])+op_now
          # b[i_o,t]<-b[i_o,(t-1)]+op_now
          # RA模型
          b[i_o,t]<-ra_mode(b[i_o,(t-1)],mean(b[n_o,(t-1)]),b_u[i_o],id_all[i_o,1])
          # b[i_o,t]<-ifelse(runif(1)<(1/(1+id_all[i_o,1])), ifelse(runif(1)<0.5,max(b[n_o,(t-1)]),min(b[n_o,(t-1)])), mean(b[n_o,(t-1)]))
          # b[i_o,t]<-id_all[i_o,1]*b[i_o,(t-1)]+(1/id_all[i_o,1])*mean(b[n_o,(t-1)])
        }
      }
    }
  }
  
  s_data1<-data.frame(time=c(1:time_step),send_number=sender_t[1:time_step])
  bb<-sort(unique(acc_i))
  if(bb[1]==0){
    bb<-bb[-1]
  }
  bc<-NULL
  for(i in seq(bb)){
    bc<-c(bc,length(which(acc_i==bb[i])))
  }
  bb[1]<-0
  bb1<-c(1:time_step)
  bb2<-rep(0,time_step)
  for(i in seq(bb)){
    bb2[which(bb1==bb[i])]<-bc[i]
  }
  # s_data1<-cbind(s_data1,acc_number=bb2)
  bb3<-cumsum(bb2)
  # s_data1<-cbind(s_data1,acc_number=bb2,acc_number_all=bb3)
  
  a<-p_send*a_int
  # a_all[t,i] 网络层i在时间t的人气值
  a_all<-matrix(rep(0,time_step*4),ncol = 4)
  for (i in c(1,2,3)) {
    for(ii in seq(time_step)){
      a_all[ii,i]<-sum(a[,ii]*d[,i])
    }
  }
  a_all[,4]<-a_all[,1]+a_all[,2]+a_all[,3]
  # s_data1<-cbind(s_data1,acc_number=bb2,acc_number_all=bb3,popular=a_all[,4],popular1=a_all[,1],popular2=a_all[,2],popular3=a_all[,3])
  op_p<-apply(b, 2, op_psum)
  op_n<-apply(b, 2, op_nsum)
  op_pn<-apply(b, 2, op_pnum)
  op_nn<-apply(b, 2, op_nnum)
  op_b<-colSums(b)
  
  s_data1<-cbind(s_data1,acc_number=bb2,acc_number_all=bb3,popular=a_all[,4],popular1=a_all[,1],popular2=a_all[,2],popular3=a_all[,3],op_b,op_p,op_n,op_pn,op_nn)
  return(s_data1)
}








