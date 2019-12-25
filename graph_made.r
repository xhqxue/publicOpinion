graph_node<-function(num_node=1000,p_set=2,beta_need=0.2){
  # 建立网络----------------------------
  # 使用BA模型建立无标度网络，规模设置为1000，个数为3.
  # num_node 节点node的数量
  seed<-c(1:100)
  set.seed(seed[1])
  g1 <- sample_pa(num_node)
  set.seed(seed[2])
  g2 <- sample_pa(num_node)
  set.seed(seed[3])
  g3 <- sample_pa(num_node)
  
  
  #  计算度---------
  # 计算每个节点的in-degree
  # k 序列，每个graph的节点的in-degree详情
  k1<-get_in_degree(g1)
  k2<-get_in_degree(g2)
  k3<-get_in_degree(g3)
  
  # k_all ,节点的邻居
  k_all1<-get_in_degree(g1,mode = "all")
  k_all2<-get_in_degree(g2,mode = "all")
  k_all3<-get_in_degree(g3,mode = "all")
  k_all<-list(k_all1,k_all2,k_all3)
  k_all4<-list()
  for(i in seq(num_node)){
    n_o1<-as.numeric(k_all[[1]][[i]])
    n_o2<-as.numeric(k_all[[2]][[i]])
    n_o3<-as.numeric(k_all[[3]][[i]])
    k_all4[[i]]<-union(n_o1,union(n_o2,n_o3))
  }
  
  # d 矩阵，每个graph的节点的进度数量,d[i,j]为个体i在网络j的进度数量,
  # 最后一列为进度总数
  d<-matrix(rep(0,num_node*(3+1)), nrow = num_node, ncol = 4)
  d[,1]<-get_in_num(k1)
  d[,2]<-get_in_num(k2)
  d[,3]<-get_in_num(k3)
  d[,4]<-d[,1]+d[,2]+d[,3]
  
  # 分配属性--------
  # 给个体分配属性
  id_all<-matrix(rep(0,num_node*3),ncol = 3)
  set.seed(100)
  # beta
  id_all[,1]<-runif(num_node,(beta_need),(beta_need+0.2))
  set.seed(100)
  # omega
  id_all[,2]<-runif(num_node,pi/12,pi/2)
  set.seed(100)
  # delta
  id_all[,3]<-runif(num_node,pi/24,pi/3)
  
  # 看法兴趣--------
  # 看法以及感兴趣程度
  # time_step 实验的时间数，步数; 
  
  time_step<-500
  a_int<-10
  # a[i,j]为个体i在时间j对谣言的感兴趣程度
  a<-matrix(rep(0,time_step*num_node),ncol = time_step)
  # b[i,j]为个体i在时间j对谣言的观点，opinion
  b<-matrix(rep(0,time_step*num_node),ncol = time_step)
  # b_u[i]个体i的不确定性
  b_u<-runif(num_node,0,0.4)
  
  # 网络选择概率----------
  # 不同个体选择在哪个网络发布的概率,
  # p_net[i,j]为个体i在网络j发布的概率
  
  p_net<-matrix(rep(0,3*num_node),nrow = num_node,ncol = 3)
  p_net[,1]<-d[,1]/d[,4]
  p_net[,2]<-d[,2]/d[,4]
  p_net[,3]<-d[,3]/d[,4]
  
  # p_net2 区间概率
  p_net2<-t(apply(p_net,1,cumsum))
  
  # 发布概率--------
  # 发布的概率
  # p_send[i,j]是个体i在时间j发布的概率
  p_send<-matrix(rep(0,time_step*num_node),nrow = num_node)
  
  # 接受概率--------
  # 接受的概率
  # p_set为模型中的P,设定的概率参数
  # p_set<-2
  # p_acc1[i,j]为接受者i，发送者v的接受概率
  # p_acc1<-matrix(rep(0,num_node*num_node),nrow = num_node)
  # for(i in seq(num_node)){
  #   p_acc1[i,]<-(1/(1+(d[i,1]/d[,1])))*p_set
  # }
  p_acc1<-get_acc(num_node,d[,1],p_set)
  p_acc2<-get_acc(num_node,d[,2],p_set)
  p_acc3<-get_acc(num_node,d[,3],p_set)
  # p_acc为多个矩阵的集合，方便计算
  p_acc<-list(p_acc1,p_acc2,p_acc3)
  
  g_list<-list(g1=g1,g2=g2,g3=g3,k1=k1,k2=k2,k3=k3,k_all1=k_all1,k_all2=k_all2,k_all3=k_all3,k_all=k_all,k_all4=k_all4,d=d,id_all=id_all,time_step=time_step,a_int=a_int,a=a,b=b,b_u=b_u,p_net=p_net,p_net2=p_net2,p_send=p_send,p_acc1=p_acc1,p_acc2=p_acc2,p_acc3=p_acc3,p_acc=p_acc,num_node=num_node)
  
  return(g_list)
}