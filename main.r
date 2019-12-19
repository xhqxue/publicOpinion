library(igraph)
source("functions.r")
# 建立网络----------------------------
# 使用BA模型建立无标度网络，规模设置为1000，个数为3.
# num_node 节点node的数量
num_node<-1000
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
id_all[,1]<-runif(num_node,0.2,0.8)
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
p_set<-2
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

# 传播过程----------
# 初始接受的点
set.seed(100)
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
  p_send[i,]<-get_p_send(i,0,time_step)
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
        p_send[ia,c((t+1):time_step)]<-get_p_send(ia,t,time_step)
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











