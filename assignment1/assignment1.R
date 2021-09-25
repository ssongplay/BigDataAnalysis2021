################# 1차 온라인 과제 #################
################ 2019146018 송연주 #################
####### 쉬운 함수를 만들어 마약범죄 소탕하기! #######

# 범죄 현장 좌표 (x, y)
x = c(2, 5, 5, 5, 1, 5, 7, 4, 5, 6)
y = c(0, 4, 1, 2, 8, 9, 5, 2, 4, 1)

# 마약 거래상 (A, B, C)의 정보
col1 = c(1, 4, 8) # x좌표
col2 = c(5, 7, 1) # y좌표 
col3 = c(5, 1, 3) # 범죄 이력 

# 범죄현장 좌표(site)와 마약거래상의 정보(seller) 
site = data.frame(site=seq(10),x, y)
seller = data.frame(seller=seq(3),col1, col2, col3)
site = as.matrix(site)
seller = as.matrix(seller)

# 거래상(seller)과 거래장소(site)의 거리를 계산하는 함수
distance = function(seller, site){
  dist_mat = matrix(0, 10, 1)
  for(i in 1:10){
    # 유클리디안 거리
    temp = sqrt((seller-site[i,1:2])%*%t(t(seller-site[i,1:2])))
    dist_mat[i,1] = temp
  }
  return(dist_mat)
}

# 산출된 거리를 각각의 거래상의 과거 전과의 숫자로 나눔
result_A = distance(seller[1,2:3],site[,2:3])/seller[1,4]
result_B = distance(seller[2,2:3],site[,2:3])/seller[2,4]
result_C = distance(seller[3,2:3],site[,2:3])/seller[3,4]

# 거리를 전과의 숫자로 나눈 값 중 가장 작은 값을 찾는 함수
# 거래상들을 범죄 현장과 할당하여 return (A=1, B=2, C=3)
who = function(seller){
  who_mat = matrix(0, 10, 1)
   for(i in 1:10){
     if(result_A[i,1] < result_B[i,1]){
       if(result_A[i,1] < result_C[i,1])
         who_mat[i,1] = 1  #A
       else
         who_mat[i,1] = 3  #C
     }
     else{
       if(result_B[i,1] < result_C[i,1])
         who_mat[i,1] = 2  #B
       else
         who_mat[i,1] = 3  #C
     }
   }
  return(who_mat)
}

who(seller)

# x, y 좌표가 주어졌을 때, A, B, C중 어떤 거래상인지 찾아내는 함수
catch = function(x,y){
  catch_mat = matrix(0, 3, 1)
  xy_mat = matrix(c(x, y), nrow=1, ncol=2, byrow=T)
  # (x,y)와 각 거래상과의 거리를 전과 숫자로 나눈 값을 산출
  for(i in 1:3){
    temp = sqrt((seller[i,2:3]-xy_mat[,1:2])%*%t(t(seller[i,2:3]-xy_mat[,1:2])))/seller[i,4]
    catch_mat[i,1] = temp
  }
  # 가장 작은 값을 구하여 거래상 할당
  if(catch_mat[1,1] < catch_mat[2,1]){
    if(catch_mat[1,1] < catch_mat[3,1])
      result = 1  #A
    else
      result = 3  #C
  }
  else{
    if(catch_mat[2,1] < catch_mat[3,1])
      result = 2  #B
    else
      result = 3  #C
  }
  return(result)
}

#TEST 
catch(8,1)  # 할당 결과 : C
catch(1,5)  # 할당 결과 : A
catch(4,7)  # 할당 결과 : B
