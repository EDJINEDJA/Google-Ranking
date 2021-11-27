graphe = function(N){
  L = matrix(rbinom(N*N,1,prob = 0.5), ncol = N, nrow = N)
  for (i in 1:N){
    L[i,i] = rbinom(1,1,prob = 0.1)
  }
  for (i in 1:N){
    if (L[i,i]==1){
      for (k in 1:N){
        L[i,k] = 0
      }
      L[i,i]=1
    }
  }
  return(L)
}


#cette fonction nous renvoie notre structure en graphe avec les liens entre les pages, et avec possibilité de tomber sur une page sans issue


ponderation = function(A){
  N = length(A[1,])
  for (i in 1:N){
    A[i,] = A[i,]/sum(A[i,])
  }
  return(A)
}


#cette fonction nous renvoie une matrice a_i,j dont les coef correspondent à 1/(nombre de liens émis de la page i)

equi = function(N){
  matrix(rep(1/N,N),ncol = N, nrow = 1)
}

#vecteur d'equi probabilité entre les pages

vect_init = function(N){            #distribution au temps 0
  i = sample(1:N,1)
  v = rep(0,N)
  v[i] = 1
  return(v)
}



transition_gr = function(N,A,x,cte){          #transition du Google Ranking
  return(cte*equi(N)+(1-cte)*(x %*% A))
}



transition = function(A,x){           #transition basique
  return(x %*% A)
}



convergence = function(A,init,t){         #promenade aleatoire basique
  for (k in 1:t){
    X = transition(A,init)
    init = X
  }
  return(X)
}



convergence_gr = function(N,A,init,cte,t){          #promenade Google Ranking
  for (k in 1:t){
    X = transition_gr(N,A,init,cte)
    init = X
  }
  return(X)
}



####################################################
#CAS BASIQUE#


cas_basique = function(N,A,init,n,ylim){
  t = seq(1,n+1,1)
  S = matrix(rep(0,(n+1)*N),ncol = N)
  S[1,] = init
  for (k in 1:n){
    for (i in 1:N){
      S[k+1,i] = convergence(A,init,k)[i]
    }
  }
  plot(t,S[,1],type = "l",ylim =c(0,ylim), ylab = "Proba") 
  for (k in 2:N){
    lines(t,S[,k])
  }
  trou_noir = c()
  for (i in 1:N){
    if (A[i,i] == 1){
      trou_noir = append(trou_noir,c(i))
    }
  }
  if (is.null(trou_noir)){
    liste = list("matrice de transition" = A, "le graphe ne contient aucun trou noir" ,"distribution initiale"= init, "distribution finale" = S[n+1,])
    return(liste)
  }
  else{
    liste = list("matrice de transition" = A, "les pages suivantes sont des trous noirs" = trou_noir, "distribution initiale"= init, "distribution finale" = S[n+1,])
    return(liste)
  }
  
}



cas_basique(12,ponderation(graphe(12)),vect_init(12),30,ylim = 0.5)





####################################################################
#CAS GOOGLE RANKING#



cas_gr = function(N,A,init,cte,n,ylim){
  t = seq(1,n+1,1)
  S = matrix(rep(0,(n+1)*N),ncol = N)
  S[1,] = init
  for (k in 1:n){
    for (i in 1:N){
      S[k+1,i] = convergence_gr(N,A,init,cte,k)[i]
    }
  }
  plot(t,S[,1],type = "l",ylim =c(0,ylim), ylab = "Proba") 
  for (k in 2:N){
    lines(t,S[,k])
  }
  trou_noir = c()
  for (i in 1:N){
    if (A[i,i] == 1){
      trou_noir = append(trou_noir,c(i))
    }
  }
  if (is.null(trou_noir)){
    liste = list("matrice de transition" = A, "le graphe ne contient aucun trou noir" ,"distribution initiale"= init, "distribution finale" = S[n+1,])
    return(liste)
  }
  else{
    liste = list("matrice de transition" = A, "les pages suivantes sont des trous noirs" = trou_noir, "distribution initiale"= init, "distribution finale" = S[n+1,])
    return(liste)
  }
  
}


cas_gr(12,ponderation(graphe(12)),vect_init(12),0.15,30,ylim = 0.5)




###########################################

#EXEMPLE DU SITE CAS BASIQUE#

N =12
A = matrix(c(0,1,1,1,1,0,0,0,0,0,0,0, 1,0,1,0,0,0,0,0,0,0,0,0, 1,0,0,1,0,0,0,0,0,0,0,0, 1,1,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,1,1,1,0,0,0,0, 1,0,0,0,0,0,1,0,0,0,0,0, 0,0,0,0,1,0,0,0,0,0,0,0, 0,0,0,0,0,0,1,0,1,0,0,0, 0,0,0,0,1,0,0,0,0,1,1,1, 0,0,0,0,0,0,0,0,1,0,1,0, 0,0,0,0,0,0,0,0,1,0,0,1, 0,0,0,0,0,0,0,0,1,1,0,0), ncol = 12, byrow = TRUE)
A
B = ponderation(A)
B
init = c(0,0,0,0,0,0,1,0,0,0,0,0)
init

t = seq(1,31,1)
S = matrix(rep(0,372),ncol = 12)
S[1,] = init
for (k in 1:30){
  for (i in 1:12){
    S[k+1,i] = convergence(B,init,k)[i]
  }
}

plot(t,S[,1],type = "l",col="blue", ylim = c(0,0.2))
for (k in 1:12){
  lines(t,S[,k])
}
plot(t,S[,1],type = "l",col="blue", ylim = c(0,0.2))
lines(t,S[,2], col="red")
lines(t,S[,3], col="black")
lines(t,S[,4], col="yellow")
lines(t,S[,5], col="green")
lines(t,S[,6], col="cyan")
lines(t,S[,7], col="purple")
lines(t,S[,8], col="orange")
lines(t,S[,9], col="grey")
lines(t,S[,10], col="brown")
lines(t,S[,11], col="darkgreen")
lines(t,S[,12], col="darkred")


cas_basique(12,B,init,30,ylim = 0.5)




###########################################

#EXEMPLE DU SITE CAS GR#

N =12
cte=0.15
A = matrix(c(0,1,1,1,1,0,0,0,0,0,0,0, 1,0,1,0,0,0,0,0,0,0,0,0, 1,0,0,1,0,0,0,0,0,0,0,0, 1,1,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,1,1,1,0,0,0,0, 1,0,0,0,0,0,1,0,0,0,0,0, 0,0,0,0,1,0,0,0,0,0,0,0, 0,0,0,0,0,0,1,0,1,0,0,0, 0,0,0,0,1,0,0,0,0,1,1,1, 0,0,0,0,0,0,0,0,1,0,1,0, 0,0,0,0,0,0,0,0,1,0,0,1, 0,0,0,0,0,0,0,0,1,1,0,0), ncol = 12, byrow = TRUE)
A
B = ponderation(A)
B
init = c(1,0,0,0,0,0,0,0,0,0,0,0)
init

t = seq(1,31,1)
S = matrix(rep(0,372),ncol = 12)
S[1,] = init
for (k in 1:30){
  for (i in 1:12){
    S[k+1,i] = convergence_gr(N,B,init,cte,k)[i]
  }
}

plot(t,S[,1],type = "l",col="blue", ylim = c(0,0.2))
for (k in 1:12){
  lines(t,S[,k])
}
plot(t,S[,1],type = "l",col="blue", ylim = c(0,0.2))
lines(t,S[,2], col="red")
lines(t,S[,3], col="black")
lines(t,S[,4], col="yellow")
lines(t,S[,5], col="green")
lines(t,S[,6], col="cyan")
lines(t,S[,7], col="purple")
lines(t,S[,8], col="orange")
lines(t,S[,9], col="grey")
lines(t,S[,10], col="brown")
lines(t,S[,11], col="darkgreen")
lines(t,S[,12], col="darkred")


cas_gr(12,B,init,0.15,30,ylim = 0.5)




##############################################################################

#VITESSE DE CONVERGENCE#



vitesse = function(N,n,cte,nbr_test,cte_vitesse){
  S = matrix(rep(0,n*nbr_test),ncol = nbr_test)
  for (k in 1:nbr_test){
    A = ponderation(graphe(N))
    init = vect_init(N)
    mes_statio = convergence_gr(N,A,init,cte,t = 100)
    for (i in 1:n){
      norme = 0
      for (l in 1:N){
        norme = norme + abs(convergence_gr(N,A,init,cte,i)[l]-mes_statio[l])
      }
      S[i,k] = norme
    }
  }
  t = seq(1,n,1)
  if (nbr_test==1){
    plot(t,S[,1],type = "l",ylab = "Norme var tot", col = "black")
    y = (1-cte)^t
    lines(t,y,type = "l",col = "red", lty = 2)
    legend("topright",legend = c("simulation","(1-c)^t"), col = c("black","red"), lty = 1:2)
  }
  else{
    plot(t,S[,1],type = "l", ylab = "Norme var tot")
    for (k in 2:nbr_test){
      lines(t,S[,k])
    }
    y = cte_vitesse*(1-cte)^t
    lines(t,y,type = "l",col = "red", lty=2)
    legend("topright",legend = c("Simulations","cte*(1-c)^t"), col = c("black","red"), lty = 1:2)
  }
}

vitesse(20,30,0.15,10,2)

##############################################################################

#CAS OPTIMAL#


cas_teleport = function(N,A,const,n,init,ylim){
  trou_noir = c()
  for (i in 1:ncol(A)){
    if (A[i,i] == 1){
      A[i,] = rep(const/(ncol(A)-1),ncol(A))
      A[i,i] = 1-const
      trou_noir = append(trou_noir,c(i))
    }
  }
  t = seq(1,n+1,1)
  S = matrix(rep(0,(n+1)*N),ncol = N)
  S[1,] = init
  for (k in 1:n){
    for (i in 1:N){
      S[k+1,i] = convergence_gr(N,A,init,0.15,k)[i]
    }
  }
  plot(t,S[,1],type = "l",ylim =c(0,ylim), ylab = "Proba") 
  for (k in 2:N){
    lines(t,S[,k])
  }
  if (is.null(trou_noir)){
    liste = list("matrice de transition" = A, "le graphe ne contient aucun trou noir" ,"distribution initiale"= init, "distribution finale" = S[n+1,])
    return(liste)
  }
  else{
    liste = list("matrice de transition" = A, "les pages suivantes sont des trous noirs" = trou_noir, "distribution initiale"= init, "distribution finale" = S[n+1,])
    return(liste)
  }
}



cas_teleport(12,ponderation(graphe(12)),0.7,30,vect_init(12),ylim = 0.2)



#########################################################################################
#CAS OPTIMAL EX CNRS#

A = matrix(c(0,1,1,1,1,0,0,0,0,0,0,0,0, 1,0,1,0,0,0,0,0,0,0,0,0,0, 1,0,0,1,0,0,0,0,0,0,0,0,0, 1,1,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,1,1,1,0,0,0,0,0, 1,0,0,0,0,0,1,0,0,0,0,0,0, 0,0,0,0,1,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,1,0,1,0,0,0,0, 0,0,0,0,1,0,0,0,0,1,1,1,0, 0,0,0,0,0,0,0,0,1,0,1,0,0, 0,0,0,0,0,0,0,0,1,0,0,1,0, 0,0,0,0,0,0,0,0,1,1,0,0,1, 0,0,0,0,0,0,0,0,0,0,0,0,1), ncol = 13, byrow = TRUE)
A
B = ponderation(A)
B


cas_gr(13,B,vect_init(13),0.15,30,ylim = 0.5)
cas_basique(13,B,vect_init(13),30,ylim = 0.5)
cas_teleport(13,B,0.5,30,vect_init(13),ylim = 0.5)
