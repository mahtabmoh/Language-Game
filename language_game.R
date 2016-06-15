# **************************************************
# Language game
# Author: Mahtab Mohammadi
# Date: May 1, 2016
# Based on:
# https://www.staff.ncl.ac.uk/daniel.nettle/ca1.pdf
# *************************************************




setwd("//Users//MMoh//Documents//Runs")
rm(list=ls())

library(knitr)
library(openssl)
library(MASS)
library(ggplot2)

#*****************
# Game Setup
#*****************

n=100
wealth <- 50
memspan<- 5 # Memory span
dial_change_rate <- 1 # The probability to update the dialect
wealth.reset <- FALSE # A logical variable to indicate whether the wealth of the offspring should be reset or not
repr_rate <- 20 # The toll of replications and deaths
Beta <- 1.029 # Decay factor
weight = c(n/4,n/4,n/4,n/4) # Weighting the strategies

give = FALSE # A logical to inform agents of the previous act of the opponent
receive = FALSE ##

# Pay-offs
T=2
R=1
P=0
S=-1
action <- c("Gift","noGift")


#Payoffs of player 1
payoff.mat.alter <- matrix(c(R,S,T,P),nrow=2,ncol=2)
rownames(payoff.mat.alter) <- c("Gift","NoGift")
colnames(payoff.mat.alter) = rownames(payoff.mat.alter)

#Payoffs of player 2
payoff.mat.ego <- matrix(c(R,S,T,P),nrow=2,ncol=2)
rownames(payoff.mat.ego) <- c("Gift","NoGift")
colnames(payoff.mat.ego) = rownames(payoff.mat.ego)

#Visualization of payoff table (package knitr) for latex
pair <- function(x,y) sprintf("(%d,%d)", x,y)
all_pairs <- c(pair(R,R), pair(S,T), pair(T,S), pair(P,P))
payoff.mat <- matrix(all_pairs, nrow=2)
dimnames(payoff.mat)[[1]] <- c("Gift","NoGift")
dimnames(payoff.mat)[[2]] = dimnames(payoff.mat)[[1]]
kable(payoff.mat, format="latex", align='c')

#*********************
# Functions and 
# some main operations
#*********************

# Generate the population assigning 5 attributes to each member:
# 1. Wealth
# 2. Dialect
# 3. Strategy (weighted)
# 4. Memory span
# 5. Position
generate.organism <- function(pos=0){
	dialect = sample(rep(1:6, each=6), size=6, replace=FALSE)
	strat <- c("CHEAT","COOP","POLYGLOT","MIMIC")
	strategy = sample(strat,size= 1, prob=weight,replace=FALSE)
	#Generate
	organism <- list(wealth,dialect,strategy,memspan,pos)
	names(organism) <- c("wealth","dial","strat", "memspan", "pos")
	return(organism)
}


update.generation <- function(repr_rate){
	# Construct a vector of wealths for evaluating the performance of agents
	wealth.vect <<- NA
	wealth.vect <<- vector("numeric", length=length(organism))
	for (i in 1:length(organism)){
		if(length(organism[[i]]$wealth)!=0){
			wealth.vect[i] <<- organism[[i]]$wealth
		}
	}
	#create a copy of wealth vector in order to extract the maximum values' index
	wealth.vect.cmax <<- rep(wealth.vect)
	max.pos <<- NA
	max.pos <<- vector("numeric",repr_rate)
	for (x in seq(repr_rate)){
		max.ind <<- which.max(wealth.vect.cmax)
		if (length(max.ind) != 0){
			max.pos[x] <<- max.ind
			wealth.vect.cmax[max.pos[x]]= NA
		}
	}
	#create a copy of wealth vector in order to extract the minimum values' index
	wealth.vect.cmin <<- rep(wealth.vect)
	min.pos <<- NA
	min.pos <<- vector("numeric",repr_rate)
	for (x in seq(repr_rate)){
		min.ind <<- which.min(wealth.vect.cmin)
		if (length(min.ind) != 0){
			min.pos[x] <<- min.ind
			wealth.vect.cmin[min.pos[x]]= NA
		}
	}
	
	# Eliminate organisms in the minimum positions
	dead.organisms <<- vector("list", repr_rate)
	for (i in 1:repr_rate){
		dead.organisms[[i]] <<- organism[[min.pos[i]]]
	}
	#Replicate the organisms in the maximum positions
	for (i in seq(repr_rate)){
		organism[[min.pos[i]]] <<- rep(organism[[max.pos[i]]], 1)
		organism[[min.pos[i]]]$pos <<- sample(min.pos, 1, replace=FALSE)
		if (wealth.reset == TRUE){
			organism[[min.pos[i]]]$wealth = 50
		}
	}
}


update.wealth <- function(i){
	#Update the wealth of population at the end of each cycle 
	#To reflect seasonal and random fluctuations in the supply of resources
	change.amount <<- sample(seq(-4, 4, 1),1)
	organism[[i]]$wealth <<- organism[[i]]$wealth + change.amount
}

update.dialect <- function(i){
	# Dialect update probability factor
	# This probability is set to 1%
	dial_prob = sample(c(1:100), 1)
	if (dial_prob <= dial_change_rate){
		dial <<- organism[[i]]$dial
		random.pos = sample(c(1:6),1)
		dial[random.pos] = sample(c(1:50), 1, replace=FALSE)
		organism[[i]]$dial <<- dial
	}
}

#Encounter probability
encounter.mat <- function(n, Beta){
	#Encounter probability
	W <- matrix (0, nrow=n, ncol=n)
	encounter <<- matrix(-1,n, n)
	#At time zero
	for (j in 1:n){
		for (k in 1:n){
			organism[[j]]$pos <<- j
			organism[[k]]$pos <<- k
			W[j,k] <- Beta/(Beta**abs(j-k))
			W[j,k] <- W[j,k]/max(W)
			encounter[j,k] <<- rbinom(1, 1, W[j,k])
		}
	}
	#Symmetrize the encounter matrix
	encounter_min <<- encounter*t(encounter)
}
#*********************
# Strategies encounters 
#*********************

CHEAT.vs.COOP <- function(i, j, iters){
	if (iters == 1){
		# Cooperate only at first encounter
		organism[[i]]$wealth <<-  organism[[i]]$wealth + T
		organism[[j]]$wealth <<- organism[[j]]$wealth + S
	} 
	else if (iters > memspan){
		# Cooperate at the moment when the number of iterations reaches the memory span
		organism[[i]]$wealth <<- organism[[i]]$wealth + T
		organism[[j]]$wealth <<- organism[[j]]$wealth + S
	}
}

COOP.vs.CHEAT <- function(i,j, iters){
	if (iters == 1){
			organism[[j]]$wealth <<-  organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
		} 
	else {
		if (iters > memspan){
			organism[[j]]$wealth <<- organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
		}
	}	
}

CHEAT.vs.POLYGLOT <- function( i, j){
	# if ployglot finds a model in the dialect of the opponent, it gives
	if (length(unique(organism[[i]]$dial)) <= 2){
		organism[[i]]$wealth <<- organism[[i]]$wealth + T
		organism[[j]]$wealth <<- organism[[j]]$wealth + S
		# The change in the dialect of polyglot at each exchange
		update.dialect(j)
	}
}

POLYGLOT.vs.CHEAT <- function( i, j){
	# if ployglot finds a model in the dialect of the opponent, it gives
	if (length(unique(organism[[j]]$dial)) <= 2){
		organism[[j]]$wealth <<- organism[[j]]$wealth + T
		organism[[i]]$wealth <<- organism[[i]]$wealth + S
		# The change in the dialect of polyglot at each exchange
		update.dialect(i)
	}
}

CHEAT.vs.MIMIC <- function( i, j){
	# Actually nothing happens
	# Function constructed only for demonstration
	break
}

MIMIC.vs.CHEAT <- function( i, j){
	# Actually nothing happens
	break
}

COOP.vs.POLYGLOT <- function( i, j, iters){
	if (iters == 1){
		organism[[j]]$wealth <<- organism[[j]]$wealth + T
		organism[[i]]$wealth <<- organism[[i]]$wealth + S
		organism[[j]]$dial <<- organism[[i]]$dial
		# if ployglot finds a model in the dialect of the opponent, it gives
		if (length(unique(organism[[i]]$dial)) <= 2){
			organism[[i]]$wealth <<- organism[[i]]$wealth + T
			organism[[j]]$wealth <<- organism[[j]]$wealth + S
			# Inform coop
			give = TRUE
			# The change in the dialect of polyglot at each exchange
		}	
		else{
			give = FALSE
		}
		update.dialect(j)
	} 
	else {
		if (iters > memspan){
			#!!!! <<- à mentionner dans le rapport
			organism[[j]]$wealth <<- organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
			organism[[j]]$dial <<- organism[[i]]$dial
			# if ployglot finds a model in the dialect of the opponent, it gives
			if (length(unique(organism[[i]]$dial)) <= 2){
				organism[[i]]$wealth <<- organism[[i]]$wealth + T
				organism[[j]]$wealth <<- organism[[j]]$wealth + S
				give = TRUE
			}
			else{
				give = FALSE
			}
			# The change in the dialect of polyglot at each exchange
			update.dialect(j)			
		}
		else{
			if (give == TRUE){
				organism[[j]]$wealth <<- organism[[j]]$wealth + T
	            	organism[[i]]$wealth <<- organism[[i]]$wealth + S
				organism[[j]]$dial <<- organism[[i]]$dial
				# The change in the dialect of polyglot at each exchange
				update.dialect(j)
			}
			if (length(unique(organism[[i]]$dial)) <= 2){
				organism[[i]]$wealth <<- organism[[i]]$wealth + T
				organism[[j]]$wealth <<- organism[[j]]$wealth + S
				give = TRUE
				# The change in the dialect of polyglot at each exchange
				update.dialect(j)
			}
			else{
				give = FALSE
			}
		}
	}
}

POLYGLOT.vs.COOP <- function( i, j, iters){
	if (iters == 1){
		organism[[i]]$wealth <<- organism[[i]]$wealth + T
		organism[[j]]$wealth <<- organism[[j]]$wealth + S
		organism[[i]]$dial <<- organism[[j]]$dial
		# if ployglot finds a model in the dialect of the opponent, it gives
		if (length(unique(organism[[j]]$dial)) <= 2){
			organism[[j]]$wealth <<- organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
			give = TRUE
		}	
		else{
			give = FALSE
		}
		# The change in the dialect of polyglot at each exchange
		update.dialect(i)
	} 
	else {
		if (iters > memspan){
			organism[[i]]$wealth <<- organism[[i]]$wealth + T
			organism[[j]]$wealth <<- organism[[j]]$wealth + S
			organism[[i]]$dial <<- organism[[j]]$dial
			# if ployglot finds a model in the dialect of the opponent, it gives
			if (length(unique(organism[[j]]$dial)) <= 2){
				organism[[j]]$wealth <<- organism[[j]]$wealth + T
				organism[[i]]$wealth <<- organism[[i]]$wealth + S
				give = TRUE
			}
			else{
				give = FALSE
			}
			# The change in the dialect of polyglot at each exchange
			update.dialect(i)			
		}
		else{
			if (give == TRUE){
				organism[[i]]$wealth <<- organism[[i]]$wealth + T
				organism[[j]]$wealth <<- organism[[j]]$wealth + S
				organism[[i]]$dial <<- organism[[j]]$dial
				# The change in the dialect of polyglot at each exchange
				update.dialect(i)
			}
			if (length(unique(organism[[j]]$dial)) <= 2){
				organism[[j]]$wealth <<- organism[[j]]$wealth + T
				organism[[i]]$wealth <<- organism[[i]]$wealth + S
				give = TRUE
				# The change in the dialect of polyglot at each exchange
				update.dialect(i)
			}
			else{
				give = FALSE
			}
		}
	}
}

POLYGLOT.vs.MIMIC <- function( i, j){
	# if ployglot finds a model in the dialect of the opponent, it gives
	if (length(unique(organism[[j]]$dial)) <=2){
		organism[[j]]$wealth <<- organism[[j]]$wealth + T
		organism[[i]]$wealth <<- organism[[i]]$wealth + S
		organism[[j]]$dial <<- organism[[i]]$dial
		# The change in the dialect of polyglot at each exchange
		update.dialect(i)
		# MIMIC changed its dialect to that of its benefactor when receives gift
	}
}

MIMIC.vs.POLYGLOT <- function( i, j){
	# if ployglot finds a model in the dialect of the opponent, it gives
	if (length(unique(organism[[i]]$dial)) <= 2){
		organism[[i]]$wealth <<- organism[[i]]$wealth + T
		organism[[j]]$wealth <<- organism[[j]]$wealth + S
		organism[[i]]$dial <<- organism[[j]]$dial
		# The change in the dialect of polyglot at each exchange
		update.dialect(j)
		# MIMIC changed its dialect to that of its benefactor when receives gift
	}	
}

COOP.vs.MIMIC <- function( i, j, iters){
	# Same structure as COOP.vs.CHEAT
	if (iters == 1){
			organism[[j]]$wealth <<-  organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
			#MIMIC adopts COOP's dialect
			organism[[j]]$dial <<- organism[[i]]$dial
		} 
	else {
		if (iters > memspan){
			organism[[j]]$wealth <<- organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
			organism[[j]]$dial <<- organism[[i]]$dial
		}
	}
}

MIMIC.vs.COOP <- function( i, j, iters){
	if (iters == 1){
			organism[[i]]$wealth <<-  organism[[i]]$wealth + T
			organism[[j]]$wealth <<- organism[[j]]$wealth + S
			organism[[i]]$dial <<- organism[[j]]$dial
		} 
	else {
		if (iters > memspan){
			organism[[i]]$wealth <<-  organism[[i]]$wealth + T
			organism[[j]]$wealth <<- organism[[j]]$wealth + S
			organism[[i]]$dial <<- organism[[j]]$dial
		}
	}
}

POLYGLOT.vs.POLYGLOT <- function( i, j){
	# if ployglot finds a model in the dialect of the opponent, it gives
	if (length(unique(organism[[j]]$dial)) <= 2){
		# if ployglot finds a model in the dialect of the opponent, it gives
		if (length(unique(organism[[i]]$dial)) <= 2){
			organism[[i]]$wealth <<- organism[[i]]$wealth + T
			organism[[j]]$wealth <<- organism[[j]]$wealth + S
			organism[[j]]$wealth <<- organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
			# Both agents will adapt their opponent's dialect
			dial.rep <<- organism[[j]]$dial
			organism[[j]]$dial <<- organism[[i]]$dial
			organism[[i]]$dial <<- dial.rep
		}
		else{
			organism[[j]]$wealth <<- organism[[j]]$wealth + T
			organism[[i]]$wealth <<- organism[[i]]$wealth + S
			organism[[j]]$dial <<- organism[[i]]$dial
		}
		# The change in the dialect of polyglot at each exchange
		update.dialect(i)
		# The change in the dialect of polyglot at each exchange
		update.dialect(j)
	}
	else{
		# if ployglot finds a model in the dialect of the opponent, it gives
		if (length(unique(organism[[i]]$dial)) <= 2){
			organism[[i]]$wealth <<- organism[[i]]$wealth + T
			organism[[j]]$wealth <<- organism[[j]]$wealth + S
			organism[[i]]$dial <<- organism[[j]]$dial
			# The change in the dialect of polyglot at each exchange
			update.dialect(i)
			# The change in the dialect of polyglot at each exchange
			update.dialect(j)
		}		
	}
}

COOP.vs.COOP <- function( i, j){
	organism[[i]]$wealth <<- organism[[i]]$wealth + T
	organism[[j]]$wealth <<- organism[[j]]$wealth + S
	organism[[j]]$wealth <<- organism[[j]]$wealth + T
	organism[[i]]$wealth <<- organism[[i]]$wealth + S
}


#*********************
# Let's play!
#*********************

#Function for plotting the positions change
giveCol <- function(val){
	if(val == "COOP") {return("red") }
	else if(val == "CHEAT"){return ("blue")}
	else if(val == "POLYGLOT"){return ("green")}
	else if(val == "MIMIC"){return ("darkgrey")}	
}

#Game function
# Parameters:
# 1. iter: game cycles,
# 2. n:number of organisms,
# 3. m: number of deaths and births ant the end of each cycle
# 4. Beta: Beta probability for the number of encounters for each organism
lang.game <- function(iter, n, m, Beta){
	organism <<- vector(mode="list", length=n)
	for (org in 1:n){
		organism[[org]] <<- generate.organism(org)		
	}
	encounter.mat(n, Beta)
	# Construct the abundance matrix
	a <<- matrix(0, nrow=iter, ncol=4)
	rownames(a) <<- as.character(c(1:iter))
	colnames(a) <<- c("CHEAT", "COOP", "POLYGLOT", "MIMIC")
	# Construct the dialect matrix
	dial.mat <<- matrix(0, nrow= iter, ncol=n)
	# Construct the position matrix
	pos.mat <<- matrix(0, nrow= iter, ncol= n)
	for (cy in 1:iter){ 
		for (k in 1:n){
			pos.mat[cy,k] <- organism[[k]]$pos
			dial.mat[cy, k] <- paste(organism[[k]]$dial, collapse=' ')
			if(!is.null(organism[[k]]$strat)){
				if(organism[[k]]$strat == "CHEAT"){
					a[cy,1] <- a[cy,1] + 1
				}
				if(organism[[k]]$strat == "COOP"){
					a[cy,2] <- a[cy,2] + 1
				}
				if(organism[[k]]$strat == "POLYGLOT"){
					a[cy,3] <- a[cy,3] + 1
				}
				if(organism[[k]]$strat == "MIMIC"){
					a[cy,4] <- a[cy,4] + 1
				}
			}
		}	
		for (i in 1:(n-1)){
			for (j in (i+1):n){
				if (encounter_min[i,j] == 1){
					if (!is.null(organism[[i]]$strat)&& organism[[i]]$strat== "CHEAT"){
						if (!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "COOP"){
							CHEAT.vs.COOP(i,j, cy)
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "MIMIC"){
							break
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "POLYGLOT"){
							CHEAT.vs.POLYGLOT(i,j)
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "CHEAT"){
							break
						}
					}
					if (!is.null(organism[[i]]$strat) && organism[[i]]$strat== "COOP"){
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "POLYGLOT"){
							COOP.vs.POLYGLOT(i,j, cy)							
						}
						if (!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "MIMIC"){
							COOP.vs.MIMIC(i,j, cy)							
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "CHEAT"){
							COOP.vs.CHEAT(i,j, cy)							
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "COOP"){
							COOP.vs.COOP(i,j)							
						}
					}
					if (!is.null(organism[[i]]$strat) && organism[[i]]$strat== "POLYGLOT") {
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "MIMIC"){
							POLYGLOT.vs.MIMIC(i,j)
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "CHEAT"){
							POLYGLOT.vs.CHEAT(i,j)
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "COOP"){
							POLYGLOT.vs.COOP(i,j, cy)
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "POLYGLOT"){
							POLYGLOT.vs.POLYGLOT(i,j)
						}
					}
					if (!is.null(organism[[i]]$strat) && organism[[i]]$strat== "MIMIC") {
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "MIMIC"){
							break
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "CHEAT"){
							break
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "COOP"){
							MIMIC.vs.COOP(i,j, cy)
						}
						if(!is.null(organism[[j]]$strat)&& organism[[j]]$strat == "POLYGLOT"){
							MIMIC.vs.POLYGLOT(i,j)
						}
					}
					update.wealth(i)
					update.wealth(j)					
				}
			}
		}
		update.generation(m)
	
		# Plot the progress of the species in png files
		# Create a gif file of 100 frames
		frames = cy
		for (frame in 1:frames){
			if(frame<10){name=paste('000', frame,'plot.png',sep='')}
			if(frame>=10 && frame<=100){name=paste('00',frame, 'plot.png',sep='')}
			if(frame>=100){name=paste('0',frame,'plot.png',sep='')}
		}
		png(name)
		plot(1:100,type="p", ylab="Inherited position of the organisms"
			,xlab="Initial position of the organisms"
			,main=paste('Species distribution at the end of cycle#',cy)
			,cex.main=1.5, font.main=3)

		for(i in 1:n){
			points(x=i,y=organism[[i]]$pos,type = "p",col=giveCol(organism[[i]]$strat), pch=21,bg=giveCol(organism[[i]]$strat))
		}
		dev.off()		
	}

	#Construct elements of the result matrix
	final.pos <<- as.vector(pos.mat[iter,], mode = "numeric")
	fav.pos <<- as.numeric(names(sort(table(final.pos), decreasing= TRUE)[1:1]))
	abund_init <<- as.vector(a[1,], mode = "numeric")
	abund_final <<- as.vector(a[n,], mode = "numeric")
	
	# Save the results of the run in csv tables
	write.matrix(a, file= "abundance.csv")
	write.matrix(dial.mat, file = "dialmat.csv")
	write.matrix(pos.mat, file="positions.csv")

	#Return the result in the console
	return(a)
}

# Call the main function
lang.game(200, 100, repr_rate, Beta)

# Each run gives a row in the result matrix
# loop needed depending of the number of runs

# Example of one run
run <- 1
array1 <- c(memspan,Beta,wealth.reset,fav.pos, (dial_change_rate/100), (repr_rate/100) ,abund_init, abund_final)
names(array1) <- c("memSpan", "Beta","wealthReset", "favPos", "dialUpdateProb", "reprRate", "cheatInit","coopInit","polyInit","mimInit","cheatFinal","coopFinal","polyFinal","mimFinal")
row1<- t(as.matrix(array1))
write.csv(row1, "row11.csv")

# Read the saved dataframes for plotting 

# Call libraries
library(foreign)
library(cluster)
library(gclus)
library(car)
library(MASS)
library(ggplot2)
library(reshape)

Dir <- file.choose()
DataSet <- read.csv(Dir, header=TRUE, sep=";")

# Plot abundance
melta <- melt(DataSet,id="Cycle")
ggplot(melta,aes(x=Cycle,y=value,colour=variable,group=variable)) + 
	geom_jitter()+stat_smooth(alpha=0)+
	xlab('Cycle')+
	ylab('Abundance')
	
# Plot the encounter probability
en <- rowSums(encounter_min)
encounter <- data.frame(en)
qplot(c(1:n), encounter, data = encounter,
	geom="smooth", main = "Encounter Probability",
	xlab = "Organisms", ylab="Number of encounters",
	alpha = 0)
	





