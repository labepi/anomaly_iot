
# This script contains the functions for computing the features used in
# our work

# the dataset should following the format:
#
# p1, ..., pm
# p1, ..., pm
# p1, ..., pm
# ...
# p1, ..., pm
#
# where p1, ..., pm are the columsn corresponding to the m data points
# of each time series

# X - the dataset containing all time series data
# D -  the embedding dimension
# tau_l - a list of embedding delays, the returning features are
#       collapsed sequentially by the tau used to compute them
# na_aware - if TRUE, the symbols with only NAs will be counted separated
# na_rm - if TRUE and na_aware=TRUE, the "NA patterns" are not counted
#
# Returns the following computed list of features (for each time series):
# 1. D
# 2. tau, 
# 3. length(E(g4)),
# 4. mean of G weights
# 5. sd of G weights
# 6. shannon entropy of G weights
# 7. complexity of G weights
# 8. fisher information of G weights
# 9. PST
# 10. (H) shannon entropy of BP distribution
# 11. (C) complexity of BP dist.
# 12. (FI) fisher information of BP dist.
extractFeatures = function(X, D=3, tau_l=1:10, showTime=FALSE, 
                           na_aware=FALSE, na_rm=FALSE)
{
    # TODO: check if this value must be given or set ouside here
    num_of_features = 8 #12

    # the features to compute
    #mycolumns = c('sdEw', 'Hw', 'Cw', 'Fw', 'PST', 'Hpi', 'Cpi', 'Fpi')
    
    # NOTE: removing D and tau from the list
    #mycolumns = c('D', 'tau', 'sdEw', 'Hw', 'Cw', 'Fw', 'PST', 'Hpi', 'Cpi', 'Fpi')
    
    # NOTE: these features have low importance
    # 'lenE', 'meanEw', 
    #buildTime = Sys.time()

    # the length of the series
    m = ncol(X)

    # checking the max number of tau, for this dataset, and if the
    # informed tau_l can be used
    max_tau = min(length(tau_l), checkMaxTau(m, D, lim=2))

    # TODO: check if this is the best strategy
    M = matrix(0, nrow=nrow(X), ncol=num_of_features*max_tau)

    #print(dim(M))

    for(i in 1:nrow(X))
    {
        if (showTime == TRUE)
        {
            buildTimeSeries = Sys.time()
        }

        #tmp = extractFeatureSingle(X[i,], D, tau_l, 
        M[i,] = extractFeatureSingle(X[i,], D, tau_l, 
                                     na_aware=na_aware, na_rm=na_rm)

        #print(tmp)
        #print(length(tmp))

        #print(M[i,])
        #print(length(M[i,]))
        
        #quit()
        
        if (showTime == TRUE)
        {
            buildTimeSeries = difftime(Sys.time(), buildTimeSeries, units='sec')
            cat('TIME PER SERIES:',buildTimeSeries,'\n')
        }
    }

    #buildTime = difftime(Sys.time(), buildTime, units='sec')
    #print(buildTime)

    # TODO: maybe this is not necessary

    # NOTE: reordering features to join same type together
    tau_pos = (0:(ncol(M)-1) %% num_of_features) + 1
    new_pos = c()
    for(i in 1:num_of_features)
    {
        new_pos = c(new_pos, which(tau_pos == i))
    }
    M = M[,c(new_pos)]

    # NOTE: setting features names
    #colnames(M) = mycolumns
    colnames(M) = paste('f', 1:ncol(M), sep='')

    return(M)
}

# x - a single time series data
# D -  the embedding dimension
# tau_l - a list of embedding delays, the returning features are
#       collapsed sequentially by the tau used to compute them
# na_aware - if TRUE, the symbols with only NAs will be counted separated
# na_rm - if TRUE and na_aware=TRUE, the "NA patterns" are not counted
# hop - the hop to consider for the consecutive pattern in transition
#
# Returns the following computed list of features:
# 1. length(E(g4)),
# 2. shannon entropy of G weights
# 3. complexity of G weights
# 4. fisher information of G weights
# 5. PST
# 6. (H) shannon entropy of BP distribution
# 7. (C) complexity of BP dist.
# 8. (FI) fisher information of BP dist.
#lenE, Hw, Cw, Fw, pst, Hpi, Cpi, Fpi
extractFeatureSingle = function(x, D=3, tau_l=1:10, na_aware=FALSE, na_rm=FALSE, hop=1)
{
    
    # all features will be together
    data = c()

    # length of series
    m = length(x)

    #print(x)
    #print(m)
    
    # computing features for each tau
    for (tau in tau_l)
    {
        if (checkParameters(m, D, tau, lim=2) == FALSE)
        {
            next
        }

        #cat('\n\n')

        # pre-computing the symbols for both bpd ans g
        #buildTime = Sys.time()

        # this is the case where NA patterns are considered
        if (na_aware == TRUE)
        {
            symbols = bandt_pompe_na_c(as.numeric(x), D, tau)

            # if considered, but must be removed
            if (na_rm == TRUE)
            {
                symbols = symbols[!is.na(symbols)]
            }
        } else {
            # TODO: testing the tie version
            #symbols = bandt_pompe_tie_c(as.numeric(x), D, tau)
            # this is the C verion
            symbols = bandt_pompe_c(as.numeric(x), D, tau)
            #symbols = bandt_pompe(as.numeric(x), D, tau)
        }

        #buildTime = difftime(Sys.time(), buildTime, units='sec')
        #cat('TIME SYMBOLS:',buildTime,'\n')

        # computing the bandt_pompe distribution
        #buildTime = Sys.time()
        bpd = bandt_pompe_distribution(symbols, D=D, tau=tau, useSymbols=TRUE)
        #buildTime = difftime(Sys.time(), buildTime, units='sec')
        #cat('TIME BPD:',buildTime,'\n')

        # computing the bandt pompe transition graph
        #buildTime = Sys.time()
        g = bandt_pompe_transition(symbols, D=D, tau=tau, useSymbols=TRUE, hop=hop)
        #buildTime = difftime(Sys.time(), buildTime, units='sec')
        #cat('TIME TG:',buildTime,'\n')
        
        # bpd distribution features
        
        # shannon entropy
        Hpi = shannon_entropy(bpd$probabilities, normalized=TRUE)
    
        # statistical complexity
        Cpi = complexity(bpd$probabilities, Hpi)

        # fisher information
        Fpi = fisher_information(bpd$probabilities)
        
        # transition graph features

        # edges and weights (non-zero transitions)
        edges = g != 0
        weights = g[edges]
        
        # number of edges
        lenE = sum(edges)

        # mean of edges weights
        #meanEw = mean(weights)
        
        # mean of edges weights
        #sdEw = sd(weights)
        #if (is.na(sdEw)){ sdEw = 0 }

        # information theory quantifiers from edges weights
        Hw = shannon_entropy(weights, normalized=TRUE)
        Cw = complexity(weights, Hw, normalized=TRUE)
        Fw = fisher_information(weights)

        # probability of self transitions
        pst = matrix.trace(g)
        
        # the current vector of features
        #curdata = c(D, tau, lenE, meanEw, sdEw, Hw, Cw, Fw, pst, Hpi, Cpi, Fpi)
        #curdata = c(D, tau, sdEw, Hw, Cw, Fw, pst, Hpi, Cpi, Fpi)
        #curdata = c(sdEw, Hw, Cw, Fw, pst, Hpi, Cpi, Fpi)
        # NOTE: do not return D and tau
        curdata = c(lenE, Hw, Cw, Fw, pst, Hpi, Cpi, Fpi)

        # TODO: check this:
        # making NA and NaN features values to be 0?
        curdata[is.na(curdata)] = 0

        # joining each features vector
        data = c(data, curdata)
    }


    return(data)
}


# function to adjust the features positions by reordering them to join
# same type together
featuresPosAdjust = function(feats, num_of_features = 8)
{

    tau_pos = (0:(length(feats)-1) %% num_of_features) + 1
    new_pos = c()
    for(i in 1:num_of_features)
    {
        new_pos = c(new_pos, which(tau_pos == i))
    }
    
    return(feats[new_pos])
}




# Functions for computing the (H,C) paris for all tau_l
# this function is used in the single step classification
#
# X - the dataset containing all time series data
# D -  the embedding dimension
# tau_l - a list of embedding delays, the returning features are
#       collapsed sequentially by the tau used to compute them
# na_aware - if TRUE, the symbols with only NAs will be counted separated
# na_rm - if TRUE and na_aware=TRUE, the "NA patterns" are not counted
#
# Returns the following computed list of features (for each time series):
# 1. (H) shannon entropy of BP distribution
# 2. (C) complexity of BP dist.
extractFeaturesHC = function(X, D=3, tau=1, na_aware=FALSE, na_rm=FALSE)
{
    # TODO: check if this value must be given or set ouside here
    num_of_features = 2

    #buildTime = Sys.time()

    # the length of the series
    m = ncol(X)

    # checking the max number of tau, for this dataset, and if the
    # informed tau_l can be used
    #max_tau = min(length(tau_l), checkMaxTau(m, D, lim=2))

    # TODO: check if this is the best strategy
    M = matrix(0, nrow=nrow(X), ncol=num_of_features)

    for(i in 1:nrow(X))
    {
        #if (showTime == TRUE)
        #{
        #    buildTimeSeries = Sys.time()
        #}
        
        #tmp = extractFeatureSingleHC(X[i,], D, tau, na_aware=na_aware, na_rm=na_rm)
        #print(tmp)
        #quit()
        
        M[i,] = extractFeatureSingleHC(X[i,], D, tau, na_aware=na_aware, na_rm=na_rm)

        #if (showTime == TRUE)
        #{
        #    buildTimeSeries = difftime(Sys.time(), buildTimeSeries, units='sec')
        #    cat('TIME PER SERIES:',buildTimeSeries,'\n')
        #}
        #print(M[i,])
        #print(length(M[i,]))
    }

    #buildTime = difftime(Sys.time(), buildTime, units='sec')
    #print(buildTime)

    return(M)
}



# computing the (H,C) pairs for all tau_l of a single time series
# x - a single time series data
# D -  the embedding dimension
# tau - the embedding delay
# na_aware - if TRUE, the symbols with only NAs will be counted separated
# na_rm - if TRUE and na_aware=TRUE, the "NA patterns" are not counted
#
# Returns the following computed list of features:
# 1. (H) shannon entropy of BP distribution
# 2. (C) complexity of BP dist.
extractFeatureSingleHC = function(x, D=3, tau=1, na_aware=FALSE, na_rm=FALSE)
{
    # all features will be together
    #data = c()

    # length of series
    m = length(x)
    
    # computing features for each tau
        if (checkParameters(m, D, tau, lim=2) == FALSE)
        {
            next
        }

        #if (showTime == TRUE)
        #{
        #    buildTimeTau = Sys.time()
        #}
        
        # computing the bandt_pompe distribution
        bpd = bandt_pompe_distribution(as.numeric(x), D=D, tau=tau,
                                     na_aware=na_aware, na_rm=na_rm)
        #bpd = bandt_pompe_distribution2(as.numeric(x), D=D, tau=tau) #old version

        # shannon entropy
        Hpi = shannon_entropy(bpd$probabilities, normalized=TRUE)

        # statistical complexity
        Cpi = complexity(bpd$probabilities, Hpi)

        #if (showTime == TRUE)
        #{
        #    buildTimeTau = difftime(Sys.time(), buildTimeTau, units='sec')
        #    cat('TIME PER TAU:',buildTimeTau,'\n')
        #}

        # the current vector of features
        curdata = c(Hpi, Cpi)

        # TODO: check this:
        # making NA and NaN features values to be 0?
        #curdata[is.na(curdata)] = 0

        # joining each features vector
        #data = c(data, curdata)

    return(curdata)
}



#library(FSelector)
#library(stats)
#
#get.data.frame.from.formula <- function(formula, data) {
#	d = model.frame(formula, data, na.action = NULL)
#	for(i in 1:dim(d)[2]) {
#		if(is.factor(d[[i]]) || is.logical(d[[i]]) || is.character(d[[i]]))
#			d[[i]] = factor(d[[i]])
#	}
#	return(d)
#}
#discretize.all <- function(formula, data) {
#	new_data = get.data.frame.from.formula(formula, data)
#	
#	dest_column_name = dimnames(new_data)[[2]][1]
#	if(!is.factor(new_data[[1]])) {
#		new_data[[1]] = equal.frequency.binning.discretization(new_data[[1]], 5)
#	}
#	
#	new_data = supervised.discretization(formula, data = new_data)
#
#	# reorder attributes
#	new_data = get.data.frame.from.formula(formula, new_data)
#	return(new_data)
#}
#supervised.discretization <- function(formula, data) {
#	data = get.data.frame.from.formula(formula, data)
#	complete = complete.cases(data[[1]])
#	all.complete = all(complete)
#	if(!all.complete) {
#		new_data = data[complete, , drop=FALSE]
#		result = Discretize(formula, data = new_data, na.action = na.pass)
#		return(result)
#	} else {
#		return(Discretize(formula, data = data, na.action = na.pass))
#	}
#	
#}
###### CHI-SQUARED
#### classification and regression
#### continous and discrete data
####chi.squared <- function(formula, data) {
###chi.squared <- function(new_data, class_data, data) {
###	
####	new_data = get.data.frame.from.formula(formula, data)
####
####    print(new_data)
####	new_data = discretize.all(formula,new_data)
####	
####	class_data = new_data[[1]]
####	new_data = new_data[-1] #new_data without class attr
###
###	results = sapply(new_data, function(w) {
###			cont = table(class_data, w)
###			row_sums = apply(cont, 1, sum)
###			col_sums = apply(cont, 2, sum)
###			all_sum = sum(col_sums)
###			expected_matrix = t(as.matrix(col_sums) %*% t(as.matrix(row_sums))) / all_sum
###			chis = sum((cont - expected_matrix) ^ 2 / expected_matrix)
###			
###			if(chis == 0 || length(col_sums) < 2 || length (row_sums) < 2) {
###				return(0)
###			} else {
###				# phi or Cramer's V
###				return(sqrt(chis / (all_sum * min(length(col_sums) - 1, length(row_sums) - 1))))
###			}
###		})
###
###	attr_names = dimnames(new_data)[[2]]
###	return(data.frame(attr_importance = results, row.names = attr_names))
###}


# computes the features selection strategy and returns the pre-selected
# features columns
# criteria:
# - no more than a half of the features
# - no less than 5 features
#maxlen = ncol(ds)/2
featuresSelection = function(x, y, maxlen=200, minlen=5, force_cut=FALSE)
{
    # performing the ensemble of features selection methods

    # number of features
    numfeats = ncol(x)

    #print(numfeats)
    #print(dim(x))

    #print('>>>>>>>>>>>>>>>')
    #print(head(x))

    # converting to a data.frame
    xdf = as.data.frame(x)
    #xdf = as.data.frame(x[,220:230])
    #xdf$class = as.factor(y)

    # NOTE: not using as factor to mimic the FSelector package
    xdf$class = as.numeric(y)
 
    
    #quit()

   
    #print('>>>>>>>>>>>>>>>')
    #print(xdf[,c(220:225)])
 
    #print(head(xdf))
    #print(xdf[10:21,100:110])

    #quit()


    #write.csv(xdf, 'tmp_xdf.csv')
 
    #set.seed(123)
    
    ###################################

    # ensemble of features 
    ens = list()

    # computing the following feature importance

    # information gain
    ens[[1]] = tryCatch({
                #information_gain(class ~ ., xdf, type='infogain')$importance
                information_gain(class ~ ., xdf, type='infogain', equal=T)$importance
                #information.gain(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })
 
    # gain ratio
    ens[[2]] = tryCatch({
                #information_gain(class ~ ., xdf, type='gainratio')$importance
                information_gain(class ~ ., xdf, type='gainratio', equal=T)$importance
                #gain.ratio(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })
 
    # symmetrical uncertainty
    ens[[3]] = tryCatch({
                #information_gain(class ~ ., xdf, type='symuncert')$importance
                information_gain(class ~ ., xdf, type='symuncert', equal=T)$importance
                #symmetrical.uncertainty(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })

#    # chi.squared
#    ens[[4]] = 
#        tryCatch({
#                chi.squared(class ~ ., xdf)
#                #chi.squared(xdf[,-ncol(xdf)], xdf$class, xdf)
#             },
#             error=function(cond){
#                tmp = data.frame(attr_importance = rep(0.0, numfeats))
#                rownames(tmp) = paste('f',rownames(tmp), sep='')
#                return(tmp)
#             })


#    # relief
#    ens[[4]] = tryCatch({
#                relief(class ~ ., xdf)$importance
#             },
#             error=function(cond){
#                tmp = data.frame(attr_importance = rep(0.0, numfeats))
#                rownames(tmp) = paste('f',rownames(tmp), sep='')
#                return(tmp)
#             })
#

    # ROC
#    ens[[4]] = tryCatch({
#                apply(filterVarImp(xdf[,1:numfeats], xdf$class), 1, mean)
#             },
#             error=function(cond){
#                tmp = data.frame(attr_importance = rep(0.0, numfeats))
#                rownames(tmp) = paste('f',rownames(tmp), sep='')
#                return(tmp)
#             })

#    # RANDF
#    ens[[4]] = tryCatch({
#                randomForest(class ~ ., data=xdf)$importance
#             },
#             error=function(cond){
#                tmp = data.frame(attr_importance = rep(0.0, numfeats))
#                rownames(tmp) = paste('f',rownames(tmp), sep='')
#                return(tmp)
#             })


    #print(ens)
    #print(cbind(ens[[1]], ens[[2]], ens[[3]], ens[[4]], ens[[5]]))
    #print(cbind(ens[[1]], ens[[2]], ens[[3]], ens[[4]]))
#    print(cbind(ens[[1]], ens[[2]], ens[[3]]))
    #quit()

    # ranked ensembles
    rens = matrix(0, nrow=numfeats, ncol=length(ens))
    for(i in 1:length(ens))
    {
        # remove NAs
        #print(sum(is.na(ens[[i]])))
        ens[[i]][is.na(ens[[i]])] = 0
        rens[,i] = sapply(1:numfeats, function(j){which(order(ens[[i]], decreasing=T)==j)})
    }

#    print(rens)
#    quit()

    # computing the overall score for each feature
    score = data.frame(
                       mean=apply(rens, 1, mean), 
                       var=apply(rens, 1, var), 
                       median=apply(rens, 1, median), 
                       mad=apply(rens, 1, mad))


    ####################
    # TODO: testing the arc idea

    # normalizing var e mad in [0,400]
    score$nvar = score$var/(max(score$var)/numfeats) #5
    score$nmad = score$mad/(max(score$mad)/numfeats) #6
    
    # hypotenuse (distance)
    # projection on x for mean/nvar and median/nmad
    score$d1 = sqrt(score$mean^2 + score$nvar^2) #7
    score$d2 = sqrt(score$median^2 + score$nmad^2) #8

    # TODO:
    # parei aqui

    # the features ordered by the radio
    xoall=order(score$d1)

    # TODO: testando remover aquelas features acima da variancia
    #xo = xoall[score[xoall,5] <= score[xoall,1]]

    # ordered features
    xo = xoall[1:maxlen]

    #print(head(xo))

    ############# ideia
    # sequence of two consecutive diffs above the mean(diff)
    xo_diff = diff(score$d1[xo])
    
    #print(head(xo_diff))
    
    # TODO: empirically used for testing
    golden_ratio = 1.618

    phi = golden_ratio/8

    # the first meating the criteria
    if (force_cut == FALSE)
    {
        # TODO: testing new idea here
        # - the point at which the var is much higher than the mean
        #inds1 = which((score[xo,5] - score[xo,1]) > 10)
        #inds1 = which((score[xo,5] - score[xo,1]) > 40)
        
        # the slope is higher than \phi
        #inds1 = which((score[xo,5]/score[xo,1]) > 1.5)
        inds1 = which((score$nvar[xo]/score$mean[xo]) > golden_ratio)
        scorecut1 = inds1[inds1 >= minlen][1]
        
        # - the largests gap
        inds2 = order(xo_diff, decreasing=TRUE)
        scorecut2 = inds2[inds2 >= minlen][1]

        printdebug(paste('Scorecut1:',scorecut1))
        printdebug(paste('Scorecut2:',scorecut2))
        
        #scorecut = scorecut1
        #scorecut = scorecut2

        # NOTE: to handle the case where there are no points over the ratio
        if (is.na(scorecut1))
        {
            scorecut = scorecut2
        } else {
#
#            # TODO: testing a new complementar idea
#
#            # counting the densities of points in each cut
#            t1 = score[xo,c(1,5)][1:scorecut1,]
#            t2 = score[xo,c(1,5)][1:scorecut2,]
#
#            # computing densities
#
#            # densities cut1
#            d_up1 = sum(t1[,2]/t1[,1]>1)
#            d_down1 = sum(t1[,2]/t1[,1]<=1)
#            d1 = min(d_up1, d_down1)/max(d_up1, d_down1)
#
#            # densities cut2
#            d_up2 = sum(t2[,2]/t2[,1]>1)
#            d_down2 = sum(t2[,2]/t2[,1]<=1)
#            d2 = min(d_up2, d_down2)/max(d_up2, d_down2)
#
#            # difference between densities are unstable
#            if (abs(d1 - d2) > phi)
#            {
#                # use the gap
#                scorecut = scorecut2
#            } else {
#                # use the slope
#                scorecut = scorecut1
#            }
#
        
            #scorecut = min(scorecut1, scorecut2)
            scorecut = max(scorecut1, scorecut2)
            #scorecut = floor((scorecut1 + scorecut2)/2)
        }

    } else {
        scorecut = maxlen
    }

    #print(scorecut)

    ####################################################################

    # prepare "circle data"
    #radius = 1
    theta = seq(0, 2 * pi, length = 200)

#    pdf('tmp_arc.pdf')
#
#    plot(score[,1], score[,5])
#for (i in xoall)
#{
#    lines(x=score[i,7]*cos(theta),    y=score[i,7]*sin(theta), col='gray')
#}
#abline(0,1)
#abline(0,1.5, col=3)
#abline(0,1.6, col=2)
#abline(0,2, col=4)
#
#j=xo[scorecut]
#lines(x=score[j,7]*cos(theta),    y=score[j,7]*sin(theta), col='red')
#
#    dev.off()



    ####################################################################
    ####################################################################
    # plotting the chosen features
    #plotFeaturesChosen(d_name, score, xo, scorecut, numfeats)
    #quit()
    ####################################################################
    ####################################################################
    
    #print(scorecut)
    #print(head(xdf))
    
    # the subset of selected features
    feats_sel = colnames(xdf)[xo[1:scorecut]]

    return(feats_sel)
}


# function to plot the features chosen arc graph
# 
plotFeaturesChosen = function(d_name, score, xo, scorecut, numfeats)
{
    ############## ggplot way

    library(ggplot2)
    library(ggforce)

    # d1 = distance with mean/var
    # d2 = distance with median/mad
    #colnames(score) = c('mean', 'var', 'median', 'mad', 'nvar', 'nmad', 'd1', 'd2')
    #xscore = as.data.frame(score)

    # hilighting the selected features
    score$col=2
    score[xo[1:scorecut],'col'] = 1

    score$col[score$col == 1] = 'Yes'
    score$col[score$col == 2] = 'No'
    score$col = as.factor(score$col)

    p = 

        ggplot(score) + theme_bw(base_size=14) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(legend.position = "bottom",
                axis.text=element_text(size=14),
                axis.title=element_text(size=14),
                legend.text=element_text(size=14)) +
        labs(x='Score', y='Normalized variance') +
        xlim(0,min(200,numfeats)) + ylim(0,min(200,numfeats)) +
        geom_arc(aes(x0=0, y0=0, r=d1, start=0, end=pi/2, 
                     colour=col))+
                     #, linetype=as.factor(col))) +
        geom_vline(xintercept=0, color='#000000', size=0.5) +
        geom_hline(yintercept=0, color='#000000', size=0.5) +
        geom_line(aes(x=x, y=x), col='red', linetype=2, data=data.frame(x=seq(1,200))) +
        geom_line(aes(x=x, y=1.5*x), col='black', linetype=1, data=data.frame(x=seq(1,200))) +
#        geom_vline(xintercept=score[xo[scorecut],7], color='#ff0000', size=0.5) +
#        geom_vline(xintercept=score[xo[scorecut+1],7], color='#ff0000', size=0.5) +
        geom_point(aes(x=mean, y=nvar, fill=col, 
                       shape=col), size=3, color="#000000", stroke=0, alpha=0.9) +
        scale_color_manual('Chosen feature',values=c("#dddddd", "#ff8080")) +
        #scale_color_manual('Chosen feature',values=c("#dddddd", "#888888")) +
        scale_fill_manual('Chosen feature',values=c("#FAAB18","#1380A1")) + 
        scale_shape_manual('Chosen feature',values=c(22, 21)) 
        
    #ggsave(paste('img/fig_features_chosen_arc_',d_name,'.eps', sep=''), plot=p, width=6, height=6, device=cairo_ps, dpi=600)
    ggsave(paste('img/fig_features_chosen_arc_',d_name,'.pdf', sep=''), plot=p, width=6, height=6, dpi=600)


}



# computes the features selection strategy and returns the pre-selected
# features columns
# criteria:
# - no more than a half of the features
# - no less than 5 features
#maxlen = ncol(ds)/2
featuresSelection2 = function(x, y, maxlen=200, minlen=5, force_cut=FALSE)
{
    # performing the ensemble of features selection methods

    # number of features
    numfeats = ncol(x)

    #print(numfeats)
    #print(dim(x))

    #print('>>>>>>>>>>>>>>>')
    #print(head(x))

    # converting to a data.frame
    xdf = as.data.frame(x)
    #xdf = as.data.frame(x[,220:230])
    xdf$class = as.factor(y)
    
    #print('>>>>>>>>>>>>>>>')
    #print(xdf[,c(220:225)])
 
    #print(head(xdf))
    #print(xdf[10:21,100:110])

    #quit()


    #write.csv(xdf, 'tmp_xdf.csv')
 
    #set.seed(123)
    
    ###################################

    # ensemble of features 
    ens = list()

    # computing the following feature importance

    # chi.squared
    ens[[1]] = tryCatch({
                chi.squared(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })

    # information gain
    ens[[2]] = tryCatch({
                information.gain(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })
 
    # gain ratio
    ens[[3]] = tryCatch({
                gain.ratio(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })
 
    # symmetrical uncertainty
    ens[[4]] = tryCatch({
                symmetrical.uncertainty(class ~ ., xdf)
             },
             error=function(cond){
                tmp = data.frame(attr_importance = rep(0.0, numfeats))
                rownames(tmp) = paste('f',rownames(tmp), sep='')
                return(tmp)
             })


    #print(ens)
    #print(cbind(ens[[1]], ens[[2]], ens[[3]], ens[[4]]))
    #quit()

    # ranked ensembles
    rens = matrix(0, nrow=numfeats, ncol=length(ens))
    for(i in 1:length(ens))
    {
        rens[,i] = sapply(1:numfeats, function(j){which(order(ens[[i]], decreasing=T)==j)})
    }

    
    #print(rens)
    #quit()

    # computing the overall score for each feature
    score = cbind(apply(rens, 1, mean), 
                  apply(rens, 1, var), 
                  apply(rens, 1, median), 
                  apply(rens, 1, mad))


    ####################
    # TODO: testing the arc idea

    # normalizing var e mad in [0,400]
    score = cbind(score, score[,2]/(max(score[,2])/numfeats)) #5
    score = cbind(score, score[,4]/(max(score[,4])/numfeats)) #6
    
    # hypotenuse
    # projection on x for mean/var and median/mad
    score = cbind(score, sqrt(score[,1]^2 + score[,5]^2)) #7
    score = cbind(score, sqrt(score[,3]^2 + score[,6]^2)) #8


    # the features ordered by the radio
    xoall=order(score[,7])

    # TODO: testando remover aquelas features acima da variancia
    #xo = xoall[score[xoall,5] <= score[xoall,1]]

    xo = xoall[1:maxlen]

    print(head(xo))

    ############# ideia
    # sequence of two consecutive diffs above the mean(diff)
    xo_diff = diff(score[xo,7])
    
    print(head(xo_diff))
    
    # TODO: empirically used for testing
    golden_ratio = 1.618

    phi = golden_ratio/8

    # the first meating the criteria
    if (force_cut == FALSE)
    {
        # TODO: testing new idea here
        # - the point at which the var is much higher than the mean
        #inds1 = which((score[xo,5] - score[xo,1]) > 10)
        #inds1 = which((score[xo,5] - score[xo,1]) > 40)
        # the slope is higher than \phi
        #inds1 = which((score[xo,5]/score[xo,1]) > 1.5)
        inds1 = which((score[xo,5]/score[xo,1]) > golden_ratio)
        scorecut1 = inds1[inds1 >= minlen][1]
        # - the largests gap
        inds2 = order(xo_diff, decreasing=TRUE)
        scorecut2 = inds2[inds2 >= minlen][1]
        
        #scorecut = scorecut1
        #scorecut = scorecut2

        # NOTE: to handle the case where there are no points over the ratio
        if (is.na(scorecut1))
        {
            scorecut = scorecut2
        } else {

            # TODO: testing a new complementar idea

            # counting the densities of points in each cut
            t1 = score[xo,c(1,5)][1:scorecut1,]
            t2 = score[xo,c(1,5)][1:scorecut2,]

            # computing densities

            # densities cut1
            d_up1 = sum(t1[,2]/t1[,1]>1)
            d_down1 = sum(t1[,2]/t1[,1]<=1)
            d1 = min(d_up1, d_down1)/max(d_up1, d_down1)

            # densities cut2
            d_up2 = sum(t2[,2]/t2[,1]>1)
            d_down2 = sum(t2[,2]/t2[,1]<=1)
            d2 = min(d_up2, d_down2)/max(d_up2, d_down2)

            # difference between densities are unstable
            if (abs(d1 - d2) > phi)
            {
                # use the gap
                scorecut = scorecut2
            } else {
                # use the slope
                scorecut = scorecut1
            }

        }
        
        #scorecut = min(scorecut1, scorecut2)
        #scorecut = max(scorecut1, scorecut2)
        #scorecut = floor((scorecut1 + scorecut2)/2)
    } else {
        scorecut = maxlen
    }

    #print(scorecut)
    #print(head(xdf))
    
    # the subset of selected features
    feats_sel = colnames(xdf)[xo[1:scorecut]]

    return(feats_sel)
}

