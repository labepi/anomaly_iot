
# transforming a single feature vector in a dataset

# organizing the feature as a matrix
featureAsDataset = function(x, series_len, label=1, skip=0)
{
    #cat('LEN X 1:',length(x),'\n')

    # nuumber of first measures to skip
    if (skip > 0)
    {
        x = x[(skip+1):length(x)]
    }
    
    #cat('LEN X 2:',length(x),'\n')

    # number of rows in the dataset
    nrows = floor(length(x)/series_len)

    # TODO:
    # check if let the matrix repeat the first values to complete the
    # number of rows, or if truncate the series and reduce one row

    # organizing the feature as a matrix
    df = matrix(x[1:(nrows*series_len)], byrow=T, ncol=series_len)
    #df = matrix(x, byrow=T, ncol=series_len)

    # TODO: check if this is necessary
    # adding the label columns
    df = cbind(df, rep(label, nrow(df)))

    return(df)
}



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
extractFeatures = function(X, D=3, tau_l=1:10, num_of_feature=8,
                           showTime=FALSE, na_aware=FALSE, na_rm=FALSE)
{
    # TODO: check if this value must be given or set ouside here
    # number of computed features per tau
    #num_of_features = 8

    # the length of the series
    m = ncol(X)

    # checking the max number of tau, for this dataset, and if the
    # informed tau_l can be used
    max_tau = min(length(tau_l), checkMaxTau(m, D, lim=2))

    # the dataset result of computed features
    num_comp_feats = num_of_features*(length(tau_l))

    # TODO: check if this is the best strategy
    # NOTE: +1 to add the class type
    #M = matrix(0, nrow=nrow(X), ncol=num_comp_feats+1)
    M = matrix(0, nrow=nrow(X), ncol=num_comp_feats)
    #ds = matrix(0, ncol=(num_comp_feats+1), nrow=0)
    
    #colnames(M) = c(paste('f',1:num_comp_feats, sep=''), 'class')
    colnames(M) = paste('f',1:num_comp_feats, sep='')

    #print(dim(M))

    for(i in 1:nrow(X))
    {
        if (showTime == TRUE)
        {
            buildTimeSeries = Sys.time()
        }
    
        # filtering
        #xtmp = X[i,1:series_len]
        xtmp = X[i,]

        # MULTISCALE FEATURES TRANSFORMATION

        # extracting features for each time series 
        feats = extractFeatureSingle(X[i,], D, tau_l, 
                                     na_aware=na_aware, na_rm=na_rm)

        # NOTE: maybe this is not necessary
        # adjusting features positions, just to be visually identifiable
        feats = featuresPosAdjust(feats, num_of_features=num_of_features)

        # adding computed features to the dataset
        M[i,] = feats

        if (showTime == TRUE)
        {
            buildTimeSeries = difftime(Sys.time(), buildTimeSeries, units='sec')
            cat('TIME PER SERIES:',buildTimeSeries,'\n')
        }
    }

    #buildTime = difftime(Sys.time(), buildTime, units='sec')
    #print(buildTime)

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
        #curdata = c(Hw, Cw, Fw, pst, Hpi, Cpi, Fpi)
        #curdata = c(Hw, Cw, pst, Hpi, Cpi)

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



plotFeatures = function(x, y, d_name, n_feats=8)
{
    # features labels
    if (n_feats == 8)
    {
        colsexp = c(
         expression(N[E]), 
         expression(paste(H[S],'[',E[w],']')), 
         expression(paste(C[JS],'[',E[w],']')), 
         expression(paste(F,'[',E[w],']')), 
         expression(p[st]), 
         expression(paste(H[S],'[',p[pi],']')), 
         expression(paste(C[JS],'[',p[pi],']')), 
         expression(paste(F,'[',p[pi],']'))
        )
    } 
    else
    {
        colsexp = paste('f', 1:n_feats, sep='')
    }
    
    ################ DATASET NUMBERS

    # the classes
    classes = unique(y)

    # number of classes
    num_classes = length(classes)

    ################ ANALYSIS OF FEATURES ###############

    # number of considered features
    #n_feats = 8

    # number of total computed features 
    m_feats = ncol(x)

    # maximum number of tau
    k = m_feats/n_feats

    # preparing the mean features
    x_mean = matrix(NA, nrow=num_classes, ncol=m_feats)
    x_sd = matrix(NA, nrow=num_classes, ncol=m_feats)

    # applying mean in train dataset
    for(i in 1:num_classes)
    {
        x_mean[i,] = apply(x[y == classes[i],], 2, mean)
        x_sd[i,] = apply(x[y == classes[i],], 2, sd)
    }


    # SAVING FIGURE

    p = list()

    myleg = NULL

    ind = 1

    # looping at each feature
    for (i in seq(1, m_feats, by=k))
    {
        x_mean_i = x_mean[,i:(i-1+k)]
        x_sd_i = x_sd[,i:(i-1+k)]

        df = data.frame()

        # preparing the data.frame
        for(j in 1:num_classes)
        {
            df = rbind(df, data.frame(tau=1:k, feat=x_mean_i[j,], 
                                      var=x_sd_i[j,], Classes=classes[j]))
        }

        df$Classes = as.factor(df$Classes)

        # plotting each feature individually
        p[[ind]] = ggplot(df, aes(x=tau, y=feat, colour=Classes, shape=Classes)) +
            geom_point(size=2) + geom_line() + 
            geom_errorbar(aes(ymin=feat-var, ymax=feat+var), width=.2,
                 position=position_dodge(0.05)) +
            theme_bw(base_size=22) +
            theme(legend.position='bottom') +
            xlab(expression(tau)) +
            ylab(colsexp[ind]) +
            guides(
                    color=guide_legend(nrow=1, title.position="top", title.hjust=0.5),
                    shape=guide_legend(nrow=1, title.position="top", title.hjust=0.5))


        # adjusting only one legend
        if (is.null(myleg))
        {
            tmp = ggplot_gtable(ggplot_build(p[[ind]]))
            leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            myleg = tmp$grobs[[leg]]
        }
        
        p[[ind]] = p[[ind]] + theme(legend.position='none')

        ind = ind + 1
    }

    pall = grid.arrange(arrangeGrob(grobs=p, ncol=4), myleg, heights=c(10,1))

    # saving the plot
    ggsave(paste('img/fig_',d_name,'_features.pdf', sep=''), pall, width=16, height=7)


}


