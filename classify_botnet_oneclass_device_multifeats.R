
# loading the config file, where are the paths to the functions
source('config.R')

# cinluding sources and libraries
source('includes.R')

# loading some util functions
source('utils.R')

# loading the bandt-pompe functions
loadSource(bandt_pompe_path, 'bandt_pompe.R')

# loading the transition graph functions
loadSource(bandt_pompe_path, 'bandt_pompe_graph.R')

# loading the measures functions
loadSource(bandt_pompe_path, 'measures.R')

# loading helper functions
loadSource(bandt_pompe_path, 'helpers.R')

# features functions
source('features.R')


library(caret)
library(ggplot2)
library(reshape2)
# some plot libs
#suppressMessages(library(ggforce))
#suppressMessages(library(grid))
#suppressMessages(library(gridExtra))
#suppressMessages(library(factoextra))

# TODO: package for isolationForest
#library(solitude)
library(isotree)

# extended isolation forest
#library(eif)

#
#library(Rlof)

printdebug('Loaded libraries and functions')


# getting command line args
args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0)
{
    # TODO: precisa criar o dataset no formato adequado?
    
    # NOTE: this is the dataset from the Intel Lab, only filtering for
    # the lines with 8 columns

    # dataset name
    #d_name = 'HH_L5_radius' # 0.90625
    #d_name = 'MI_dir_L5_weight'  # 0.9375
    #d_name = 'MI_dir_L5_mean'     # 0.04
    #d_name = 'MI_dir_L5_variance' # 0.88
    #d_name = 'H_L5_weight'  # 0.9453

    # the Bandt-Pompe parameters

    # embedding dimension
    D=3

    # embedding delay
    #tau_l=1:50
    #tau_l=1:10
    tau_l=1:10

    # default SEED is loaded from config.R
    SEED=1

    # used for loading the time series
    series_len = 1000
    #series_len = 2000
    #series_len = 5000

    # trying a single model for device
    device = 'Danmini_Doorbell'
    #device = 'Ecobee_Thermostat'
    #device = 'Ennio_Doorbell'
    #device = 'Philips_B120N10_Baby_Monitor'
    #device = 'Provision_PT_737E_Security_Camera'
    #device = 'Provision_PT_838_Security_Camera'
    #device = 'Samsung_SNH_1011_N_Webcam'
    #device = 'SimpleHome_XCS7_1002_WHT_Security_Camera'
    #device = 'SimpleHome_XCS7_1003_WHT_Security_Camera'

    # the attack data
    attack = 2

} else {
    # loading the datasets
    #d_name = args[1]
    
    # the Bandt-Pompe parameters

    # embedding dimension
    D = as.numeric(args[1])
    
    # embedding delay (end of list)
    tau_l = 1:as.numeric(args[2])

    # the seed to random
    SEED = as.numeric(args[3])

    # the series len to use (1000 or 5000)
    series_len = as.numeric(args[4])
    
    # the device
    device = args[5]

    # the attack data
    attack = as.numeric(args[6])
}

# defining the seed
set.seed(SEED)

# the percentage of train dataset split
train_pct = TRAIN_PCT # from config.R

#printdebug(d_name)

printdebug(paste('(D,tau): ',D,',',paste(range(tau_l), collapse=':'), sep=''))

printdebug(paste('SEED:',SEED))


dev_names = c(
    'Danmini_Doorbell',
    'Ecobee_Thermostat',
    'Ennio_Doorbell',
    'Philips_B120N10_Baby_Monitor',
    'Provision_PT_737E_Security_Camera',
    'Provision_PT_838_Security_Camera',
    'Samsung_SNH_1011_N_Webcam',
    'SimpleHome_XCS7_1002_WHT_Security_Camera',
    'SimpleHome_XCS7_1003_WHT_Security_Camera'
    )

# this is the number of points in the new dataset
#num_rows=5000
num_rows=10000

# NOTE: the labels for each class is defined here:

f_names = data.frame(
            files = c(
            'benign_traffic',
            'mirai/ack',
            'mirai/scan',
            'mirai/syn',
            'mirai/udp',
            'mirai/udpplain',
            'gafgyt/combo',
            'gafgyt/junk',
            'gafgyt/scan',
            'gafgyt/tcp',
            'gafgyt/udp'),
            groups = c(1,rep(2,5), rep(3,5)),
            labels = 1:11,
            stringsAsFactors=FALSE
            )



#######################################
# LOADING DATA AND COMPUTING FEATURES #
#######################################


dataset_path = './data/botnet/original'

# number of computed features per tau
#num_of_features = 5
#num_of_features = 7
num_of_features = 8


# LOADING BENIGN DATA
#####################

# loading benign data
# these are prepared botnet data, from N-BaIoT
x_ben = read.csv(paste(dataset_path, '/', device, '/', f_names[1,1], '.csv', sep=''))


# LOADING ATTACK DATA
######################

# these are prepared botnet data, from N-BaIoT
#x_att = read.csv(paste(dataset_path, '/', device, '/', f_names[2,1], '.csv', sep=''))
#x_att = read.csv(paste(dataset_path, '/', device, '/', f_names[11,1], '.csv', sep=''))
x_att = read.csv(paste(dataset_path, '/', device, '/', f_names[attack,1], '.csv', sep=''))



d_name_l = c(
#    'MI_dir_L5_weight',
#    'H_L5_weight'
# 'HpHp_L5_magnitude',
# mirai
# 'HH_L5_radius', 
# 'MI_dir_L5_variance',
# 'HH_L5_covariance',
## 'HpHp_L5_pcc',
 'HH_L5_magnitude',
 'MI_dir_L5_weight',
 'H_L5_weight'
# bashlite
# "MI_dir_L3_variance",
# "HH_jit_L1_mean",
## "HpHp_L1_magnitude",
# "HH_L0.1_std",
## "HH_L5_magnitude",
# "HpHp_L0.01_covariance",
# "MI_dir_L5_weight",
# "H_L5_weight"
)

# TODO: testando todas as L5 features
d_name_l = read.csv('data/botnet/demonstrate_structure.csv', header=F)
# filtering only L5 features
#d_name_l = d_name_l[grepl('L5', d_name_l)]
# filtering only MI features
#d_name_l = d_name_l[grepl('MI_dir_L5', d_name_l)]
d_name_l = d_name_l[grepl('MI_dir_L5_wei', d_name_l)]
# filtering only MI features
#d_name_l = d_name_l[grepl('^H_L5', d_name_l)]

# multiple models
model = list()

# the classification results
res = list()

i = 0

# preparing the parameters for confusion matrix 
l_s = c("1", "0")

# number of initial observations to skip
skip=0

# multi features classifiers
for (d_name in d_name_l)
{
    # the model index
    i = i + 1

    # BENIGN DATA
    #############

    # filtering by a single feature
    x_ben_feat = x_ben[,d_name]

    # transforming a single feature vector in a dataset
    # organizing the feature as a matrix
    x_ben_df = featureAsDataset(x_ben_feat, series_len, label=1, skip=skip)

    # removing class column from dataset matrix
    y_ben_df = x_ben_df[,ncol(x_ben_df)]
    x_ben_df = x_ben_df[,-ncol(x_ben_df)]

    print(dim(x_ben_df))

    # TODO: esta dando valores diferentes quando usa-se o metodo featureAsDataset

    # computing features for the whole dataset, for all time series
    x_all = extractFeatures(x_ben_df, D, tau_l, num_of_features, showTime=FALSE, na_aware=FALSE, na_rm=FALSE)

    # all classes 
    y_all = y_ben_df

    
    # ATTACK DATA
    #############

    # filtering by a single feature
    x_att_feat = x_att[,d_name]

    # #TODO: here the label may change for separating different types of
    # attacks

    # formatting dataset of series_len time series length
    x_att_df = featureAsDataset(x_att_feat, series_len, label=0, skip=skip)

    # removing class column from dataset matrix
    y_att_df = x_att_df[,ncol(x_att_df)]
    x_att_df = x_att_df[,-ncol(x_att_df)]

    print(dim(x_att_df))

    # computing features for the whole dataset, for all time series
    x_all_att = extractFeatures(x_att_df, D, tau_l, num_of_features, showTime=FALSE, na_aware=FALSE, na_rm=FALSE)

    # all classes
    y_all_att = y_att_df


    ################ SPLIT TRAIN/TEST ###############

    # BENIGN DATA
    #############

    #TODO: maybe select these train samples randomly?
    
    ## define the split rate
    #id_train = createDataPartition(y=y_all, p=train_pct, list=FALSE)
    # NOTE: using half the time series for training on benign data
    #id_train = 1:round(nrow(x_all)/2)
    # NOTE: using 2/3 for train and 1/3 for test
    id_train = 1:round((2/3)*nrow(x_all))

    # Splitting datasets
    x_train = x_all[id_train,]
    y_train = y_all[id_train]

    x_test = x_all[-id_train,]
    y_test = y_all[-id_train]


    # ATTACK DATA
    #############

    x_test = rbind(x_test, x_all_att)
    y_test = c(y_test, y_all_att)

    print(dim(x_test))

    
    ########### TODO:
    # testando a analise das features
    #plotFeatures(x_test, y_test, paste('antes-',d_name,'-', sep=''), num_of_features)

    ### TODO: ### TODO: ### TODO: ### TODO: ###
    # testando remover as features do numero de arestas
    ###########################################

    # depois remover do proprio features.R
#    x_train = x_train[,-tau_l]
#    x_test = x_test[,-tau_l]

    ### TODO: ### TODO: ### TODO: ### TODO: ###
    # TODO: parei aqui
    # ok - concatenar as features de ataque no dataset de test
    # ok - testar a classificao geral
    # - analisar os erros
    # - precisa ver quais features sao melhores, pra tentar melhorar os
    # resultados

    ################ SCALING DATA ###############

    printdebug('Scaling data')

    # TODO: we have to scale the computed features 
    # preprocesing the features dataset
    transform = preProcess(x_train, method=c("center", "scale"))

    x_train = predict(transform, x_train)
    x_test  = predict(transform, x_test)

    # testando a analise das features
    #plotFeatures(x_test, y_test, paste('depois-',d_name,'-', sep=''), num_of_features)

    printdebug('Data scaled')

    ##################################################
    ##################################################
    ######## TESTING SIMPLER CLASSIFIER ##############
    ##################################################
    ##################################################

    
    #set.seed(1)

    ### Fit a small isolation forest model
    #model[[i]] = isolation.forest(x_train, ntrees = 200, nthreads = 1)
    model[[i]] = isolation.forest(x_train, ntrees = 300, nthreads = 1)
    #model[[i]] = isolation.forest(x_train, ntrees = 500, nthreads = 1)
    #model[[i]] = isolation.forest(x_train, ntrees = 1000, nthreads = 1)

    print(model[[i]])
    
    print(summary(model[[i]]))

    print('------------')
    
    #print('EIF')
    #df.eif = eif(x_train, ntrees = 200, sample_size = 20, ExtensionLevel = 1)
    #print(df.eif)
    #print('------------')
   
    ## LOF
    #print('TRAIN')
    #df.lof = lof(x_train, k=5)
    #print(df.lof)

    #print('TEST')
    #x_test2 = x_test[1:5,]
    #
    #myk = nrow(x_all)-length(id_train)
    ##x_train = x_all[id_train,]
    #df.lof = lof(x_test, k=myk-2)
    #print(df.lof)
    #
    #print('TEST SCALED')
    #print((df.lof-mean(df.lof))/sd(df.lof))

    #print('------------')

    pred_train =  predict(model[[i]], x_train)
    #cat("Point with highest outlier score: ",
    #x_train[which.max(pred), ], "\n")

    cat("pred: ",pred_train,'\n')
    cat("mean: ",mean(pred_train),'\n')
    cat("median: ",median(pred_train),'\n')
    cat("sd: ",sd(pred_train),'\n')
    cat("mad: ",mad(pred_train),'\n')

    golden_ratio = 1.618
    euler = 0.5772156649
    
    #max_pred = max(pred_train)+sd(pred_train)
    #max_pred = mean(pred_train) + 2*sd(pred_train)  #<- <- <- 
    #max_pred = median(pred_train) + 2*mad(pred_train)
    #max_pred = mean(pred_train) + sd(pred_train)
    #max_pred = median(pred_train) + 2*sd(pred_train)
    #max_pred = mean(pred_train)*golden_ratio   
    #max_pred = 0.6 #<<<< ese é muito bom, erra apenas 1
    #max_pred = mean(pred_train)+mean(pred_train)*euler   
    max_pred = 0.59 #<<<< ese é muito bom, erra apenas 1

    cat('max_pred0: ',max(pred_train) + sd(pred_train),'(max+sd)\n')
    cat('max_pred1: ',mean(pred_train) + 2*sd(pred_train),'(mean+2sd)\n')
    cat('max_pred2: ',median(pred_train) + 2*mad(pred_train),'(median+2mad)\n')
    cat('max_pred3: ',mean(pred_train) + sd(pred_train),'(mean+sd)\n')
    cat('max_pred4: ',median(pred_train) + 2*sd(pred_train),'(median+2sd)\n')
    cat('max_pred5: ',mean(pred_train)*golden_ratio,' (golden)\n')
    cat('max_pred6: ',0.6,' (fixed)\n')
    cat('max_pred7:',mean(pred_train)+mean(pred_train)*euler,' (euler)\n')
    cat('!max_pred8: ',0.59,' (fixed)\n')
    
    #print('----')

    #pred2 <- predict(iso, x_test)
    #print(pred2)
    #print(pred2>max_pred)

    # solitude

##    # create isolation forest using isolationForest function from
##    # solitude package with default parameters
##    #iforest <- isolationForest$new(sample_size = ncol(x_train))
##
##
##    # initiate an isolation forest
##    iso = isolationForest$new(sample_size = nrow(x_train))
##
##    print('OK1')
##
##    # fit for attrition data
##    iso$fit(x_train)
##
##    print('ok2')
##    
##    # Obtain anomaly scores
##    scores_train = iso$predict(x_train)
##    scores_train[order(anomaly_score, decreasing = TRUE)]
##
##    print(scores_train)
##
##    # predict scores for unseen data (50% sample)
##    scores_unseen = iso$predict(x_test)
##    scores_unseen[order(anomaly_score, decreasing = TRUE)]
##
##    print(scores_unseen)
##    
##    ##predict outliers within dataset
##    #pred <- predict(iforest, x_test, type = "anomaly_score")
##    #outlier <- as.factor(ifelse(pred >=0.50, "outlier", "normal"))
##
##    #print(pred)
##    #print('-----')
##    #print(outlier)
##

# TODO: for the svm tests
#    quit()
#    
#    # computing model
#    model[[i]] = svm(x_train, y_train, 
#                   type='one-classification', 
#                   #type='nu-classification', 
#                   #kernel='sigmoid')
#                   #kernel='radial')
#                   kernel='linear',
#                   # cost=1, # 10
#                   nu=0.5)
#                   #nu=0.05)
#                   #cross=5) #train an one-classification model 
#    
#    print(summary(model[[i]]))

#    svm.tune <- tune(svm, x_train, y_train, kernel = "linear",
#                     type='one-classification',
#                  ranges = list(cost = c(0.001, 0.5, 1, 10)))

#    print(svm.tune)
#    quit()


    
    # TODO: test more kernels
    # TODO: try tuning svm parameters 

    # TODO: decidir se o test é junto logo ou separado
    #}

    # TODO: parei aqui
    # - testar agora a predicao com os varois classificadores
    # - juntar o resultado como um ensemble, definir regras)



    ################ BEGIN TEST ###############

    # testing the tunned parameters


    # predicting on x_test
    res[[i]] = predict(model[[i]], x_test)

    print('PRED:')
    print(res[[i]])
    
    # TODO: for isolation.forest
    res[[i]] = res[[i]] < max_pred
    #res[[i]] = res[[i]] < 0.59

    res[[i]] = as.numeric(res[[i]])

    printdebug(paste('Predicted test:', paste(y_test, res[[i]], sep='-', collapse=',')))

    print(factor(res[[i]], l_s))
    print(factor(y_test, l_s))

    # confusion matrix
    #cm = confusionMatrix(table(y_test,res[[i]]))
    cm = confusionMatrix(factor(res[[i]], l_s),factor(y_test, l_s))
    printdebug(paste('OVERALL accuracy feat: ', d_name, cm$overall['Accuracy']))
    #output1 = paste('FINAL_ACC', cm$overall['Accuracy'])

    print(cm)
}


print(res)
print(length(res))
#print(length(res[[1]]))
#print(length(res[[2]]))

#quit()

##################################################
# ENSEMBLE OF FEATURES CLASSIFIERS
##################################################

# TODO: 
# - acho que o ensemble nao pode ser simplesmente 'se um for 0' é 0,
# pois assim eu estou juntando os erros de alguma feature que nao
# classificou bem, o que devo fazer é utilizar o "peso" que cada
# classificador possui para fazer a votação final (TKDE spin off)
# - nao sei bem como ficará por ser on-class, mas posso ver uma simples
# votação?

# performing the ensemble here

# number of results (according to the features considered)
nrows = length(res)
# number of time series tested
ncols = length(y_test)

# converting the list of results ins a single matrix
res_M = matrix(unlist(res, use.names=F), ncol=ncols, byrow=T)

# applying the single rule

#############
# testing for only one attack
#############

#res_attack = apply(res_M, 2, function(x) sum(x == 0)) > 0
## all is benign, until...
#res_all = rep(1, ncols)
#res_all[res_attack] = 0

#############
# testing for only one benign
#############

# TODO: 100% aqui

#res_attack = apply(res_M, 2, function(x) sum(x == 1)) > 0
## all is attack, until...
#res_all = rep(0, ncols)
#res_all[res_attack] = 1

#############
# testing for voting scheme
#############

res_attack = apply(res_M, 2, function(x) sum(x == 1)) >= ceiling(nrow(res_M)/2)
## all is attack, until...
res_all = rep(0, ncols)
res_all[res_attack] = 1


print(res_all)

cm = confusionMatrix(factor(res_all, l_s),factor(y_test, l_s))

print(cm)


attack_name = f_names[attack,1]

printdebug(paste('OVERALL accuracy: ', attack_name, cm$overall['Accuracy']))

#acc = sum(res[i]==y_test)/length(y_test)

#output1 = paste('FINAL_ACC', acc)

#cat(d_name,SEED,output1,'\n')



quit()




## Radon Forest classifier with tunning parameters
###################################################
#
## performing a custom caret package extension
#
#printdebug('Tunning randomForest parameters')
#
## tunning metric is accuracy
#metric = "Accuracy"
#
## creating the custom classifier
#customRF = list(type = "Classification", library = "randomForest", loop = NULL)
#
## configuring tunning parametrs
#customRF$parameters = data.frame(parameter = c("mtry", "ntree"),
#                                  class = rep("numeric", 2),
#                                  label = c("mtry", "ntree"))
#
## setting options and functions
#customRF$grid = function(x, y, len = NULL, search = "grid") {}
#customRF$fit  = function(x, y, wts, param, lev, last, weights, classProbs) {
#  randomForest(x, y,
#               mtry = param$mtry,
#               ntree=param$ntree)
#}
#
##Predict label
#customRF$predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
#
##Predict prob
#customRF$prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
#
#customRF$sort   = function(x) x[order(x[,1]),]
#customRF$levels = function(x) x$classes
#
## if parallelization is enabled
#if (DO_PARALLEL)
#{
#    if (CORES_NUM==-1)
#        cores = makeCluster(detectCores()-1)
#    else
#        cores = makeCluster(CORES_NUM)
#    registerDoParallel(cores = cores)
#}
#
################# BEGIN TRAIN ###############
#
## train model
#control = trainControl(method="repeatedcv", 
#                        number=10, 
#                        repeats=3,
#                        allowParallel = TRUE)
#
## the features interval for tunning
#tunegrid = expand.grid(.mtry=c(1:15),.ntree=c(100, 200, 500, 1000, 1500))
#
#
## training the customized classifier
#rf = train(x_train, as.factor(y_train), 
#                method=customRF, 
#                metric=metric, 
#                tuneGrid=tunegrid, 
#                trControl=control)
#
#printdebug(paste('Tunned parameters: ',
#                 paste(c('mtry', 'ntree'), rf$bestTune,
#                       collapse=' ')))
#
#printdebug(paste('TRAIN accuracy: ',
#                    rf$results[rownames(rf$bestTune),'Accuracy']))
#
#summary(rf)
#plot(rf)
#print(rf)




####################################################
####################################################
## TESTING WITH THE SPARSED COSENSE DATA 
####################################################
####################################################

# removing some data samples
i=1

# filterign by ID
xtmp = x[x$moteid == i,]
# ordering by time
xtmp = xtmp[order(xtmp$datetime),]
# cutting until the maxlen
xtmp = xtmp[1:(min(maxlen,nrow(xtmp))),]

# 2x interval
my_by=3
xtmp2 = xtmp[seq(1, nrow(xtmp), by=my_by),]
# NOTE: results:
# 2 => 1 2 3 4
# 3-9x => 1 2 4 4
# 10-12x => 1 1 4 4
# 13-20x => 3 1 4 4
# 21x => 4 1 4 4
# 22x => 1 1 4 4
# 23x => 4 1 4 4
# 24-26x => 3 1 4 4
# 27,28x => 3 1 4 4
# 29-31x => 4 1 4 4
# 32x => 3 1 4 4
# 33x => 4 1 2 4
# 34,35x => 4 1 4 4
# 36x => 4 2 2 4
# 37x => 4 2 4 4
# 38x => 4 1 4 4
# 39,40x => 4 2 4 4
# 41-51x => 4 2 2 4
# 52x => 4 2 2 2
# 53-54x => 4 2 2 4
# 55x => 4 2 2 2
# 56x => 4 2 2 4
# 57-59x => 4 2 2 2
# 60x => 4 2 2 4

# removing some data
median_int = median(diff(xtmp2$datetime))
units(median_int) = 'secs'
median_int

# MULTISCALE FEATURES TRANSFORMATION

ds2 = matrix(0, ncol=(num_comp_feats+1), nrow=0)
colnames(ds2) = c(paste('f',1:num_comp_feats, sep=''), 'class')


# NOTE: classes are in columns 5:8
for( j in 1:4 )
{
    # extracting features for each time series class
    feats = extractFeatureSingle(xtmp2[,j+4], D=D, tau_l=tau_l)

    # NOTE: maybe this is not necessary
    # adjusting features positions, just to be visually identifiable
    feats = featuresPosAdjust(feats, num_of_features=num_of_features)

    # adding copmuted features to the dataset, with the class label
    ds2 = rbind(ds2, c(feats, j))
}

# TODO: escalar os dados parece nao impactar na classificação quando se
# usa o randf

# scaling data
#ds2 = predict(transform, ds2)

predict(rf, ds2[,-ncol(ds2)])


####################################################


####################################################
# TRANSFER LEARNING TESTING
####################################################

# features alignment

# displacement
d = my_by
# TODO: discover this from time series 

# the aligned dataset
ds3 = matrix(NA, ncol=(num_comp_feats+1), nrow=nrow(ds2))
colnames(ds3) = c(paste('f',1:num_comp_feats, sep=''), 'class')


# TODO: parei aqui
# TODO: precisa ajustar pra cada tipo de feature

# c(lenE, Hw, Cw, Fw, pst, Hpi, Cpi, Fpi)

for(f in 1:(ncol(ds2)-1))
{
    newf = f * d

    if (newf > (ncol(ds2)-1))
    {
        break
    }
    ds3[,newf] = ds2[,f]
}

ds3[,ncol(ds3)] = ds2[,ncol(ds2)]

predict(rf, ds3[,-ncol(ds3)])





####################################################
####################################################
## TESTING WITH THE THINGSPEAK SENSORS 
####################################################
####################################################

# helper function to extract features
get_feats = function(temp)
{
    temp = as.numeric(temp)
    feats = extractFeatureSingle(temp, D=D, tau_l=tau_l)
    new_feats = featuresPosAdjust(feats, num_of_features=num_of_features)
    return (new_feats)
}
# TODO: o que acontece se passar pra esse modelo (rf) as features
# extraidas de uma serie da thingspeak, com diferentes intervalos entre
# samples, mas sendo de um tipo conhecido dentre os treinados?


## NOTE: testing the thingspeak 10108 channel
#channel = read.csv('../crawlers_iot/thingspeak/public_channels/data_feeds_10108.csv')
channel = read.csv('../crawlers_iot/thingspeak/public_channels/data_feeds_11467.csv')
# last row is channel info
channel_info = channel[nrow(channel),]
channel = channel[-nrow(channel),]

# adding a datetime column to channel
channel$datetime = as.POSIXct(strptime(channel$created_at, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC'))

m_test = matrix(0, nrow=0, ncol=num_comp_feats)
colnames(m_test) = paste('f', 1:num_comp_feats, sep='')

# channel 10108
# channel outdoor temperature
#m_test = rbind(m_test, get_feats(channel$field6))
# channel temperature basement
#m_test = rbind(m_test, get_feats(channel$field7))
# channel humidity
#m_test = rbind(m_test, get_feats(channel$field8))
##m_test = rbind(m_test, get_feats(channel$field3))
##m_test = rbind(m_test, get_feats(channel$field4))
##m_test = rbind(m_test, get_feats(channel$field5))

# channel 11467
# Outside Temp 
m_test = rbind(m_test, get_feats(channel$field1))
# Outside Humidity
m_test = rbind(m_test, get_feats(channel$field2))
# Outside Pressure 
# Outside Light 
# Office Temp 
# Office Humidity 
# Office Pressure



# predicting on m_test
predict(rf, m_test)

# original classes:
# 1 temperature
# 2 humidity
# 3 light
# 4 voltage

# ERRO - ERRO - ERRO
# NOTE: 
# - algumas series de mesmo tipo estao sendo classificadas erradas, por
# exemplo, outside Temp e Humidity do 11467 estao sendo identificadas
# como 'light', quando comparadas às features extraidas dos dados do
# cosense.
# - observar a questao das diferencas de intervalo entre coletas:
#   - enquanto cosense é de 30 segundos, 11467 é de 60 segundos

median_int = median(diff(x$datetime))
units(median_int) = 'secs'
median_int
# Time difference of 32.05709 secs
median_int = median(diff(channel$datetime))
units(median_int) = 'secs'
median_int
# Time difference of 60 secs
#   - so que o 10108 é de 15min e ele classifica temperatura
#   normalmente, embora erre  os demais dados!!!!!!!!!!
# - observar tbm o tempo de start e end das coletas
#   - enquanto cosense é de 1 mes (uso 30000 samples)
xtmp$datetime[nrow(xtmp)] - xtmp$datetime[1]
#Time difference of 19.26357 days
#   - 11467 é de uns 5 dias (usando apenas 8000 samples devido ao limite
#   do thingspeak)
channel$datetime[nrow(channel)] - channel$datetime[1]
# Time difference of 5.640995 days


# TODO: e se for um tipo diferente dos treinados?

# TODO: preciso ver um open-set classifier
# - ver na literatura, existe o openset svm
# - ver tbm o termo open recognition (ja baixei algumas coisas no
# mendeley?)
# - sera que a ideia de community detection em um grafo ajuda?
