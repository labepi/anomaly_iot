
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


printdebug('Loaded libraries and functions')


# getting command line args
args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0)
{
    # TODO: precisa criar o dataset no formato adequado?
    
    # NOTE: this is the dataset from the Intel Lab, only filtering for
    # the lines with 8 columns

    # dataset name
    #d_name = 'HH_L5_radius.csv' 
    d_name = 'MI_dir_L5_weight.csv' 

    # the Bandt-Pompe parameters

    # embedding dimension
    D=3

    # embedding delay
    #tau_l=1:50
    tau_l=1:10

    # default SEED is loaded from config.R
    #SEED=1

    # used for loading the time series
    series_len = 1000
    #series_len = 5000

    # trying a single model for device
    device = 'Danmini_Doorbell'

} else {
    # loading the datasets
    d_name = args[1]
    
    # the Bandt-Pompe parameters

    # embedding dimension
    D = as.numeric(args[2])
    
    # embedding delay (end of list)
    tau_l = 1:as.numeric(args[3])

    # the seed to random
    SEED = as.numeric(args[4])

    # the series len to use (1000 or 5000)
    series_len = as.numeric(args[5])
}

# defining the seed
set.seed(SEED)

# the percentage of train dataset split
train_pct = TRAIN_PCT # from config.R

printdebug(d_name)

printdebug(paste('(D,tau): ',D,',',paste(range(tau_l), collapse=':'), sep=''))

printdebug(paste('SEED:',SEED))



#######################################
# LOADING DATA AND COMPUTING FEATURES #
#######################################


dataset_path = './data/botnet'

#d_name = 'HH_L1_radius.csv' 
#d_name = 'HH_L5_radius.csv' 

# these are prepared botnet data, from N-BaIoT
x = read.csv(paste(dataset_path, '/', series_len, '/', d_name, sep=''), header=F)

# columns
# 1..series_len - series
# series_len+1 - class

# TODO: testing the two-class case: benign or malign
#x[,series_len+1][x[,series_len+1] != 1] = 2

# number of computed features per tau
num_of_features = 8

# NOTE: +1 to add the class type
# the dataset result of computed features
num_comp_feats = num_of_features*(length(tau_l))
ds = matrix(0, ncol=(num_comp_feats+1), nrow=0)
colnames(ds) = c(paste('f',1:num_comp_feats, sep=''), 'class')

# for each ID
for( i in 1:nrow(x) )
{
    # filtering
    xtmp = x[i,1:series_len]

    # MULTISCALE FEATURES TRANSFORMATION

    # extracting features for each time series class
    feats = extractFeatureSingle(xtmp, D=D, tau_l=tau_l)

    # NOTE: maybe this is not necessary
    # adjusting features positions, just to be visually identifiable
    feats = featuresPosAdjust(feats, num_of_features=num_of_features)

    # adding copmuted features to the dataset, with the class label
    ds = rbind(ds, c(feats, x[i,series_len+1]))

}


# all classes 
y_all = ds[,ncol(ds)]

# all the time series
x_all = ds[,-ncol(ds)]


################ SPLIT TRAIN/TEST ###############

## define the split rate
#id_train = createDataPartition(y=y_all, p=train_pct, list=FALSE)
#
## Splitting datasets
#x_train = x_all[id_train,]
#y_train = y_all[id_train]
#
#x_test = x_all[-id_train,]
#y_test = y_all[-id_train]


# TODO: testing the one-class classification

id_train = which(y_all == 1)

x_train = x_all[id_train,]  #choose only one of the classes
y_train = y_all[id_train]

#x <- subset(df, select = -Species) #make x variables
#y <- df$Species #make y variable(dependent)

x_test = x_all
y_test = y_all
y_test[y_test != 1] = 0


# TODO: fazer um script tbm para gerar graficos das features das series
# de cosense


################ SCALING DATA ###############

printdebug('Scaling data')

# TODO: we have to scale the computed features 
# preprocesing the features dataset
transform = preProcess(x_train, method=c("center", "scale"))

x_train = predict(transform, x_train)
x_test  = predict(transform, x_test)

printdebug('Data scaled')




##################################################
##################################################
######## TESTING SIMPLER CLASSIFIER ##############
##################################################
##################################################



rf = svm(x_train, y_train, type='one-classification') #train an one-classification model 


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

################ BEGIN TEST ###############

# testing the tunned parameters


# predicting on x_test
res = predict(rf, x_test)

res = as.numeric(res)

printdebug(paste('Predicted test:', paste(y_test, res, sep='-', collapse=',')))

# confusion matrix
cm = confusionMatrix(table(y_test,res))
printdebug(paste('OVERALL accuracy: ', cm$overall['Accuracy']))
#output1 = paste('FINAL_ACC', cm$overall['Accuracy'])

print(cm)

acc = sum(res==y_test)/length(y_test)

output1 = paste('FINAL_ACC', acc)

cat(d_name,SEED,output1,'\n')




quit()




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
