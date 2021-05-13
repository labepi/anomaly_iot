
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

# for isolationForest
library(isotree)

printdebug('Loaded libraries and functions')

# getting command line args
args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0)
{
    # the network measure name
    #d_name = 'HH_L5_radius'
    #d_name = 'MI_dir_L5_mean'
    #d_name = 'MI_dir_L5_variance'
    #d_name = 'H_L5_weight'
    d_name = 'MI_dir_L5_weight'

    # the Bandt-Pompe parameters

    # embedding dimension
    D=3

    # embedding delay
    #tau_l=1:50
    tau_l=1:10

    # default SEED is loaded from config.R
    SEED=1

    # used for creating the dataset of time series
    series_len = 1000

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

    # the series len to create the dataset
    series_len = as.numeric(args[4])
    
    # the device
    device = args[5]

    # the attack data
    attack = as.numeric(args[6])
}

# defining the seed
set.seed(SEED)

# fixing the network measure
d_name = 'MI_dir_L5_weight'

# the percentage of train dataset split
# default is 2/3
train_pct = TRAIN_PCT

printdebug(paste('(D,tau): ',D,',',paste(range(tau_l), collapse=':'), sep=''))

printdebug(paste('SEED:',SEED))

# the devices names
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
num_of_features = 8

# number of initial observations to skip
skip=0

# PREPARING THE MATRICES TO TRAIN AND TEST PHASES
#################################################

# the dataset for all the benign data for train
x_all_train = matrix(NA, nrow=0, ncol=num_of_features*length(tau_l))
y_all_train = c()

# the dataset for all the benign data for test
x_all_test = matrix(NA, nrow=0, ncol=num_of_features*length(tau_l))
y_all_test = c()



#########################################
######### STEPS FOR BENIGN DATA #########
#########################################


################ LOADING BENIGN DATA ###############

# loading benign data from N-BaIoT
x_ben = read.csv(paste(dataset_path, '/', device, '/', f_names[1,1], '.csv', sep=''))



################ PREPARING BENIGN DATA ###############

# filtering by a single feature
x_ben_feat = x_ben[,d_name]

# formatting dataset of series_len time series length
x_ben_df = featureAsDataset(x_ben_feat, series_len, label=1, skip=skip)

# removing class column from dataset matrix
y_ben_df = x_ben_df[,ncol(x_ben_df)]
x_ben_df = x_ben_df[,-ncol(x_ben_df)]

#print(dim(x_ben_df))

################ COMPUTING FEATURES ###############
 
# computing features for the whole dataset, for all time series
x_ben_feats = extractFeatures(x_ben_df, D, tau_l, num_of_features, 
                        showTime=FALSE, na_aware=FALSE, na_rm=FALSE)

# all classes 
y_ben_feats = y_ben_df


################ SPLIT TRAIN/TEST ###############

#TODO: maybe select these train samples randomly?

## define the split rate
# NOTE: using 2/3 for train and 1/3 for test
id_train = 1:round(TRAIN_PCT*nrow(x_ben_feats))

# Splitting datasets
x_train_tmp = x_ben_feats[id_train,]
y_train_tmp = y_ben_feats[id_train]

x_test_tmp = x_ben_feats[-id_train,]
y_test_tmp = y_ben_feats[-id_train]


################ ADDING TO GENERAL DATASET ###############

# train
x_all_train = rbind(x_all_train, x_train_tmp)
y_all_train = c(y_all_train, y_train_tmp)

# test
x_all_test = rbind(x_all_test, x_test_tmp)
y_all_test = c(y_all_test, y_test_tmp)


printdebug(paste('LOADED BENIGN DATA:',device))


#########################################
######### STEPS FOR ATTACK DATA #########
#########################################


################ LOADING ATTACK DATA ###############

# the path to the attack data
filename = paste(dataset_path, '/', device, '/', f_names[attack,1], '.csv', sep='')

if (!file.exists(filename))
{
    printdebug(paste('This device does not have data for the attack:',f_names[attack,1]))
    quit()
}

# loading the attack data from N-BaIoT
x_att = read.csv(filename)


################ PREPARING ATTACK DATA ###############

# filtering by a single feature
x_att_feat = x_att[,d_name]

# #TODO: here the label may change for separating different types of
# attacks

# formatting dataset of series_len time series length
x_att_df = featureAsDataset(x_att_feat, series_len, label=0, skip=skip)

# removing class column from dataset matrix
y_att_df = x_att_df[,ncol(x_att_df)]
x_att_df = x_att_df[,-ncol(x_att_df)]


################ COMPUTING FEATURES ###############

# computing features for the whole dataset, for all time series
x_att_feats = extractFeatures(x_att_df, D, tau_l, num_of_features, 
                            showTime=FALSE, na_aware=FALSE, na_rm=FALSE)

# all classes
y_att_feats = y_att_df


################ ADDING TO GENERAL DATASET ###############

# NOTE: all attack data is going to test
x_all_test = rbind(x_all_test, x_att_feats)
y_all_test = c(y_all_test, y_att_feats)

printdebug(paste('LOADED ATTACK DATA:',device,'-',f_names[attack,1]))



print(dim(x_all_train))
print(dim(x_all_test))


###############################################
################ CLASSIFICATION ###############
###############################################

max_pred = 0.59

################ TRAINING PHASE ###############

# TODO: number of trees should be from settings before

### Fit a small isolation forest model
model = isolation.forest(x_all_train, ntrees = 300, nthreads = 1)

print(model)
    
print(summary(model))


################ TESTING PHASE ###############


# predicting on x_test
res = predict(model, x_all_test)

print('PRED:')
print(res)
    
# TODO: for isolation.forest

# benign is 1 and attack is 0
res = res < max_pred
res = as.numeric(res)

printdebug(paste('Predicted test:', paste(y_all_test, res, sep='-', collapse=',')))

# preparing the parameters for confusion matrix 
l_s = c("1", "0")

# confusion matrix
cm = confusionMatrix(factor(res, l_s),factor(y_all_test, l_s))
printdebug(paste('OVERALL accuracy',f_names[attack,1],':', cm$overall['Accuracy']))

print(cm)


