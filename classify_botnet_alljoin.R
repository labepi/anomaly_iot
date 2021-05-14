
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

# package for isolationForest
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
#    device = 'Danmini_Doorbell'
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
    #device = args[5]

    # the attack data
    attack = as.numeric(args[5])
}

# defining the seed
set.seed(SEED)

# the percentage of train dataset split
# default is 2/3
train_pct = TRAIN_PCT # from config.R

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

# the network measure to consider
d_name = 'MI_dir_L5_weight'

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

# LOADING BENIGN DATA FROM ALL DEVICES
######################################

for (device in dev_names)
{

    ################ LOADING BENIGN DATA ###############

    # the path to the benign data
    filename = paste(dataset_path, '/', device, '/', f_names[1,1], '.csv', sep='')

    if (!file.exists(filename))
    {
        next
    }
    
    # loading the data
    x_ben = read.csv(filename)

    # filtering by a single feature
    x_ben_feat = x_ben[,d_name]

    # transforming a single feature vector in a dataset
    # organizing the feature as a matrix
    x_ben_df = featureAsDataset(x_ben_feat, series_len, label=1, skip=skip)

    # removing class column from dataset matrix
    y_ben_df = x_ben_df[,ncol(x_ben_df)]
    x_ben_df = x_ben_df[,-ncol(x_ben_df)]

    ################ COMPUTING FEATURES ###############
    
    # computing features for the whole dataset, for all time series
    x_all_tmp = extractFeatures(x_ben_df, D, tau_l, num_of_features, 
                            showTime=FALSE, na_aware=FALSE, na_rm=FALSE)

    # all classes 
    y_all_tmp = y_ben_df

    ################ SPLIT TRAIN/TEST ###############

    #TODO: maybe select these train samples randomly?
    
    ## define the split rate
    # NOTE: using 2/3 for train and 1/3 for test
    id_train = 1:round((2/3)*nrow(x_all_tmp))

    # Splitting datasets
    x_train_tmp = x_all_tmp[id_train,]
    y_train_tmp = y_all_tmp[id_train]

    x_test_tmp = x_all_tmp[-id_train,]
    y_test_tmp = y_all_tmp[-id_train]

    ################ ADDING TO GENERAL DATASET ###############
    
    # train
    x_all_train = rbind(x_all_train, x_train_tmp)
    y_all_train = c(y_all_train, y_train_tmp)
    
    # test
    x_all_test = rbind(x_all_test, x_test_tmp)
    y_all_test = c(y_all_test, y_test_tmp)

    printdebug(paste('LOADED BENIGN:',device))
}

#quit()

print(dim(x_all_train))
print(dim(x_all_test))


# LOADING ATTACK DATA FROM ALL DEVICES
######################################

for (device in dev_names)
{
    # according to f_names
    for(attack in 2:11)
    {
    
        ################ LOADING ATTACK DATA ###############
        
        # the path to the attack data
        filename = paste(dataset_path, '/', device, '/', f_names[attack,1], '.csv', sep='')

        if (!file.exists(filename))
        {
            next
        }
    
        # loading the data
        x_att = read.csv(filename)

        # filtering by a single feature
        x_att_feat = x_att[,d_name]

        # TODO: here the label may change for separating different types of
        # attacks

        # formatting dataset of series_len time series length
        x_att_df = featureAsDataset(x_att_feat, series_len, label=0, skip=skip)

        # removing class column from dataset matrix
        y_att_df = x_att_df[,ncol(x_att_df)]
        x_att_df = x_att_df[,-ncol(x_att_df)]

        #print(dim(x_att_df))

        ################ COMPUTING FEATURES ###############

        # computing features for the whole dataset, for all time series
        x_all_att = extractFeatures(x_att_df, D, tau_l, num_of_features, 
                                    showTime=FALSE, na_aware=FALSE, na_rm=FALSE)

        # all classes
        y_all_att = y_att_df

        ################ ADDING TO GENERAL DATASET ###############

        # NOTE: all attack data is going to test
        x_all_test = rbind(x_all_test, x_all_att)
        y_all_test = c(y_all_test, y_all_att)

        printdebug(paste('LOADED ATTACK:',device,'-',f_names[attack,1]))

    }
}

print(dim(x_all_train))
print(dim(x_all_test))

#quit()

################ SCALING DATA ###############

printdebug('Scaling data')

# TODO: we have to scale the computed features 
# preprocesing the features dataset
transform = preProcess(x_all_train, method=c("center", "scale"))

x_all_train = predict(transform, x_all_train)
x_all_test  = predict(transform, x_all_test)



###############################################
################ CLASSIFICATION ###############
###############################################

max_pred = 0.59

################ TRAINING PHASE ###############


# Fit a small isolation forest model
#model[[i]] = isolation.forest(x_train, ntrees = 200, nthreads = 1)
model = isolation.forest(x_all_train, ntrees = 300, nthreads = 1)
#model[[i]] = isolation.forest(x_train, ntrees = 500, nthreads = 1)
#model[[i]] = isolation.forest(x_train, ntrees = 1000, nthreads = 1)

print(model)
print(summary(model))


################ TESTING PHASE ###############


# testing the tunned parameters


# predicting on x_test
res = predict(model, x_all_test)

print('PRED:')
print(res)

# TODO: for isolation.forest

# benign is 1 and attack is 0
res = res < max_pred
res = as.numeric(res)

printdebug(paste('Predicted test:', paste(y_all_test, res, sep='-', collapse=',')))

#print(factor(res[[i]], l_s))
#print(factor(y_test, l_s))

# preparing the parameters for confusion matrix 
l_s = c("1", "0")

# confusion matrix
cm = confusionMatrix(factor(res, l_s),factor(y_all_test, l_s))
printdebug(paste('OVERALL accuracy feat: ', cm$overall['Accuracy']))

print(cm)

