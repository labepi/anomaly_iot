# visualizando os datasets

library(caret)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
# color palettes
library(ggsci)

source('../includes.R')
source('../utils.R')
source('../features.R')

bandt_pompe_path = '../../bandt_pompe'

# loading the bandt-pompe functions
loadSource(bandt_pompe_path, 'bandt_pompe.R')

# loading the transition graph functions
loadSource(bandt_pompe_path, 'bandt_pompe_graph.R')

# loading the measures functions
loadSource(bandt_pompe_path, 'measures.R')

# loading helper functions
loadSource(bandt_pompe_path, 'helpers.R')

###################################
# PLOTTING THE MULTISCAPE FEATURES
###################################

feat1 = 'MI_dir'
#feat1 = 'H'
#feat2 = '0.01'
feat2 = '5'
feat3 = 'weight'

feat2_l = c('0.01', '0.1', '1', '3', '5')

#par(mfrow=c(2,3))

j=4
j_l=1:11

#for (i in feat2_l)
#{
#    feat = paste(feat1, '_L', i, '_', feat3, sep='')
#
#    x1 = read.csv(paste('../data/botnet/1000/',feat,'.csv', sep=''), header=F)
#    x1 = apply(x1, 2, as.numeric)
#
#    classes = x1[,ncol(x1)]
#    x1 = x1[,-ncol(x1)]
#
#    # calculate the pre-process parameters from the dataset
#    preprocessParams = preProcess(x1, method=c("center", "scale"))
#
#    # summarize transform parameters
#    #print(preprocessParams)
#    
#    # transform the dataset using the parameters
#    x1 = predict(preprocessParams, x1)
#
#}

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

# TODO: loading the pre-isolated features
loadFeat = function(feat1 = "MI_dir_L5_weight", num=1000, scale=TRUE)
{
    x = read.csv(paste('../data/botnet/',num,'/',feat,'.csv', sep=''), header=F)
    x = apply(x, 2, as.numeric)

    classes = x[,ncol(x)]
    x = x[,-ncol(x)]


    # transform the dataset using the parameters
    if (scale==TRUE)
    {
        # calculate the pre-process parameters from the dataset
        preprocessParams = preProcess(x, method=c("center", "scale"))

        x = predict(preprocessParams, x)
    }

    attr(x, 'classes') = classes

    return(x)
}

# NOTE: como visto acima, os fenomenos parecem variar mais para L5, o
# que parece ser com intervalos de time window de 100ms

matplot2 = function(data, linelegend=TRUE)
{
    #ggplot needs a dataframe
    data = as.data.frame(data)

    #id variable for position in matrix 
    data$id = 1:nrow(data) 
    
    #reshape to long format
    plot_data = melt(data,id.var="id")

    plot_data$x = rep(1:(ncol(data)-1), each=nrow(data))

    plot_data$id = as.factor(plot_data$id)
    plot_data$Operation = factor(plot_data$id, labels=operation)

    p = ggplot(plot_data, aes(x=x,y=value,group=Operation,colour=Operation)) +
        #geom_line(size=1.5) +
        geom_line(size=1.5, show.legend=linelegend) +
        theme_bw() + 
        scale_color_d3(palette="category20")

    #p = ggplot(plot_data, aes(x=x,y=value,group=id,colour=id,shape=id)) +
      #geom_point()+
      #geom_line(aes(lty=id), size=2) +
    
    return(p)
}


operation = c(
            'Benign',
            'Mirai/ACK',
            'Mirai/SCAN',
            'Mirai/SYN',
            'Mirai/UDP',
            'Mirai/UDPPLAIN',
            'Bashlite/COMBO',
            'Bashlite/JUNK',
            'Bashlite/SCAN',
            'Bashlite/TCP',
            'Bashlite/UDP'
        )


###################################
# PLOTTING THE RAW SERIES
###################################

feat1 = 'MI_dir'
feat2 = '5'
feat3 = 'weight'

feat = paste(feat1, '_L', feat2, '_', feat3, sep='')

x = loadFeat(feat, scale=F, num=num_rows)
#x = loadFeat(feat, scale=F)

p = matplot2(x[j_l,101:500])
p = p + xlab('Number of samples') + ylab('MI_dir_L5_weight')
p = p + theme_bw(base_size=24) + theme(legend.position="bottom")

ggsave(paste('img/fig_feat_operation-',feat,'.pdf',sep=''), p, width=12, height=8)


###################################
# PLOTTING THE CCEP
###################################

# TODO:
# - agora testar o bandt-pompe com esses valores

x = loadFeat(feat, scale=T, num=num_rows)

D=3
tau=1

HC = matrix(NA, ncol=3, nrow=0)

#for(i in 1:nrow(x))
for(i in j_l)
{
    HC = rbind(HC, complexity_entropy(x[i,], D=D, tau=tau))
}

H_SC = data.frame(x=HC[,1], y=HC[,2])
H_SC$Operation = factor(j_l, labels=operation)
    
# plotting the ccep for the classes

#p = gplot.ccep(H=HC[,1], SC=HC[,2], D=3, col=j_l, shp=j_l, xlim=c(0.5,1), ylim=c(0,0.25))
p = gplot.ccep(D=3, xlim=c(0.0,1), ylim=c(0,0.3))

p = p + geom_point(aes(x, y, color=Operation, shape=Operation), data=H_SC, size=7) + 
    scale_color_d3(palette="category20") + scale_shape_manual(values=j_l+14) +
    xlab(expression('Normalized Permutation Entropy ('*H[S]*'['*p[pi]*'])')) +
    ylab(expression('Statistical Complexity ('*C[JS]*'['*p[pi]*'])')) + 
    theme_bw(base_size=24) + theme(legend.position="bottom")

ggsave(paste('img/fig_ccep_D',D,'-',feat,'.pdf',sep=''), p, width=12, height=8)


# TODO: parei aqui,
#   - usar o expression pra organizar os labels acima
#   - colocar labels na figura anterior tbm, das series
#   - fazer uma figura com as features extraidas (2x4)


##classes = attr(x, 'classes')
#classes = j_l
#
#pdf(paste('img/fig_ccep-',feat,'.pdf',sep=''), width=6, height=6)
#
#plot.ccep(D=D, xlim=c(0.5,1), ylim=c(0,0.4))
#points(HC[,1], HC[,2], col=classes, pch=classes)
#
#dev.off()

###################################
# PLOTTING THE MULTISCALE FEATURES
###################################

D=3
tau_l = 1:10
num_of_features=8

x_feats = extractFeatures(x[j_l,], D, tau_l, num_of_features, 
                        showTime=FALSE, na_aware=FALSE, na_rm=FALSE)


# Pst feature
d=5 # <- pst
p = matplot2(x_feats[,1:10 + (d-1)*10], lineleg=FALSE) + 
    geom_point(aes(shape=Operation), size=5) + scale_shape_manual(values=j_l+14) +
    xlab(expression(tau)) + ylab(expression(p[st])) + 
    theme_bw(base_size=24) + theme(legend.position="bottom") +
    scale_x_discrete(limits=1:10)

ggsave(paste('img/fig_feature_pst_D',D,'-',feat,'.pdf',sep=''), p, width=12, height=6)

###################3

# TODO: 
# - acho que farei um multiscale aqui
# - parece que vai ser mais um spin-off do tkdd
# - usar metodos do class_ucr:
#   - analysis/features_transformation.R tem os metodos


# TODO:
# - precisa definir a estrategia dos experimentos
# 1. classificação com todas as classes
# 2. classificação binaria
# 3. treinamento apenas com benigno e detecção da anomalia
# 4. tempo de detecção, importante junto com a acurácia
# 5. reproduzir as mesmas configurações dos trabalhos para comparação
#   - NBaIoT e Sensors 2020

feats = c(
    "MI_dir_L5_weight",
    "MI_dir_L5_mean",
    "MI_dir_L5_variance",
    "H_L5_weight",
    "H_L5_mean",
    "H_L5_variance",
    "HH_L5_weight",
    "HH_L5_mean",
    "HH_L5_std",
    "HH_L5_magnitude",
    "HH_L5_radius",
    "HH_L5_covariance",
    "HH_L5_pcc",
    "HH_jit_L5_weight",
    "HH_jit_L5_mean",
    "HH_jit_L5_variance",
    "HpHp_L5_weight",
    "HpHp_L5_mean",
    "HpHp_L5_std",
    "HpHp_L5_magnitude",
    "HpHp_L5_radius",
    "HpHp_L5_covariance",
    "HpHp_L5_pcc"
)

j_l = 1:11

for (feat in feats)
{
    x = loadFeat(feat, scale=T) #F
    p = matplot2(x[j_l,101:500])
    #ggsave(paste('img/matplot/',feat,'.png',sep=''), p)
    ggsave(paste('img/matplot_scaled/',feat,'.png',sep=''), p, width=10, height=6)
}


