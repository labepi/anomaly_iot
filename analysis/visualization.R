# visualizando os datasets

library(caret)
library(ggplot2)
library(reshape2)

source('../utils.R')

bandt_pompe_path = '../../bandt_pompe'

# loading the bandt-pompe functions
loadSource(bandt_pompe_path, 'bandt_pompe.R')

# loading the transition graph functions
loadSource(bandt_pompe_path, 'bandt_pompe_graph.R')

# loading the measures functions
loadSource(bandt_pompe_path, 'measures.R')

# loading helper functions
loadSource(bandt_pompe_path, 'helpers.R')


#feat1 = 'MI_dir'
feat1 = 'H'
feat2 = '0.01'
feat3 = 'weight'

feat2_l = c('0.01', '0.1', '1', '3', '5')

par(mfrow=c(2,3))

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


loadFeat = function(feat1 = "MI_dir_L5_weight", num=1000, scale=TRUE)
{
    x = read.csv(paste('../data/botnet/',num,'/',feat,'.csv', sep=''), header=F)
    x = apply(x, 2, as.numeric)

    classes = x[,ncol(x)]
    x = x[,-ncol(x)]

    # calculate the pre-process parameters from the dataset
    preprocessParams = preProcess(x, method=c("center", "scale"))

    # transform the dataset using the parameters
    if (scale==TRUE)
    {
        x = predict(preprocessParams, x)
    }

    attr(x, 'classes') = classes

    return(x)
}

# NOTE: como visto acima, os fenomenos parecem variar mais para L5, o
# que parece ser com intervalos de time window de 100ms

matplot2 = function(data)
{
    #ggplot needs a dataframe
    data = as.data.frame(data)

    #id variable for position in matrix 
    data$id = 1:nrow(data) 
    
    #reshape to long format
    plot_data = melt(data,id.var="id")

    plot_data$x = rep(1:(ncol(data)-1), each=nrow(data))

    plot_data$id = as.factor(plot_data$id)

    ggplot(plot_data, aes(x=x,y=value,group=id,colour=id)) +
      geom_point()+
      geom_line(aes(lty=id))
}

feat1 = 'MI_dir'
#feat1 = 'H'
#feat1 = 'HH'
#feat1 = 'HpHp'
#feat2 = '0.01'
#feat2 = '1'
feat2 = '5'
feat3 = 'weight'
#feat3 = 'mean'
#feat3 = 'radius'

feat = paste(feat1, '_L', feat2, '_', feat3, sep='')

x = loadFeat(feat)

matplot2(x[j_l,1:100])


#matplot(t(x1[j_l,1:1000]), type='b', pch=1:11)

# TODO:
# - agora testar o bandt-pompe com esses valores

D=3
tau=1

HC = matrix(NA, ncol=3, nrow=0)

for(i in 1:nrow(x))
{
    HC = rbind(HC, complexity_entropy(x[i,], D=D, tau=tau))
}

classes = attr(x, 'classes')

plot.ccep(D=D, ylim=c(0,0.5))
points(HC[,1], HC[,2], col=classes, pch=classes)


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

