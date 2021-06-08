
library(ggplot2)
library(ggsci)

D=3

#####################################
######## SINGLE
#####################################

# ALL

x = read.table('accuracy_single_all.txt')

colnames(x) = c('num', 'acc', 'sd', 'len', 'Device')

# filtering to clean visualization
#x2 = x[x[,1] %in% seq(200, 1000, 200),]
#x2 = x[x[,1] %in% seq(400, 1000, 200),]
#x2 = x[x[,1] %in% seq(300, 1000, 100),]

df = as.data.frame(x)
#df = as.data.frame(x2)
df$Device = factor(df$Device)

df$acc = as.numeric(df$acc)
df$sd = as.numeric(df$sd)

N = 10

# Calculate standard error of the mean
# standard error
df$se = df$sd/sqrt(N)

# Confidence interval multiplier for standard error
conf.interval = 0.95
# Calculate t-statistic for confidence interval:
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
ciMult = qt(conf.interval/2 + .5, N-1)
df$ci = df$se * ciMult

#df

p = ggplot(df, aes(x=as.factor(num),y=acc,group=Device,color=Device,shape=Device)) + #,linetype=Device
        geom_line(size=1.5) + geom_point(size=7) +
        geom_errorbar(aes(ymin=acc-ci, ymax=acc+ci), width=.2) +
        theme_bw(base_size=24) + scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        theme(legend.position="bottom") + 
        guides(color=guide_legend(nrow=1), shape=guide_legend(nrow=1)) 
        #coord_cartesian(ylim=c(0.5, 1.00)) 

ggsave(paste('img/fig_accuracy_single_all_line_D',D,'.pdf',sep=''), p, width=12, height=7)

#x2 = x[x[,1] %in% seq(400, 1000, 200),]
#df = as.data.frame(x2)
#df$Device = factor(df$Device)

#df = df[df$num %in% seq(400, 1000, 200),]
df = df[df$num %in% seq(200, 1000, 200),]

p = ggplot(df, aes(x=as.factor(num),y=acc,group=Device,fill=Device)) +
        geom_bar(stat='identity',position=position_dodge(), width=0.9) + #, color='black'
        geom_errorbar(aes(ymin=acc-ci, ymax=acc+ci), width=.4,
                 position=position_dodge(.9), color='black') +
        theme_bw(base_size=24) + scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        #scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        #scale_fill_brewer(palette = "Oranges", direction=-1) +
        #scale_fill_brewer(palette = "Blues", direction=-1) +
        #scale_fill_brewer(palette = "Greys") +
        coord_cartesian(ylim=c(0.6, 1.00)) +
        #coord_cartesian(ylim=c(0.95, 1.00)) +
        theme(legend.position="bottom") + 
        guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) 

ggsave(paste('img/fig_accuracy_single_all_bar_D',D,'.pdf',sep=''), p, width=12, height=6)
ggsave(paste('img/fig_accuracy_single_all_barlarge_D',D,'.pdf',sep=''), p, width=30, height=6)


#######

# SINGLE PER ATTACK

x = read.table('accuracy_single_all_raw.txt')

colnames(x) = c('num', 'Operation', 'acc', 'Device')

num_len=200
#dev_id=6
dev_id=1

# filtering to a single device and number
x2 = x[x$num == num_len & x$Device == dev_id,]

operation = c(
#            'Benign',
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

df = as.data.frame(x2)
df$Device = factor(df$Device)
df$Attack = factor(1:10, labels=operation)
df$acc = as.numeric(df$acc)
df$num = factor(df$num)
df$accf = sprintf("%.3f", df$acc)

p = ggplot(df, aes(x=Attack,y=acc,group=Device,fill=Device)) +
        geom_bar(stat='identity',position=position_dodge(), fill='#aec7e8ff', color='black') +
        #geom_line(size=1.5) + geom_point(size=5) +  #afd3f1
        geom_text(aes(label=accf), vjust=2.5, color="black",
            position = position_dodge(0.9), size=8)+
        theme_bw(base_size=24) + 
        scale_shape_manual(values=15:24) +
        xlab('Attack per device') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, hjust=1))
 
ggsave(paste('img/fig_accuracy_single_device',dev_id,'_num',num_len,'_bar_D',D,'.pdf',sep=''), p, width=12, height=6)

#################


# MIRAI

x = read.table('accuracy_single_mirai.txt')

colnames(x) = c('num', 'acc', 'sd', 'len', 'Device')

# filtering to clean visualization
x2 = x[x[,1] %in% seq(200, 1000, 200),]

df = as.data.frame(x2)

df$Device = factor(df$Device)

# removing devices without mirai
df = df[!df$Device %in% c(3,7),]

p = ggplot(df, aes(x=as.factor(num),y=acc,group=Device,color=Device,shape=Device)) + #,linetype=Device
        geom_line(size=1.5) + geom_point(size=5) +
        geom_errorbar(aes(ymin=acc-sd, ymax=acc+sd), width=.2) +
        theme_bw(base_size=24) + scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        theme(legend.position="bottom") + 
        guides(color=guide_legend(nrow=1), shape=guide_legend(nrow=1)) 
        #coord_cartesian(ylim=c(0.5, 1.00)) 

ggsave(paste('img/fig_accuracy_single_mirai_line_D',D,'.pdf',sep=''), p, width=12, height=8)


p = ggplot(df, aes(x=as.factor(num),y=acc,group=Device,fill=Device)) +
        geom_bar(stat='identity',position=position_dodge()) +
        geom_errorbar(aes(ymin=acc-sd, ymax=acc+sd), width=.4,
                 position=position_dodge(.9)) +
        theme_bw(base_size=24) + scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        coord_cartesian(ylim=c(0.5, 1.00)) +
        theme(legend.position="bottom") + 
        guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) 

ggsave(paste('img/fig_accuracy_single_mirai_bar_D',D,'.pdf',sep=''), p, width=12, height=8)



# GAFGYT

x = read.table('accuracy_single_gafgyt.txt')

colnames(x) = c('num', 'acc', 'sd', 'len', 'Device')

# filtering to clean visualization
x2 = x[x[,1] %in% seq(200, 1000, 200),]

df = as.data.frame(x2)

df$Device = factor(df$Device)

p = ggplot(df, aes(x=as.factor(num),y=acc,group=Device,color=Device,shape=Device)) + #,linetype=Device
        geom_line(size=1.5) + geom_point(size=5) +
        geom_errorbar(aes(ymin=acc-sd, ymax=acc+sd), width=.2) +
        theme_bw(base_size=24) + scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        theme(legend.position="bottom") + 
        guides(color=guide_legend(nrow=1), shape=guide_legend(nrow=1)) 
        #coord_cartesian(ylim=c(0.5, 1.00)) 

ggsave(paste('img/fig_accuracy_single_gafgyt_line_D',D,'.pdf',sep=''), p, width=12, height=8)


p = ggplot(df, aes(x=as.factor(num),y=acc,group=Device,fill=Device)) +
        geom_bar(stat='identity',position=position_dodge()) +
        geom_errorbar(aes(ymin=acc-sd, ymax=acc+sd), width=.4,
                 position=position_dodge(.9)) +
        theme_bw(base_size=24) + scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        coord_cartesian(ylim=c(0.4, 1.00)) + 
        theme(legend.position="bottom") + 
        guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) 

ggsave(paste('img/fig_accuracy_single_gafgyt_bar_D',D,'.pdf',sep=''), p, width=12, height=8)


#####################################
######## ALLJOIN
#####################################

# ONLY D3 TAU 10

x = read.table('accuracy_alljoin_raw.txt')

colnames(x) = c('num', 'acc')

df = as.data.frame(x)

df$num = factor(df$num)
df$acc = as.numeric(df$acc)
df$Device = factor(1)
df$accf = sprintf("%.3f", df$acc)

p = ggplot(df, aes(x=num,y=acc, group=Device,color=Device,shape=Device), linetype=1) + #,linetype=Device
        geom_bar(stat='identity',position=position_dodge(), fill='#aec7e8ff', color='black') +
        geom_line(size=1.5) + geom_point(size=5) +  #afd3f1
        geom_text(aes(label=accf), vjust=2.5, color="black",
            position = position_dodge(0.9), size=8)+
        theme_bw(base_size=24) + 
        scale_shape_manual(values=15:24) +
        xlab('Length of time series') +
        ylab('Accuracy') +
        scale_color_d3(palette="category20") +
        scale_fill_d3(palette="category20") +
        theme(legend.position="none") 
        #coord_cartesian(ylim=c(0.5, 1.00)) 

ggsave(paste('img/fig_accuracy_alljoin_linebar_D',D,'.pdf',sep=''), p, width=12, height=6)


# VARYING D and TAU

x = read.table('accuracy_alljoin_raw_multi.txt')

colnames(x) = c('num', 'acc', 'D', 'tau')

df = as.data.frame(x)

df$Parameters = factor(paste(df$D, df$tau, sep='-'))

df$num = factor(df$num)
df$acc = as.numeric(df$acc)
df$D = factor(df$D)
df$tau = factor(df$tau)
df$accf = sprintf("%.3f", df$acc)

# removing other numbers
df$accf[df$Parameters != '3-10'] = NA

#new_labels = list(
#               expression('D=3,'*tau*'=10'),
#               expression('D=3,'*tau*'=5'),
#               expression('D=4,'*tau*'=10'),
#               expression('D=4,'*tau*'=5')
#)
new_labels = list(
               'D=3, t=10',
               'D=3, t=5',
               'D=4, t=10',
               'D=4, t=5'
)

p = ggplot(df, aes(x=num,y=acc, group=Parameters, color=Parameters, shape=Parameters,linetype=Parameters)) + #
        geom_line(size=1.5, show.legend=FALSE) + geom_point(size=5) +  #afd3f1
        geom_text(aes(label=accf), vjust=-1.5, color="black",
            position = position_dodge(0.2), size=8)+
        theme_bw(base_size=24) + 
        xlab('Length of time series') +
        ylab('Accuracy') +
        theme(legend.position="bottom") +
        scale_shape_manual(labels=new_labels, values=15:19) +
        scale_linetype_discrete(labels=new_labels) +
        scale_color_d3(palette="category20", labels=new_labels) +
        coord_cartesian(ylim=c(0.5, 1.11)) 

ggsave(paste('img/fig_accuracy_alljoin_var_Dtau.pdf',sep=''), p, width=12, height=6)


###########
# sensitivity and specificity
############

x1 = read.table('sensitivity_alljoin_raw_multi.txt')
x1 = cbind(x1, 1)
colnames(x1) = c('num', 'val', 'D', 'tau', 'Metric')

x2 = read.table('specificity_alljoin_raw_multi.txt')
x2 = cbind(x2, 2)
colnames(x2) = c('num', 'val', 'D', 'tau', 'Metric')

x = rbind(x1, x2)

# filtering for the result with best accuracy
x = x[x$D == 3 & x$tau==10,]

df = as.data.frame(x)

df$num = factor(df$num)
df$val = as.numeric(df$val)
df$valf = sprintf("%.3f", df$val)
df$D = factor(df$D)
df$tau = factor(df$tau)
df$Metric = factor(df$Metric, labels=c('Sensitivity', 'Specificity'))
df$vjust = c(rep(-1.5, 9), rep(2.5, 9))

p = ggplot(df, aes(x=num,y=val, group=Metric, color=Metric, shape=Metric, linetype=Metric)) + #
        geom_line(size=1.5, show.legend=FALSE) + geom_point(size=5) +  #afd3f1
        geom_text(aes(label=valf, color=Metric, vjust=vjust), #vjust=-1.5, #color="black",
            position = position_dodge(0.2), size=8, show.legend = FALSE)+
        theme_bw(base_size=24) + 
        xlab('Length of time series') +
        ylab('Value') +
        theme(legend.position="bottom") +
        scale_shape_manual(values=15:19) +
        scale_linetype_discrete() + 
        scale_color_d3(palette="category20") +
        coord_cartesian(ylim=c(0.5, 1.11)) 


ggsave(paste('img/fig_sens_spec_alljoin_var_Dtau.pdf',sep=''), p, width=12, height=6)


########################


