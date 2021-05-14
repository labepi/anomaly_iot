
library(ggplot2)
library(ggsci)

D=3


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

df = df[df$num %in% seq(400, 1000, 200),]

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
        coord_cartesian(ylim=c(0.95, 1.00)) +
        theme(legend.position="bottom") + 
        guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) 

ggsave(paste('img/fig_accuracy_single_all_bar_D',D,'.pdf',sep=''), p, width=12, height=6)
ggsave(paste('img/fig_accuracy_single_all_barlarge_D',D,'.pdf',sep=''), p, width=30, height=6)


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


