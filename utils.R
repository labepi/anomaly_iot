# load a source file by its path and file name
loadSource = function(path, file)
{
    source(
        paste(
            path,
            file,
            sep='/'
            )
        )
}

# loads a station data by its 'name'
# returns the matrix of data 
# other fields as attributes:
# y: the classes
# name: the name of dataset
loadStation = function(name, begin=NULL, end=NULL)
{
    # getting the filename and path
    filename = system(paste('ls ',dataset_path,' | grep ^',name,'_',sep=''), intern=TRUE)
    path = paste(dataset_path,'/',filename, sep='')
    
    # if the file was not found
    # returns an empty matrix
    if (dir.exists(path))
    {
        return(matrix(0,ncol=0,nrow=0))
    }

    # loading the dataset
    x = read.csv(path, stringsAsFactors=FALSE, comment.char='#')

    # convert as data frame
    x = as.data.frame(x)

    if (nrow(x) == 0)
    {
        return(x)
    }

    # some attributes from the dataset
    station = x$station[1]
    lon = x$lon[1]
    lat = x$lat[1]
    metar = x$metar[1] 

    # TODO: check this when data has M as NA data
    #x2 = x[x$tmpf != 'M',]

    # formatting the time
    if ('valid' %in% names(x))
    {
        x$times = strptime(x$valid, format='%Y-%m-%d %H:%M')
    }
    else if ('times' %in% names(x))
    {
        x$times = strptime(x$times, format='%Y-%m-%d %H:%M')
    }
    
    # filtering the desired features
    x = x[,c('times', feats_l)]

    # converting features to numeric values
    x[,feats_l] = apply(x[,feats_l], 2, as.numeric)

    # filtergin by date (if informed)
    if(!is.null(begin))
    {
        begin = strptime(begin, format='%Y-%m-%d %H:%M')
        x = x[x$times >= begin,]
    }
    if(!is.null(end))
    {
        end = strptime(end, format='%Y-%m-%d %H:%M')
        x = x[x$times <= end,]
    }

    # some attributes from the dataset
    attr(x, 'station') = station
    attr(x, 'lon') = lon
    attr(x, 'lat') = lat
    attr(x, 'metar') = metar 


    # TODO: depois ver se alguma outra informacao deve ser adicionada
    
#    # getting the classes
#    y = as.numeric(x[,dim(x)[2]])
#
#    # removing the class from data
#    x = x[,-dim(x)[2]]
#
#    # naming the columns
#    colnames(x) = paste('f', 1:dim(x)[2], sep='')
#
#    # returning attributes
#
#    # classes
#    attr(x, 'y') = y
#
#    # get the name of the dataset
#    attr(x, 'name') = gsub('.*/(.+)_TRAIN.arff', '\\1', path, perl=TRUE)
#
#    # number of series
#    attr(x, 'n') = dim(x)[1]
#
#    # time series length
#    attr(x, 'm') = dim(x)[2]
#
#    # number of classes
#    attr(x, 'num_classes') = length(unique(y))

    return(x)
}

copyAttributes = function(to, from)
{
    att = names(attributes(from))
    att = att[att != 'dim']
    for(i in 1:length(att))
    {
        attr(to, att[i]) = attr(from, att[i])
    }
    return(to)
}

# z-transformation (version with and without biased sd)
znorm = function(x, correct=TRUE)
{
    if (correct == TRUE)
    {
        stdv = sd(x, na.rm=TRUE)
    }
    else
    {
        stdv = sqrt(sum((x - mean(x, na.rm=TRUE))^2)/length(x))
    }

    if (stdv == 0)
    {
        return( x - mean(x, na.rm=TRUE) )
    }

    return( (x - mean(x, na.rm=TRUE))/stdv )
}

# a debug printing function
printdebug = function(s)
{
    op <- options(digits.secs = 6)
    
    if (DEBUG==TRUE)
    {
        write(paste('DEBUG [',Sys.time(),']:',s), stderr())
    }
}


