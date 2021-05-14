#!/usr/bin/awk -f

# USAGE: ./mean.awk -v col=10
#   where col=10 is the 10th column

BEGIN {
    #num = ARGV[1];
    #print "column:",col;
}
{
    delta=$col-avg; 
    avg+=delta/NR; 
    mean2+=delta*($col-avg);
} 
END {
    print avg" "sqrt(mean2/(NR-1))" "NR; 
}
