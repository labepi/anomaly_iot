
# script for automatizing the classification call

if [ $# -eq 0 ]
then
    D=3
else
    D=$1
fi


#two_class='TRUE'
two_class='FALSE'

# botnet path
dataset_path='./data/botnet/1000'

# fixing the seed and check the best field
#seed=1
#for field in $(cat data/botnet/demonstrate_structure.csv | sed 's/,/ /g')

# fixing the field and looping on the seeds
field="HH_L5_radius"
#field="HH_jit_L5_variance"
start_seq=1
total=30
for seed in $(seq $start_seq $((start_seq + total - 1)))
do
    #echo $field
    d_name=$field"_feats"
    Rscript classify.R $d_name $D $seed $dataset_path FALSE $two_class
done

# TODO: check this
# the best features for D3 were these (ascending order):
# HH_jit_L1_weight_feats 1 FINAL_ACC 0.75
# HH_L1_weight_feats 1 FINAL_ACC 0.75
# HH_L5_std_feats 1 FINAL_ACC 0.75
# H_L1_weight_feats 1 FINAL_ACC 0.75
# H_L3_weight_feats 1 FINAL_ACC 0.75
# MI_dir_L1_weight_feats 1 FINAL_ACC 0.75
# MI_dir_L3_weight_feats 1 FINAL_ACC 0.75
# HH_jit_L5_variance_feats 1 FINAL_ACC 0.8125
# HH_L5_radius_feats 1 FINAL_ACC 0.8125

