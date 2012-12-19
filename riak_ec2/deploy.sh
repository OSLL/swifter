#!/bin/bash

DEFAULT_CONFIG_FILE="config"
conf_file=$DEFAULT_CONFIG_FILE

init_conf_file(){  
    if [ $# -gt 0 ] 
    then
      conf_file=$1
    fi
    echo Reading setting from:  $conf_file ...
  
    if [ ! -f $conf_file  ]; then
      echo "$conf_file : does not exists! "
      exit 1
    elif [ ! -r $conf_file  ]; then
      echo "$conf_file : can not read! "
      exit 1
    fi  
} 

init_conf_file $1

old_IFS=$IFS
IFS=$'\n'
hosts_param=($(cat "$conf_file"))
IFS=$old_IFS

# sequential execution script on ec2-hosts
# TODO parallel
for h in "${hosts_param[@]}" ; do
    p=( $h )    
    host_name=${p[0]}
    key=${p[1]}
    ec2_script=${p[2]}
    echo "Execution $ec2_script on $host_name..."
    
    ssh -i $key $host_name 'bash -s' < $ec2_script 
    
    if [ $? -eq 0 ]
    then
       echo "Execution $ec2_script on $host_name is succesfull!"
    else
       echo "Execution $ec2_script on $host_name is failure!"
    fi
done

exit 0
