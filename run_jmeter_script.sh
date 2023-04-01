#!/bin/bash

JMETER_PATH="apache-jmeter-5.5/bin/jmeter.sh"

while getopts u:i:a:s: flag
do
    case "${flag}" in
        u) users=${OPTARG};;
        i) iterations=${OPTARG};;
        a) address=${OPTARG};;
        s) sleep=${OPTARG};;
    esac
done

for iteration in `seq 1 $iterations`; do
	mkdir $iteration
	
	for user in `seq 1 $users`; do
		eval $JMETER_PATH -n -t /mnt/jmeter/Pi2_parametrised.jmx -l log.jtl -Juser=$user -Jiteration=$iteration -Jaddress=$address -o /home/ec2-user/
		sleep $sleep
	done;
done;
