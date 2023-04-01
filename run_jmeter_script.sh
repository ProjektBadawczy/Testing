#!/bin/bash

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
		eval docker run -v /home/ec2-user/:/mnt/jmeter/ justb4/jmeter:latest -n -t Pi2_parametrised.jmx -l log.jtl -Jusers=$user -Jiteration=$iteration -Jaddress=$address
		sleep $sleep
	done;
done;