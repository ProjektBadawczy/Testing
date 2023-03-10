FROM justb4/jmeter:latest

COPY First_tests.jmx /jmeter

RUN -n -t /jmeter/First_tests.jmx -Jurl=abcdef.com -l /jmeter/results.csv