#!/bin/bash

# call via SSH 
#  ssh hmf@192.168.40.69 "ulimit -c unlimited ; nohup ~/git/dl4jtest/run0.sh >> out.txt 2>&1"

source ~/.profile
cd ~/git/dl4jtest
#~/dev/scala/sbt/bin/sbt -mem 12336 "run-main pt.inescn.experiments.bosch.TableSawExpV2"
#~/dev/scala/sbt/bin/sbt -mem 15360 "run-main pt.inescn.experiments.bosch.TableSawExpV2"
# memory, enable asserts, avoid JVM error (see notes in code) 
~/dev/scala/sbt/bin/sbt -mem 15360 -J-ea -J-XX:+UseCountedLoopSafepoints "run-main pt.inescn.experiments.bosch.TableSawExpV2"


