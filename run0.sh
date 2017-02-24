#!/bin/bash

source ~/.profile
cd ~/git/dl4jtest
~/dev/scala/sbt/bin/sbt -mem 12336 "run-main pt.inescn.experiments.bosch.TableSawExpV2"
