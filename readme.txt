Scala
......
http://docs.scala-lang.org/cheatsheets/
. Download from scala site
. Extract to ~/dev/scala
. Leave new directory as is
. Add PATH="$HOME/dev/scala/scala-2.12.1/bin" to .profile

SBT
....
https://gist.github.com/djspiewak/cb72c41ac335a3a9b28b3307be04aa43
. Download from sbt site
. Extract to ~/dev/scala
. Rename directory to sbt
. Add PATH="$HOME/dev/scala/scala-2.12.1/bin"
    PATH="$HOME/dev/scala/scala-2.12.1/bin:$HOME/dev/scala/sbt/bin:$PATH"
  to .profile
. Add to ~/.sbt/0.13/global.sbt (touch global.sbt)
    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
. Add to ~/.sbt/0.13/plugins (mkdir plugins; touch plugins/plugins.sbt) 
     addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.0.1")
. sbt update


Note on Artima Supersafe
-------------------------
- When we add the plug-in in projects/build.sbt:
     addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.2")
- We must *not* add the above plug-in anywhere else
- Add to ~/.sbt/0.13/global.sbt
      resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
- Do not add this resolver anywhere else
- Make sure you have an empty line before and after each of the sbt lines
     
     
http://stackoverflow.com/questions/17552457/how-do-i-upload-eclipse-projects-to-github
git init
git remote add origin https://github.com/[username]/[reponame].git
git commit -a -m "Initial commit"
git push -u origin --all

sbt eclipse
reload
eclipse with-source=true


ND4J Examples
..............
sbt "run-main org.nd4j.examples.MatrixOperationExample"
sbt "run-main org.nd4j.examples.Nd4jEx1_INDArrayBasics"
sbt "run-main org.nd4j.examples.Nd4jEx2_CreatingINDArrays"
sbt "run-main org.nd4j.examples.Nd4jEx3_GettingAndSettingSubsets"
sbt "run-main org.nd4j.examples.Nd4jEx4_Ops"
sbt "run-main org.nd4j.examples.Nd4jEx5_Accumulations"
sbt "run-main org.nd4j.examples.Nd4jEx6_BooleanIndexing"


Checking if OpenBLAS is working
...................................

Instalation
............
sudo update-alternatives --list libblas.so
/usr/lib/atlas-base/atlas/libblas.so
/usr/lib/libblas/libblas.so
/usr/lib/openblas-base/libblas.so
root@gandalfix:/home/hmf# sudo update-alternatives --config libblas.so
There are 3 choices for the alternative libblas.so (providing /usr/lib/libblas.so).

  Selection    Path                                  Priority   Status
------------------------------------------------------------
* 0            /usr/lib/openblas-base/libblas.so      40        auto mode
  1            /usr/lib/atlas-base/atlas/libblas.so   35        manual mode
  2            /usr/lib/libblas/libblas.so            10        manual mode
  3            /usr/lib/openblas-base/libblas.so      40        manual mode

Press <enter> to keep the current choice[*], or type selection number: 0

sudo update-alternatives --config libblas.so.3
There are 3 choices for the alternative libblas.so.3 (providing /usr/lib/libblas.so.3).

  Selection    Path                                    Priority   Status
------------------------------------------------------------
* 0            /usr/lib/openblas-base/libblas.so.3      40        auto mode
  1            /usr/lib/atlas-base/atlas/libblas.so.3   35        manual mode
  2            /usr/lib/libblas/libblas.so.3            10        manual mode
  3            /usr/lib/openblas-base/libblas.so.3      40        manual mode

Press <enter> to keep the current choice[*], or type selection number: 0

sudo update-alternatives --config liblapack.so
sudo update-alternatives --config liblapack.so.3

Checking with R
...............
Start R
root@gandalfix:/home/hmf# ps aux | grep exec/R

Press <enter> to keep the current choice[*], or type selection number: 0
root@gandalfix:/home/hmf# ps aux | grep exec/R
hmf      19961  9.2  0.2 418792 36924 pts/18   Sl+  17:55   0:00 /usr/lib/R/bin/exec/R
root     19973  0.0  0.0  14232  1016 pts/19   S+   17:55   0:00 grep --color=auto exec/R
root@gandalfix:/home/hmf#  lsof -p 19961 | grep 'blas\|lapack'
lsof: WARNING: can't stat() fuse.gvfsd-fuse file system /run/user/1000/gvfs
      Output information may be incomplete.
R       19961  hmf  mem    REG    8,2  6172176 18612834 /usr/lib/openblas-base/liblapack.so.3
R       19961  hmf  mem    REG    8,2 31962416 18488605 /usr/lib/libopenblasp-r0.2.18.so
R       19961  hmf  mem    REG    8,2   395528 18612831 /usr/lib/openblas-base/libblas.so.3

Checking with ND4J Examples
.............................

sbt "run-main org.nd4j.examples.MatrixOperationExample"

(See the line referring to BLAS)

[info] Loading global plugins from /home/hmf/.sbt/0.13/plugins
[info] Loading project definition from /home/hmf/tmp/dl4jtest/project
[info] Set current project to dl4jtest (in build file:/home/hmf/tmp/dl4jtest/)
[info] Running org.nd4j.examples.MatrixOperationExample 
[run-main-0] INFO org.nd4j.nativeblas.NativeOps - Number of threads used for NativeOps: 4
[run-main-0] INFO org.nd4j.nativeblas.Nd4jBlas - Number of threads used for BLAS: 4
[run-main-0] INFO org.reflections.Reflections - Reflections took 90 ms to scan 11 urls, producing 121 keys and 415 values 
[run-main-0] INFO org.reflections.Reflections - Reflections took 35 ms to scan 11 urls, producing 121 keys and 415 values 
[1.00, 2.00]
[3.00, 4.00]
[[1.00, 3.00],
 [2.00, 4.00]]
[[3.00, 4.00],
 [5.00, 6.00]]
Creating nd array with data type FLOAT
11.00
[13.00, 16.00]
[[18.00, 22.00],
 [26.00, 32.00]]
[[11.00, 25.00],
 [17.00, 39.00]]
[[3.00, 6.00],
 [4.00, 8.00]]
[5.00, 5.00]
[success] Total time: 1 s, completed 15/nov/2016 10:43:35


DL4J
-----

Anomaly Detection
.................

1. Autoencoder
   . https://github.com/deeplearning4j/dl4j-examples/blob/master/dl4j-examples/src/main/java/org/deeplearning4j/examples/feedforward/anomalydetection/MNISTAnomalyExample.java
   . sbt "run-main org.dl4j.examples.MNISTAnomalyExample"


2. https://github.com/deeplearning4j/dl4j-examples/blob/master/dl4j-examples/src/main/java/org/deeplearning4j/examples/unsupervised/deepbelief/DeepAutoEncoderExample.java
   . sbt "run-main org.dl4j.examples.DeepAutoEncoderExample"
   
   
Running Experimets in server
----------------------------

http://askubuntu.com/questions/349262/run-a-nohup-command-over-ssh-then-disconnect
ssh hmf@192.168.40.69 "nohup ~/git/dl4jtest/run0.sh >> out.txt 2>&1"  
ssh hmf@192.168.40.69 "ulimit -c unlimited ; nohup ~/git/dl4jtest/run0.sh >> out.txt 2>&1"
   
TODO
-----

StreamBuilder
.............
. Take multi discrete distribution that return E. Make E array of distributions
  Returned E is sampled
. Use above to implementnet and test binomial distributions
. Make example above for mixed distributions
. Start with labelled distributions using above functions
  Use combine 
. Add TODO to use HLists via Shapeless for combine
. TODO Add support for filtering in streams - supports segmentation
. TODO Add exponential, polynomial and other types of space functions
  Use linspace as base. Pass the function that does the jumps. Show examples

Others
. TODO Add comprehension to generate parameters using streams or lazy stuff or tailed can stuff
. Add support to filter generation - limit testing set
. Add random search and others auto- ml
. TODO: add filtering based on multiple signals/streams


Pattern Examples
. http://gigiigig.github.io/tlp-step-by-step/introduction.html
. https://apocalisp.wordpress.com/2010/06/08/type-level-programming-in-scala/
. http://www.michaelpollmeier.com/presentations/2013-11-28-typelevel-programming-scala/#/
. http://slick.lightbend.com/talks/scalaio2014/Type-Level_Computations.pdf
. http://stackoverflow.com/questions/4415511/scala-type-programming-resources
. http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/T4_Barnes_Typelevel_Prog.pdf
  https://www.youtube.com/watch?v=_-J4YRI1rAw
. http://rudairandamacha.blogspot.pt/2012/02/type-level-programming.html
  https://www.youtube.com/watch?v=WZOzxAP8NpI
    

https://amplab.cs.berkeley.edu/wp-content/uploads/2015/07/163-sparks.pdf
https://xyclade.github.io/MachineLearning/
https://github.com/haifengl/smile
https://github.com/yahoo/egads

https://blog.scalac.io/2015/05/07/encog-dsl.html
http://haifengl.github.io/smile
http://www.heatonresearch.com/encog/


Anomaly Detection Benckmark
----------------------------

0. Numenta Anomaly Detection Benchmark
   https://github.com/numenta/NAB
   
0. Create a virtual environment for python 2.7
   cd /home/hmf
   mkdir my_py2
   sudo pip install virtualenv
   sudo pip install --upgrade virtualenv
   pip install --upgrade pip
   virtualenv --system-site-packages --always-copy --python=/usr/bin/python2 my_py2
   source my_py2/bin/activate
   
0. Clone the repository
   https://help.github.com/articles/importing-a-git-repository-using-the-command-line/
   git clone --bare https://external-host.com/extuser/repo.git
   # Makes a bare clone of the external repository in a local directory
   http://stackoverflow.com/questions/1872113/how-do-i-clone-a-github-project-to-run-locally
   git clone git://github.com/ryanb/railscasts-episodes.git
   git clone git://github.com/ryanb/railscasts-episodes.git -b branch_name
   git clone https://github.com/ryanb/railscasts-episodes.git Rails
   ```
    cd /home/hmf/my_py2/
    mkdir download
    cd download/
    cd /home/hmf/my_py2/download
    git clone https://github.com/numenta/NAB.git
    ```

0. Update the cloned repository
   http://stackoverflow.com/questions/7244321/how-do-i-update-a-github-forked-repository
   
   ```
	# Add the remote, call it "upstream":
	
	cd /home/hmf/my_py2/download/NAB
	git remote add upstream https://github.com/numenta/NAB.git
	
	# Fetch all the branches of that remote into remote-tracking branches,
	# such as upstream/master:
	
	git fetch upstream
	
	# Make sure that you're on your master branch:
	
	git checkout master
	
	# Rewrite your master branch so that any commits of yours that
	# aren't already in upstream/master are replayed on top of that
	# other branch:
	
	git rebase upstream/master   
    ```
   
0. Installing NAB
   cd /home/hmf/my_py2
   source ./bin/activate
   
   # Force local numpy install
   pip install -U --force numpy
   
   cd /home/hmf/my_py2/download/NAB
   pip install -r requirements.txt
   python setup.py install --user
   python run.py --help
     
0. Running existing bench marks
   cd /home/hmf/my_py2
   cd /home/hmf/my_py2/download/NAB
   
   # Run single algorithm HTM - but use obnly a subset of the benchmark data
   # Need to install NuPIC first - https://github.com/numenta/nupic
   python run.py -d numenta --detect --windowsFile labels/combined_windows_tiny.json
 
   # Run all of NAB
   python run.py
   
   # Run HTM with NAB with all of the data   
   python run.py -d numenta --detect --optimize --score --normalize
   
   # This should work
   # Run Twitter's algorithm - "--optimize" is ptional
   python run.py -d twitterADVec --optimize (optional) --score --normalize
   
   Others (see NAB/results):
   ```
	bayesChangePt  null    random     skyline
	contextOSE     htmjava numenta    twitterADVec
	expose         knncad  numentaTM  relativeEntropy  windowedGaussian   
   ```
   
0. See results
	```
	Running scoring step
	twitterADVec detector benchmark scores written to /home/hmf/my_py2/download/NAB/results/twitterADVec/twitterADVec_reward_low_FP_rate_scores.csv
	twitterADVec detector benchmark scores written to /home/hmf/my_py2/download/NAB/results/twitterADVec/twitterADVec_reward_low_FN_rate_scores.csv
	twitterADVec detector benchmark scores written to /home/hmf/my_py2/download/NAB/results/twitterADVec/twitterADVec_standard_scores.csv
	
	Running score normalization step
	Final score for 'twitterADVec' detector on 'reward_low_FP_rate' profile = 33.61
	Final score for 'twitterADVec' detector on 'reward_low_FN_rate' profile = 53.50
	Final score for 'twitterADVec' detector on 'standard' profile = 47.06
	Final scores have been written to /home/hmf/my_py2/download/NAB/results/final_results.json.
   ```
   