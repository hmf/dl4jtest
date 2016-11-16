 #!/bin/bash

# IMPORTANT !
# You must source this file for use in the parent shell
#

# Only if we manually install or use NVIDIA's deb file 
#export PATH=/usr/local/cuda-7.5/bin:$PATH
#export LD_LIBRARY_PATH=/usr/local/cuda-7.5/lib64:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/lib/nvidia-367"
export CUDA_HOME=/usr/local/cuda
export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$CUDA_HOME/lib"
export PATH="$CUDA_HOME/bin:$PATH"

sudo tee /proc/acpi/bbswitch <<<ON
dmesg |tail -1

sudo nvidia-modprobe
lsmod | grep -i nvidia
cat /proc/driver/nvidia/version

current="$PWD"
#cd ~/Downloads/nvidia_cuda/cudnn/mnistCUDNN/
cd ~/Downloads/nvidia_cuda/cudnn/cudnn_samples_v5/mnistCUDNN
./mnistCUDNN
cd "$current"

ls -l /dev/nvidia*

