#!/bin/bash
#SBATCH --job-name=DeepDive       # create a short name for your job
#SBATCH --nodes=1                 # node count
#SBATCH --ntasks=1                # total number of tasks across all nodes
#SBATCH --cpus-per-task=1         # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=4G          # memory per cpu-core (4G is default)
#SBATCH --time=01:00:00           # total run time limit (HH:MM:SS)
#SBATCH --mail-type=begin         # send email when job begins
#SBATCH --mail-type=end           # send email when job ends
#SBATCH --mail-user=sw8569@princeton.edu
#SBATCH --output=/scratch/gpfs/sw8569/deepdive_%j.out
#SBATCH --error=/scratch/gpfs/sw8569/deepdive_%j.err

# Change to the directory where the script should run
cd /home/sw8569/DeepDiveR

# Load any necessary modules (you may need to adjust this based on your cluster setup)
module purge
module load anaconda3/2024.2

# Activate your Python environment if you're using one (uncomment if needed)
# conda activate your_environment_name

# Run the DeepDive command
python ../deepdive/run_dd_config.py config_file.ini -wd reptilia/
