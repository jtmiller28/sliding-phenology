#!/bin/bash

#SBATCH --job-name=Phenesse-distributing      # Job name
#SBATCH --mail-type=ALL                  # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=jtmiller@ucsb.edu    # Where to send mail
#SBATCH --output=%j.log                  # Standard output and error log
#SBATCH --nodes=1                        # Run all processes on a single node
#SBATCH --ntasks=1                       # Run a single task
#SBATCH --cpus-per-task=10               # Number of CPU cores per task
#SBATCH --mem=50gb                       # Job memory request
#SBATCH --time=00-60:00:00               # Time limit days-hrs:min:sec
#SBATCH --qos=soltis-b
pwd; hostname; date

#load modules

module load R/4.0

#do some (or alot) of coding
Rscript --vanilla /blue/soltis/millerjared/temp-sliding-phen/sliding-window-phenesse.R $1 $2 

##example for running: sbatch /blue/soltis/millerjared/temp-sliding-phen/sliding-window-phenesse.sh /blue/soltis/millerjared/temp-sliding-phen/outputs/phenesse-species-plots-par/ /blue/soltis/millerjared/temp-sliding-phen/outputs/phenesse-species-tables-par/