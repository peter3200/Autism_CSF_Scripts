import glob
import os
import time
import csv

ResultsDir="/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/output_1yr/time5_t1"
ResultsSuffix="FinalStats.txt"
OutDir="/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/final_results"
timestamp=str(int(time.time()))
outfile=OutDir+"/EaCSF_results_1.7.7_T1only_time5_t1_"+timestamp+".csv"

def strip_end(text, suffix):
    if not text.endswith(suffix):
        return text
    return text[:len(text)-len(suffix)]

def strip_start(text, prefix):
    if not text.startswith(prefix):
        return text
    return text[len(prefix):]


VoxelsByScanid=dict()

for f in glob.glob(ResultsDir+"/*/*"+ResultsSuffix):


    base = os.path.basename(f)
    scanid=strip_end(base,ResultsSuffix)
    scanid=strip_start(scanid,"ACPC")
    print(f,scanid)

    with open(f, 'r') as content_file:
        content = content_file.read()
    Thirdline=content.splitlines()[2]
    SeventhColumnOfThirdLine=Thirdline.split('\t')[6]
    VoxelsByScanid[scanid]=SeventhColumnOfThirdLine

with open(outfile,'w') as g:
    w = csv.writer(g)
    w.writerows(VoxelsByScanid.items())
