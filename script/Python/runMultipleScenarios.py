# -*- coding: utf-8 -*-
"""
Created on Tue Dec 24 09:41:26 2019
@author: Chihiro Haga
"""

import os
import subprocess
from joblib import Parallel, delayed

# Define functions =-=====================================
# Run LANDIS-II model function ----
def run_landisii(scenario_path):
    os.chdir(scenario_path)
    print('Model run started: {}'.format(scenario_path))
    command = 'landis-ii-7 scenario.txt | echo off'
    try:
        subprocess.call(command, shell = True)
        with open('Landis-log.txt', 'r') as f:
            lines = f.readlines()
        if "Model run is complete" in lines[-1]:
            print('Model run finished')
        else:
            print('Error! See Landis-log.txt in {}'.format(scenario_path))
    except:
        print('Error in processing {} ...'.format(scenario_path))

# Parallel control ------
def usejoblib(job, path_list):
    Parallel(n_jobs=job)([delayed(run_landisii)(path) for path in path_list])

# Main ===================================================
if __name__ == "__main__":
    CORE_NUMBER = 6 # NUMBER OF CORES FOR MULTIPROCESSING
    root_path = r'F:\scenario_v2.1'
    os.chdir(root_path)
    mng_list = [path for path in os.listdir() if os.path.isdir(path) and path.startswith('a')]
    scenario_path = []
    for rcp_name in ['MRI_rcp26', 'MRI_rcp85']:
        for mng_name in mng_list:
            scenario_path.append(os.path.join(root_path, mng_name, rcp_name))
    usejoblib(CORE_NUMBER, scenario_path)
