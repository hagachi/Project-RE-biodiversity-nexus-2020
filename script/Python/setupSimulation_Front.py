# -*- coding: utf-8 -*-
"""
Created on Tue Dec 24 09:41:26 2019
@author: Chihiro Haga
"""

import os
import subprocess
import datetime
import numpy as np
import pandas as pd
import xlrd
import shutil
from PIL import Image # pillow library is required

def select_ecoregions(climdata):
  newdata = pd.DataFrame(climdata[1:, :], columns=climdata[0, :])
  eco_list = []
  for cname in newdata.columns:
    if cname in ['eco{}'.format(name) for name in [eco_unique_vals, 201, 202, 203]]:
      eco_list.append(True)
    elif cname == '':
      eco_list.append(True)
    else:
      eco_list.append(False)
  newdata = newdata.loc[:, eco_list]
  return newdata.columns.values, newdata.values

def make_climdata():
    fname = "../../default_files/climate_data.xlsx"
    BASE_INTERCEPT = 0.024
    AtmosNslope = 0.0058
    varnames = ['prcp', 'Tmax', 'Tmin', 'Ndep']
    for sim_time in ['historical', climname.split('_')[1]]:
      with open('./input_MRI-CGCM3_{}.csv'.format(sim_time), 'w') as f:
        for varname in varnames:
          if varname == 'Ndep': # N-deposition
              out_dat = prcp[:, 0].reshape(prcp.shape[0], 1)
              FERTILIZED = np.array([0, 0, 0, 0, 3.33, 3.33, 0, 3.33, 0, 0, 0, 0]).reshape(12, 1) + BASE_INTERCEPT
              NOT_FERTILIZED = np.ones(shape=(12, 1)) * BASE_INTERCEPT
              for eco_iter in range(len(eco_unique_vals)):
                  if sim_time == 'historical':
                      if eco_iter == 0: # forest ecoregion
                          AtmosNintercept = NOT_FERTILIZED
                      else:
                          AtmosNintercept = FERTILIZED
                  else:
                      if eco_iter == 0: # forest ecoregion
                          AtmosNintercept = np.tile(NOT_FERTILIZED, reps=(duration, 1))
                      else: # pastureland ecoregion
                          ndep_fertilize = np.tile(FERTILIZED, reps=(min(eco_unique_vals[eco_iter] % 1000, duration), 1))
                          ndep_nofertilize = np.tile(NOT_FERTILIZED, reps=(max(int(duration - eco_unique_vals[eco_iter] % 1000), 0), 1))
                          AtmosNintercept = np.concatenate([ndep_fertilize, ndep_nofertilize], axis=0)
                  # Compute n-deposition for each month
                  ndep = prcp[:, 1].reshape(out_dat.shape[0], 1) * 0.1 * AtmosNslope + AtmosNintercept # convert prcp from mm to cm
                  out_dat = np.concatenate([out_dat, ndep], axis=1)
          else: # Other climate variables
              # Read climate data from xlsx file
              dat = pd.read_excel(fname, sheet_name='{}_{}'.format(sim_time, varname))
              # dat = pd.read_excel(fname, sheetname='{}_{}'.format(sim_time, varname))
              dat = np.asarray(dat)
              duration = int(dat.shape[0]/12)
              # store precipitation for Ndep calculation
              if varname == 'prcp':
                  prcp = dat
              # Mutate columns for each ecoregion
              out_dat = dat[:, 0].reshape(dat.shape[0], 1)
              for _ in range(len(eco_unique_vals)):
                  out_dat = np.concatenate([out_dat, dat[:, 1].reshape(dat.shape[0], 1)], axis=1)
          # insert var and stdev
          for _ in range(len(eco_unique_vals) * 2):
              out_dat = np.concatenate([out_dat, np.zeros(shape=(out_dat.shape[0], 1))], axis=1)
          # Write data
          if varname == 'prcp':
            f.write('#prcp\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(mm/month)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(mm/month^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(mm/month)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))
          elif varname == 'Tmax':
            f.write('#Tmax\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(degC)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(degC^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(degC)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))
          elif varname == 'Tmin':
            f.write('#Tmin\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(degC)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(degC^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(degC)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))
          elif varname == 'Ndep':
            f.write('#Ndep\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(gN/month)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(gN/month^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(gN/month)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))

          for _ in range(out_dat.shape[0]):
            buf = [str(val) for val in out_dat[_, :]]
            f.write('{}\n'.format(','.join(buf)))

if __name__ == "__main__":
  # set variables
  paareas = [223, 178, 134, 89, 45, 0]
  mngnames = ['BaU']
  climnames = ["MRI_rcp26", "MRI_rcp85"]
  BETULA_AGE = 25
  # set root directory
  os.chdir(os.path.dirname(os.path.abspath(__file__)))
  rootdir = os.path.abspath("../../")
  print('Root directory: %s'%rootdir)
  for paarea in paareas:
    for biomrate in [0, 2, 4, 6, 8, 10]:
      if paarea == 0:
        if biomrate > 0: # those cases show the same result as 0-0 case
          break
        biomrate = round(biomrate * 0.1, 1)
        solrate = 0
      else:
        biomrate = round(biomrate * 0.1, 1)
        solrate = round(1 - biomrate, 1)

      mngname = 'a{}_s{:.1f}'.format(paarea, solrate)
      print(mngname)
      mngdir = os.path.join(rootdir, mngname)
      if not os.path.isdir(mngdir):
          os.mkdir(mngdir)

      # Check the management and ecoregion raster to get management and ecoregion codes
      mngras = Image.open(os.path.join(rootdir, 'input', '200304_mngMap_BaU_area{}_sol{:.1f}.tif'.format(paarea, solrate)))
      ecoras = Image.open(os.path.join(rootdir, 'input', 'ecoregions_bekambe_BaU_{}_v2.1.tif'.format(paarea)))
      # Get unique code for each pastureland utilization IDs
      mng_unique_vals = np.unique(np.asarray(mngras))
      eco_unique_vals = np.unique(np.asarray(ecoras))
      eco_unique_vals = eco_unique_vals[eco_unique_vals > 0]
      eco4pasture = np.sort(eco_unique_vals[eco_unique_vals >= 1000])
      mng4biomass = np.sort(mng_unique_vals[(mng_unique_vals >= 1000) & (mng_unique_vals < 2000)])
      mng4riparian = np.sort(mng_unique_vals[(mng_unique_vals >= 2000) & (mng_unique_vals < 3000)])
      mng4solar = np.sort(mng_unique_vals[(mng_unique_vals >= 3000) & (mng_unique_vals < 4000)])
      mng4pasture = np.sort(mng_unique_vals[mng_unique_vals >= 4000])

      for climname in climnames:
          climdir = os.path.join(mngdir, climname)
          try:
            if not os.path.isdir(climdir):
              os.mkdir(climdir)
            os.chdir(climdir)
          except:
            print("failed to make climate directory")

          # Replace 0: ecoregion.txt
          try:
            shutil.copy('../../default_files/ecoregions_bekambe.txt', './ecoregions_bekambe.txt')
            with open("ecoregions_bekambe.txt", "a", encoding = "utf-8") as f:
              f.write('\n>> Setting for pasture land\n')
              for mapcode in eco4pasture:
                f.write('\tyes\t{}\teco{}\t"pastureland"\n'.format(mapcode, mapcode))

          except:
            print('failed to make ecoregions_bekambe.txt')

          # Create climate data for the case
          try:
            make_climdata()
          except:
            print('failed to make climate data')

          # Replace1: climate configfile in NECN-succession.txt
          try:
            oldClimateConfigName = "ClimateConfigFile\n"
            fertilizeMode = "A"
            newClimateConfigName = "ClimateConfigFile           ../../ini/climate-generator_%s_%s.txt\n" % (climname, fertilizeMode)

            oldNECNIniFile = open("../../default_files/NECN-succession_bekambe.txt", encoding = "utf-8")
            newNECNIniFile = open("NECN-succession_bekambe.txt", "w", encoding = "utf-8")

            for s in oldNECNIniFile:
              newNECNIniFile.write(s.replace(oldClimateConfigName, newClimateConfigName))

            oldNECNIniFile.close()
            newNECNIniFile.close()
          except:
            print("Climate config file copy error")


          # Replace2: ecoregion map name in scenario.txt
          try:
            oldEcoregionsMapName = "EcoregionsMap replace here\n"
            newEcoregionsMapName = "EcoregionsMap               ../../input/ecoregions_bekambe_BaU_%s_v2.1.tif\n" % (paarea)

            oldScenarioFile = open("../../default_files/scenario.txt", encoding = "utf-8")
            newScenarioFile = open("scenario.txt", "w", encoding = "utf-8")

            for s in oldScenarioFile:
              if s == oldEcoregionsMapName:
                newScenarioFile.write(s.replace(oldEcoregionsMapName, newEcoregionsMapName))
              else:
                newScenarioFile.write(s)

            oldScenarioFile.close()
            newScenarioFile.close()
          except:
            print("scenario.txt file copy error")


          # Replace 3: Harvest
          try:
              with open('../../default_files/BiomassHarvest.txt', 'r', encoding = "utf-8") as f:
                  lines = f.readlines()

              with open("./BiomassHarvest.txt", "w", encoding = "utf-8") as f:
                # ひな型をコピーする
                for line in lines:
                  if line == 'ManagementAreas	replacehere\n':
                    f.write('ManagementAreas	../../input/200304_mngMap_BaU_area{}_sol{:.1f}.tif\n'.format(paarea, solrate))
                  else:
                    f.write(line)
                # Write the harvest implementation table for pastureland management -----------------------
                if len(mng4riparian) > 0:
                  f.write('\n>> Setting for natural riparian zone\n')
                  for mng4riparian_iter in range(len(mng4riparian)):
                    change_year = min(85, mng4riparian_iter + 1)
                    if paarea == 0:
                      change_year = 85
                    mng_id = mng4riparian[mng4riparian_iter]
                    # pastureland for riparian forest
                    f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, change_year))

                if len(mng4solar) > 0:
                  f.write('\n>> Setting for solar power energy\n')
                  for mng4solar_iter in range(len(mng4solar)):
                    change_year = min(85, mng4solar_iter + 1)
                    if paarea == 0:
                      change_year = 85
                    mng_id = mng4solar[mng4solar_iter]
                    # pastureland for solar pv systems
                    f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, change_year))
                    if paarea > 0:
                      if change_year < 85:
                        f.write('{} stop_and_solar 100% {} 85\n'.format(mng_id, change_year + 1))

                if len(mng4biomass) > 0:
                  f.write('\n>> Setting for biomass energy\n')
                  for mng4biomass_iter in range(len(mng4biomass)):
                    change_year = min(85, mng4biomass_iter + 1)
                    if paarea == 0:
                      change_year = 85
                    mng_id = mng4biomass[mng4biomass_iter]
                    # pastureland for biomass utilization
                    f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, change_year))
                    if paarea > 0 and change_year + 1 + BETULA_AGE < 86:
                      if change_year < 85:
                        f.write('{} betula_ClearCutting_abandoned 100% {} 85\n'.format(mng_id, change_year + 1 + BETULA_AGE))

                if len(mng4pasture) > 0:
                  f.write('\n>> Setting for managing pastureland\n')
                  mng_id = mng4pasture[0]
                  f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, 85))

                f.write('\n>> Outputs\n')
                f.write('PrescriptionMaps    OutputMaps/harvest/prescripts-{timestep}.img\n')
                f.write('BiomassMaps         OutputMaps/harvest/biomass-removed-{timestep}.img\n')
                f.write('EventLog            biomass-harvest-event-test-log.csv\n')
                f.write('SummaryLog	    	biomass-harvest-summary-log.csv\n')

          except:
              print('BiomassHarvest.txt file error')

      del eco4pasture
      del mng4biomass
      del mng4riparian
      del mng4solar
      del mng4pasture
