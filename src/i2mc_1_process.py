import os
import glob
import I2MC
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# This script is adapted from DevStart https://tommasoghilardi.github.io/DevStart/
# It reads the raw data from tobii, and runs each recording session through the i2mc function, and outputs a big data file at the end
# Optional: It also generates i2mc fixation plots as separate images.

# =============================================================================
# Import data from Tobii TX300
# =============================================================================

def tobii_TX300(rec_df, res=[1920,1080]):
    df = pd.DataFrame()
    

    # Extract required data
    df['time'] = rec_df['time']
    df['L_X'] = rec_df['L_X']
    df['L_Y'] = rec_df['L_Y']
    df['R_X'] = rec_df['R_X']
    df['R_Y'] = rec_df['R_Y']
    
    
    ###
    # Sometimes we have weird peaks where one sample is (very) far outside the
    # monitor. Here, count as missing any data that is more than one monitor
    # distance outside the monitor.
    
    # Left eye
    lMiss1 = (df['L_X'] < -res[0]) | (df['L_X']>2*res[0])
    lMiss2 = (df['L_Y'] < -res[1]) | (df['L_Y']>2*res[1])
    lMiss  = lMiss1 | lMiss2 | (rec_df['L_V'] > 1)
    df.loc[lMiss,'L_X'] = np.NAN
    df.loc[lMiss,'L_Y'] = np.NAN
    
    # Right eye
    rMiss1 = (df['R_X'] < -res[0]) | (df['R_X']>2*res[0])
    rMiss2 = (df['R_Y'] < -res[1]) | (df['R_Y']>2*res[1])
    rMiss  = rMiss1 | rMiss2 | (rec_df['R_V'] > 1)
    df.loc[rMiss,'R_X'] = np.NAN
    df.loc[rMiss,'R_Y'] = np.NAN

    return(df)


# Find the files
current_path = os.path.dirname(os.path.realpath('d'))
data_files = 'data/raw/PET_network_study1_Ngamba_raw_new_AOIs.tsv'
# define the output folder
output_folder = os.path.join(current_path, 'data/temp/i2mc_output') # define folder path\name

# Create the outputfolder
os.makedirs(output_folder, exist_ok=True)

# =============================================================================
# NECESSARY VARIABLES

opt = {}
# General variables for eye-tracking data
opt['xres']         = 1920.0                # maximum value of horizontal resolution in pixels
opt['yres']         = 1080.0                # maximum value of vertical resolution in pixels
opt['missingx']     = np.NAN                # missing value for horizontal position in eye-tracking data (example data uses -xres). used throughout the algorithm as signal for data loss
opt['missingy']     = np.NAN                # missing value for vertical position in eye-tracking data (example data uses -yres). used throughout algorithm as signal for data loss
opt['freq']         = 300.0                 # sampling frequency of data (check that this value matches with values actually obtained from measurement!)

# Variables for the calculation of visual angle
# These values are used to calculate noise measures (RMS and BCEA) of
# fixations. The may be left as is, but don't use the noise measures then.
# If either or both are empty, the noise measures are provided in pixels
# instead of degrees.
opt['scrSz']        = [50.9174, 28.6411]    # screen size in cm
opt['disttoscreen'] = 60.0                  # distance to screen in cm.

# Options of example script
do_plot_data = True # if set to True, plot of fixation detection for each trial will be saved as png-file in output folder.
# the figures works best for short trials (up to around 20 seconds)

# =============================================================================
# OPTIONAL VARIABLES
# The settings below may be used to adopt the default settings of the
# algorithm. Do this only if you know what you're doing.

# # STEFFEN INTERPOLATION
opt['windowtimeInterp']     = 0.1                           # max duration (s) of missing values for interpolation to occur
opt['edgeSampInterp']       = 2                             # amount of data (number of samples) at edges needed for interpolation
opt['maxdisp']              = opt['xres']*0.2*np.sqrt(2)    # maximum displacement during missing for interpolation to be possible

# # K-MEANS CLUSTERING
opt['windowtime']           = 1.0                           # time window (s) over which to calculate 2-means clustering (choose value so that max. 1 saccade can occur) #default: 0.2
opt['steptime']             = 0.02                          # time window shift (s) for each iteration. Use zero for sample by sample processing
opt['maxerrors']            = 100                           # maximum number of errors allowed in k-means clustering procedure before proceeding to next file
opt['downsamples']          = [2, 5, 10]
opt['downsampFilter']       = False                         # use chebychev filter when downsampling? Its what matlab's downsampling functions do, but could cause trouble (ringing) with the hard edges in eye-movement data

# # FIXATION DETERMINATION
opt['cutoffstd']            = 2.0                           # number of standard deviations above mean k-means weights will be used as fixation cutoff
opt['onoffsetThresh']       = 3.0                           # number of MAD away from median fixation duration. Will be used to walk forward at fixation starts and backward at fixation ends to refine their placement and stop algorithm from eating into saccades
opt['maxMergeDist']         = 30.0                          # maximum Euclidean distance in pixels between fixations for merging
opt['maxMergeTime']         = 30.0                          # maximum time in ms between fixations for merging
opt['minFixDur']            = 40.0                          # minimum fixation duration after merging, fixations with shorter duration are removed from output

#%% Run I2MC

raw_df = pd.read_csv(data_files, delimiter='\t', 
                     usecols = ['Recording timestamp', 'Participant name', 'Recording name', 
                                'Gaze point left X', 'Gaze point left Y', 'Pupil diameter left', 'Validity left',
                                'Gaze point right X', 'Gaze point right Y', 'Pupil diameter right', 'Validity right']).rename(columns = {'Recording timestamp': 'time', 
                                               'Participant name': 'participant_name', 
                                               'Recording name': 'recording_name',
                                               'Gaze point left X':'L_X',
                                                'Gaze point left Y': 'L_Y',
                                                'Pupil diameter left': 'L_P',
                                                'Validity left': 'L_V', 
                                                'Gaze point right X': 'R_X', 
                                                'Gaze point right Y': 'R_Y', 
                                                'Pupil diameter right': 'R_P', 
                                                'Validity right': 'R_V'})
raw_df.loc[raw_df['R_V'] == 'Valid', 'R_V'] = True
raw_df.loc[raw_df['L_V'] == 'Valid', 'L_V'] = True
raw_df.loc[raw_df['R_V'] == 'Invalid', 'R_V'] = False
raw_df.loc[raw_df['L_V'] == 'Invalid', 'L_V'] = False


big_df = pd.DataFrame()
for i, ((participant_name, recording_no), group_df) in enumerate(raw_df.groupby(['participant_name', 'recording_name'])):
    print(f'Processing {participant_name}, {recording_no}')
    data = tobii_TX300(group_df, [opt['xres'], opt['yres']])
    fix,_,_ = I2MC.I2MC(data,opt)

    # uncomment to plot output as images
    # f = I2MC.plot.data_and_fixations(data, fix, fix_as_line=True, res=[opt['xres'], opt['yres']])
    # f.savefig(f'{output_folder}/{participant_name}_{recording_no}.png')
    # plt.close(f)

    fix['recname'] = f"{participant_name.lower()}_{recording_no.replace('Recording', '')}"
    big_df = pd.concat([big_df, pd.DataFrame(fix)])

    if i%20 == 0: # output every 20 recordings
        big_df.to_csv('data/temp/i2mc_csv.csv')
big_df.to_csv('data/temp/i2mc_csv.csv')