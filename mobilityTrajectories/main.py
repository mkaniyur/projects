# run on python 3.12.4
# imports
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import csv
import math
from sklearn.cluster import DBSCAN

# =================================
# DATA PROCESSING & DATAFRAME PREPARATION
# =================================

# load data
mobility_individuals = pd.read_csv('mobility_individuals.csv')
mobility_trajectories = pd.read_csv('mobility_trajectories.csv')

# to parallel compute evaluation figures
ground_truth_ids = mobility_individuals["individual_id"].tolist()

# helper function for assigning quadrants
def quadrant(x, y):
    if y <= 5:
        lat = "South"
    else:
        lat = "North"
    if x <= 5:
        long = "west"
    else:
        long = "east"
    return lat + long

# dataframe to store classification results
predictions = pd.DataFrame({'individual_id': pd.Series(dtype='int'),
                   'home_quadrant': pd.Series(dtype='str'),
                   'work_quadrant': pd.Series(dtype='str'),
                   'employment_status': pd.Series(dtype='str')})

# lists to store metrics to visualize performance
home_evaluation = []
work_evaluation = []
fulltime_home_cluster_sizes = []
fulltime_work_cluster_sizes = []
parttime_home_cluster_sizes = []
parttime_work_cluster_sizes = []
unemployed_home_cluster_sizes = []

# =================================
# CLASSIFICATION ALGORITHM
# =================================

# loop through each individual trajectory
for i in range(200):
    # isolate individual location data
    trajectory = mobility_trajectories.loc[mobility_trajectories['individual_id'] == i]
    trajectory_coords = trajectory[["x", "y"]]

    # clustering algorithm to identify any locations with >15 hours/week
    # candidates for work & home
    scan = DBSCAN(eps = 0.2, min_samples = 10, metric='euclidean').fit(trajectory_coords)
    labels = scan.labels_
    clusters = labels.max() + 1

    # individual frequents nowhere besides residence -> unemployed
    if clusters == 1:
        status = 'unemployed'
        indices = np.where(labels == 0)[0].tolist()
        home_coords = trajectory_coords.iloc[indices]
        home_x = home_coords["x"].mean()
        home_y = home_coords["y"].mean()
        home_dist = quadrant(home_x, home_y)
    
    # individual frequents multiple locations -> likely employed
    elif clusters == 2:
        cluster_1_indices = np.where(labels == 0)[0].tolist()
        cluster_2_indices = np.where(labels == 1)[0].tolist()

        # individual likely works <= 30 hours/week -> part-time
        if min(len(cluster_1_indices), len(cluster_2_indices)) <= 30:
            status = 'part_time'
        else:
            status = 'full_time'
        
        # assuming individual spends more time at home than at work
        # to select which cluster is work and which is home
        if len(cluster_1_indices) < len(cluster_2_indices):
            work_coords = trajectory_coords.iloc[cluster_1_indices]
            home_coords = trajectory_coords.iloc[cluster_2_indices]
        else:
            work_coords = trajectory_coords.iloc[cluster_2_indices]
            home_coords = trajectory_coords.iloc[cluster_1_indices]
        work_x = work_coords["x"].mean()
        work_y = work_coords["y"].mean()
        home_x = home_coords["x"].mean()
        home_y = home_coords["y"].mean()
        home_dist = quadrant(home_x, home_y)
        work_dist = quadrant(work_x, work_y)

    # append results to dataframe
    new_row = {'individual_id': i, "home_quadrant": home_dist, "work_quadrant": work_dist, "employment_status": status}
    predictions = pd.concat([predictions, pd.DataFrame([new_row])])

    # if in ground truth, compute differences to evaluate algorithm
    if i in ground_truth_ids:
        # check that clustering algorithm identifies locations/quadrants accurately
        individual = mobility_individuals.loc[mobility_individuals['individual_id'] == i]
        true_status = individual['status'].unique().tolist()[0]
        home = [home_x, home_y]
        work = [work_x, work_y]
        true_home = [individual['home_x'].unique().tolist()[0],
                     individual['home_y'].unique().tolist()[0]]
        true_work = [individual['work_x'].unique().tolist()[0],
                     individual['work_y'].unique().tolist()[0]]
        home_diff = math.dist(home, true_home)
        work_diff = math.dist(work, true_work)
        home_evaluation.append(home_diff)
        work_evaluation.append(work_diff)

        # double check assumptions on working & home hours
        if true_status == 'full_time':
            fulltime_work_cluster_sizes.append(work_coords.shape[0])
            fulltime_home_cluster_sizes.append(home_coords.shape[0])
        elif true_status == 'part_time':
            parttime_work_cluster_sizes.append(work_coords.shape[0])
            parttime_home_cluster_sizes.append(home_coords.shape[0])
        elif true_status == 'unemployed':
            unemployed_home_cluster_sizes.append(home_coords.shape[0])

# write dataframe to csv
predictions.to_csv('mobility_predictions.csv', index=False)

# =================================
# HEURISTIC EVALUATION FIGURES
# =================================

# Figure 1: Validity of selection heuristic for part vs. full-time employment 
fig, ax = plt.subplots()
ax.boxplot([fulltime_work_cluster_sizes, parttime_work_cluster_sizes], labels=["Fulltime Work", "Parttime Work"])
plt.title("Weekly Hours at Work For Part-Time vs. Full-Time Employees")
plt.savefig('WorkHours_PartTime_VS_FullTime.png')

# Figure 2: Validity of assumption that hours at home > hours at work 
fig, ax = plt.subplots()
ax.boxplot([fulltime_work_cluster_sizes, fulltime_home_cluster_sizes], labels=["Fulltime Work", "Fulltime Home"])
plt.title("Weekly Hours at Work vs. at Home for Full-Time Employees")
plt.savefig('HomeHours_VS_WorkHours_FullTime.png')

# Figure 3: Accuracy of home & work locations computed by DBSCAN's clustering
work_evaluation = [i for i in work_evaluation if not np.isnan(i)]
fig, ax = plt.subplots()
ax.boxplot([home_evaluation, work_evaluation], labels=["Home", "Work"])
plt.title("Distance between True and Estimated Home & Work Locations")
plt.savefig('ClusterLocationAccuracy.png')
# show all figures
plt.show()