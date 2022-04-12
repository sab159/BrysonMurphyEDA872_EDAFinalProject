# BrysonMurphyEDA872_EDAFinalProject
A project for collaborating on the final project for environmental data analytics (EDA) - Spring 2022

This project looks at water use by source and target across states and time. 
Created by Rebecca Murphy & Sophia Bryson for Environmental Data Analytics (ENV872) at Duke University's NSOE in Spring 2022. 


## Summary:

The purpose of this dashboard is to assist water resource planners in visualizing types of water use by political (state or county) or administrative (EPA Regions or USACE Districts and Divisions) boundaries over time. Based on desired inputs of spatial extent and year, dashboard users are able to produce a Sankey Diagram that highlights relevant surface water and groundwater usage by categories. 

## Investigators:

Rebecca Murphy: _rebecca.murphy@duke.edu_ 
Nicholas School of the Environment MEM-WRM

Sophia Bryson: _sophia.bryson@duke.edu_ 
Nicholas School of the Environment MEM-WRM

## Database Information: 

The primary datasets used to generate the dashboard include:

1. USGS Water Use Data: 

State and county water use by source and use with five year resolution. Data was provided by US Geological Survey (USGS), but cleaned and formatted by the Nicholas Institute for Environmental Policy Solutions (NIEPS). 

2. EPA Regions: 

Match file provided by NIEPS. 

3. USACE Divisions and Districts:

Match file provided by NIEPS. 


## Folder structure:

BrysonMurphyEDA872FinalProject:
1. USGSWaterUseSankey
 A. data: This also has a shortcut in the larger project folder. 
   a. Processed
      - Spatial 
      - Water Use
   b. Raw
 B. www: images to insert into the dashboard that highlight administrative regions. 

2. README.md


## Metadata

We did not create a separate Metadata file for this project. Please see above for data information. 

## Scripts and code

1. app.R: contains the script to generate the project's dashboard. 

## Quality assurance/quality control

We went through QA/QC measures in the initial data wrangling process. This primarily included removing incomplete data (rows with N/A's). Data relating to EPA Regions and USACE Divisions and Districts were already QA/QC'ed through NIEPS.   
