# GeoPIXE

### Repository to host an open-source release of GeoPIXE.

GeoPIXE uses the IDL language and its Eclipse development environment IDLDE. This directory
organization enables IDLDE to run directly using the "Workspace" directory as its workspace.
The .metadata directory, which normally holds config for the IDL projects in Workspace is not
included in this archive. However, documentation is provided for setup of the Eclipse environment
if so desired. At least some minimal setup will be required. It assumes at least IDL 8.8 is used.

See the documentation "**doc/Getting started with GeoPIXE.pdf**" to get started and
"**doc/GeoPIXE Software Organization - open.pdf**" for Eclipse setup, an 
overview of the program, its main areas of application for X-ray spectral analysis and imaging 
using PIXE and synchtotron and laboratory X-ray beams, the software users guide, the organization 
of the core software and its extensions (plugins and device objects) and building GeoPIXE and 
its modules.

There are two main branches: "main", which is used to hold the latest stable release, and "develop", 
which may be regarded as the 'beta' version with new features under test.

### Install IDL

Running GeoPIXE requires at least IDL runtime support. With IDL installed, but not licensed, you 
can run GeoPIXE using IDL Virtual Machine runtime support. To program in IDL and build GeoPIXE 
source, you will need an IDL license.

### Running GeoPIXE from the compiled 'geopixe' directory

Once IDL is installed, you can run GeoPIXE simply by double clicking on "GeoPIXE.sav" in the
'geopixe' folder within the 'Workspace' tree. It will note the absence of a 'geopixe.conf' file,
but will then create one for you in your [home]/.geopixe directory. For Linux, there is a bash 
script in the 'Workspace/geopixe/bin/' directory to launch GeoPIXE. Just add 'bin' to your path.
After downloading, you will need to enable execution of these scripts (chmod a+x bin/*).

It helps to have a working understanding of the Fundamental Parameter approach as used for 
quantitative analysis in the workflow through GeoPIXE, and some experience with the Demo data. 
See the GeoPIXE Users Guide and the **GeoPIXE Worked Examples PDF** for worked examples with 
step-by-step tips. Both provide examples of the main tasks of fitting spectra to generate the 
Dynamic Analysis (DA) image projection matrix; using this DA matrix to process full-spectral data 
to deconvolute elemental components and project separated elemental images; and exploration and 
processing of the images to first verify their accuracy, including extracting spectra from 
observed features (region shapes or element-element Associations), and make corrections and then 
to explore their content.

### GeoPIXE Demo data

The GeoPIXE Demo data can be downloaded from this DOI:  https://doi.org/10.25919/ff5b-wr11
This uses a simple dir structure, aimed at a single user.

If you plan to host a GeoPIXE analysis workshop of many users, and would like organize the data
on disk to avoid multiple copies of the raw data (but have a separate copy of working files
for each user), then download from this DOI:  https://doi.org/10.25919/3yrz-7x38.
Then check out the notes in the "Read me - Linux.txt" file in this download.

### Opening GeoPIXE using the Eclipse IDLDE environment

On Windows, the script "**Example Windows IDLDE 88 Python38 GeoPIXE.bat**" can be adapted to launch 
IDLDE using this workspace and a IDL pref file (idl88-py38.pref), and also assign a default 
python, which is only used in some extensions to GeoPIXE. *NOTE: Edit paths in both the bat and 
pref files*.

On Linux, the bash script "**idlde2**" can be adapted to launch IDLDE using this workspace and a 
IDL pref file (idl-linux.pref). *NOTE: Edit full paths in both the script and pref files first*.

Launching the runtime version of IDL to run a compiled version of GeoPIXE is illustrated in the 
script "Example Runtime GeoPIXE.bat". It also shows how GeoPIXE can be used in a workflow to perform
some work. See the documentation "doc/GeoPIXE Software Organization - open.pdf" for details.

### Setting up the Eclipse IDLDE environment

To import all projects, use the "***File->Import->General->Projects from folder or archive***" menu and 
navigate to “Workspace” in the local downloaded GeoPIXE "Workspace" directory as the "Import source" 
folder. Then select all project folders and "Finish". This will import all projects. See the section 
“Eclipse environment and organization” for more details of the Eclipse environment.

However, this does not import project setting for building, etc. Building can be handled using the 
"builder" SAV, as outlined in the documentation "doc/GeoPIXE Software Organization - open.pdf" 
(see section “Building GeoPIXE”). But there are some settings that must be set now. For just the 
“geopixe” and “Fortran” projects, right click and select “Properties” for each. For the “IDL project 
properties” group, uncheck the option “update IDL path when project is opened or closed”, so that 
these are never added to the IDL path. If these projects are open, **close** them using the right-click
"Close Project" option. Make sure they remain **closed**. You can also do that for the “Default” project, 
if you are not using that. Check that the paths were set correctly (from your edited "idl-linux.pref" 
file) by using menu "***Window->Preferences***" and select the "IDL" group from the tree on the left. 
Check that the "Initial working directory" points to your 'Workspace/main' path and that "Startup file" 
points to "startup.spro" in this directory.

### Obtaining IDL

IDL, the interactive data language, can be obtaned from L3Harris Geospatial. A free "Virtual Machine"
provides runtime support, which can be used for most GeoPIXE functionality by executing the SAV files.

However, some advanced methods, such as parallel processing of large data-sets such as Maia blog data,
make use of features not available in the VM. For these, a runtime licensed version is required. 

If you desire to edit and test IDL pro files, and to build SAV files, then a full license is required 
to run IDLDE, the IDL Eclipse development environment.
