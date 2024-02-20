# GeoPIXE

### Repository to host an open-source release of GeoPIXE.

GeoPIXE uses the IDL language and its Eclipse development environment IDLDE. This directory
organization enables IDLDE to run directly using the "Workspace" directory as its workspace.
The .metadata directory, which normally holds config for the IDL projects in Workspace is not
included in this archive. However, documentation is provided for setup of the Eclipse environment
if so desired. At least some minimal setup will be required. It assumes the current IDL 8.8 is used.

See the documentation "**doc/GeoPIXE Software Organization - open.pdf**" for Eclipse setup, an 
overview of the program, its main areas of application for X-ray spectral analysis and imaging 
using PIXE and synchtotron and laboratory X-ray beams, the software users guide, the organization 
of the core software and its extensions (plugins and device objects) and building GeoPIXE and 
its modules.

On Windows, the script "**Example Windows IDLDE 88 Python38 GeoPIXE.bat**" can be adapted to launch 
IDLDE using this workspace and a IDL pref file (idl88-py38.pref), and also assign a default 
python, which is only used in some extensions to GeoPIXE. *NOTE: Edit paths in both the bat and 
pref files*.

On Linux, the bash script "**idlde2**" can be adapted to launch IDLDE using this workspace and a 
IDL pref file (idl-linux.pref). *NOTE: Edit full paths in both the script and pref files*.

Launching the runtime version of IDL to run a compiled version of GeoPIXE is illustrated in the 
script "Example Runtime GeoPIXE.bat". It also shows how GeoPIXE can be used in a workflow to perform
some work. See the documentation "doc/GeoPIXE Software Organization - open.pdf" for details.

### Setting up the Eclipse IDLDE environment

To import all projects, use the “***File->Import->General->Projects from folder or archive***” menu and 
navigate to “Workspace” in the local downloaded GeoPIXE “Workspace” directory as the “Import source” 
folder. Then select all project folders and “Finish”. This will import all projects. See the section 
“Eclipse environment and organization” for more details of the Eclipse environment.

However, this does not import project setting for building, etc. Building can be handled using the 
“builder” PRO, as outlined in the documentation "doc/GeoPIXE Software Organization - open.pdf" 
(see section “Building GeoPIXE”). But there are some settings that must be set now. For just the 
“geopixe” and “Fortran” projects, right click and select “Properties” for each. For the “IDL project 
properties” group, uncheck the option “update IDL path when project is opened or closed”. You can 
also do that for the “Default” project, if you are not using that.

### Obtaining IDL

IDL, the interactive data language, can be obtaned from L3Harris Geospatial. A free "Virtual Machine"
provides runtime support, which can be used for most GeoPIXE functionality by executing the SAV files.

However, some advanced methods, such as parallel processing of large data-sets such as Maia blog data,
make use of features not available in the VM. For these, a runtime licensed version is required. 

If you desire to edit and test IDL pro files, then a full license is required to run IDLDE.
