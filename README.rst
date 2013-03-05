=================
ACCESS-O (AusCOM)
=================
*These instructions are from the COE CSS CMS website, and not for general use*

ACCESS-O (or AusCOM) is the ocean-ice implementation of the ACCESS climate
model. It consists of the ocean (MOM) and ice (CICE) components coupled through
OASIS and a data-driven atmospheric model (MATM), primarily driven by CORE
atmospheric forcing.

This document provides basic instructions for building and running standard
CORE-forced experiments.

*Note: These instructions assume that the user has access to the v45 project.
Contact CMS if you do not belong to this project.*


Installing AusCOM
=================
First locate an appropriate directory from where you can run AusCOM (usually
``/short``) and create a directory to hold your source code, e.g.:

.. code:: bash

   mkdir -p /short/v45/${USER}/projects     # Or any directory

where ``${USER}`` is your username.

*Note: Do not use ``/short/v45/${USER}/auscom``, since model output is stored
in this directory.*

Next, clone a copy of the repository:

.. code:: bash

   cd /short/v45/${USER}/projects
   git clone http://github.com/coecms/auscom.git

Once this complete, run the build script:

.. code:: bash

   cd auscom
   ./build_auscom.sh

A full build of all AusCOM components will usually take about an hour.

Once installation is complete, run the ``link_input.sh`` script to create
symbolic links to the input forcing fields:

.. code:: bash

   ./link_input.sh

The original copies of these files are stored in ``/g/data/v45/auscom``. If you
want to modify the forcing fields and configuration files, then you will need
to create new symbolic links to your own directories. *Contact CMS if you
require any assistance.*

You should now be ready to submit an AusCOM experiment.

However, if you ever need to delete the executables and rebuild AusCOM, there
is a script named ``clean_auscom.sh`` script which will delete any compiled
binaries:

.. code:: bash

   ./clean_auscom.sh


Running AusCOM
==============
The default experiment ``cnyf2-sw1`` is located with the repository. To run the
experiment, move to its directory and run the provided script:

.. code:: bash

   cd exp/cnyf2-sw1
   qsub run_auscom.sh

This will run for one model year, and will take approximately 3 hours.

Model output can be accessed in the ``output/cnyf2-sw1/history`` directory.

*This is a symbolic link to the actual data, located at
/short/v45/${USER}/auscom/OUTPUT*


Configuring AusCOM
==================
AusCOM is configured to run a series of self-resubmitting one-year simulations.

The total duration is set within the ``run_auscom.sh`` script. In this part of
this script (line 135):

.. code:: bash

   # Initial and final date of the experiment
   if [[ $DEBUG = "yes" ]]; then
       iniyear=1;  finalyear=1;        typeset -Z4 iniyear  finalyear
       inimonth=1; finalmonth=1;       typeset -Z2 inimonth finalmonth
       iniday=1;   finalday=4;         typeset -Z2 iniday   finalday
   else
       iniyear=1;  finalyear=5;        typeset -Z4 iniyear  finalyear
       inimonth=1; finalmonth=12;      typeset -Z2 inimonth finalmonth
       iniday=1;   finalday=31;        typeset -Z2 iniday   finalday
   fi

Under the ``else`` block (non-debug mode), replace ``finalyear`` with the
numbers of years that you want to run. (Set as five years in this example).

To change the duration of a single run, locate this code block (line 146):

.. code:: bash

   # Duration of this run (maybe the most often visited place for test/short runs):
   if [[ $DEBUG = "yes" ]]; then
       nyear=0         # number of years (ALWAYS 0 ! change nmonth etc...)
       nmonth=0        # number of months
       nday=1          # number of days
   else
       nyear=0         # number of years (ALWAYS 0 ! change nmonth etc...)
       nmonth=12       # number of months
       nday=0          # number of days
   fi

and replace ``nmonth`` and ``ndays`` of the ``else`` block with the desired
duration.

Standard PBS configuration also applies to the script header (beginning on line
35 of ``run_auscom.sh``). Most have been pre-configured for the standard
one-year experiment, but users may need to change the following:

* ``#PBS -P v45``: Deduct CPU hours from project ``v45``
* ``#PBS -W group_list=v45``: New files belong to the ``v45`` group
* ``#PBS -l walltime=4:00:00``: Submit a four-hour job

If you need further information about model configuration, contact CMS.
