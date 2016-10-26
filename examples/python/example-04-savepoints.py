#!/usr/bin/python3
# -*- coding: utf-8 -*-
##===-----------------------------------------------------------------------------*- Python -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license.
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## This example demonstrates the different ways of accessing savepoints of a Serializer in the 
## Python interface. As input we will use real-world meta-data produced by the Fortran dynamical 
## core of COSMO with an older version of serialbox.
##
##===------------------------------------------------------------------------------------------===##

#
# First, we have to make sure Python finds the Serialbox module. Alternatively, you can also set the 
# environment variable PYTHONPATH.
#
import os
import sys
sys.path.append(os.path.dirname(__file__) + '../../src/serialbox-python')
sys.path.append(os.path.dirname(__file__) + '../../python')

#
# Import Serialbox
#
import serialbox as ser

def main():
    #
    # Initialize the Serializer with old meta-data ('./Field.json'). The meta-data will be upgrade 
    # and converted on on the fly. If you are curious, you can turn on the logging 
    # (ser.Loggging.enable()) to see what's exactly happening. If everything goes well, two new 
    # files `MetaData-Field.json` and `ArchiveMetaData-Field.json` will be created such that we can 
    # avoid an upgrade in the future.
    #
    serializer = ser.Serializer(ser.OpenModeKind.Read, ".", "Field", "Binary")

    #
    # The canonical way of accessing the Savepoints is to directly access the vector of Savepoints. 
    # Note that the order of the Savepoints is the order in which they were inserted.  
    #
    savepoint_list = serializer.savepoint_list()
    print("There are", len(savepoint_list), "savepoints registered.")

    #
    # Savepoints are primarily identified by their `name` and further distinguished by their 
    # `meta_info` but they are unique within the Serializer. For example to access all savepoints 
    # with the name `CoriolisUnittest-in` ...
    #
    savepoint_collection = serializer.savepoint['CoriolisUnittest.Apply-in']
    
    #
    # ... to get the list of savepoints back, call the member method `savepoints()`. Note that the 
    # order is still preserved.
    #
    print("There are", len(savepoint_collection.savepoints()), 
          "savepoints named 'CoriolisUnittest.Apply-in':")
    for sp in savepoint_collection.savepoints():
      print("  ", sp)
      
    #
    # Most of the time you will be interested in a particular Savepoint. This requires you to 
    # specify the meta-information key=value pairs. There are two ways of doing this. But first, 
    # recall that there is NO ordering in the  meta-information, hence it does not matter in which 
    # sequence you specify them! To access the savepoint with LargeTimeStep=1 of 
    # `CoriolisUnittest.Apply-in` ...
    #   
    savepoint = serializer.savepoint['CoriolisUnittest.Apply-in']['LargeTimeStep'][1]

    #
    # The second case uses the member access of python. Note that this case has some downsides if 
    # you don't use savepoint names or keys which can be mapped to valid Python identifiers. 
    # For example instead of using a '.' you can use '_', same goes for '-' (See the documentation 
    # of Serializer.savepoint for further information). Thus we can also write ...  
    #    
    savepoint = serializer.savepoint.CoriolisUnittest_Apply_in.LargeTimeStep[1]

    #    
    # To get the actual savepoint, use the member method `as_savepoint()`. The write/read methods 
    # of the Serializer will do this automatically if they detect you passed in a savepoint 
    # collection.
    #
    print("The savepoint of intrest is:", savepoint.as_savepoint())

    #
    # This last example shows the two approaches in a a more advanced query ...
    #
    sp_1 = serializer.savepoint['FastWavesSCUnittest.UV-in']['LargeTimeStep'][1]['RKStageNumber'][2]['SmallTimeStep'][1]
    
    sp_2 = serializer.savepoint.FastWavesSCUnittest_UV_in.LargeTimeStep[1].RKStageNumber[2].SmallTimeStep[1]
    
    assert(sp_1.as_savepoint() == sp_2.as_savepoint())
    
    #
    # ... or you can mix the two approaches to bypass any problems with the identifiers.
    #
    sp_3 = serializer.savepoint['FastWavesSCUnittest.UV-in'].LargeTimeStep[1].RKStageNumber[2].SmallTimeStep[1]
    
    assert(sp_1.as_savepoint() == sp_3.as_savepoint())
    
    #
    # Remember, the order in which you specify the meta-information does not matter ...
    #
    sp_1 = serializer.savepoint['FastWavesSCUnittest.UV-in'].LargeTimeStep[1].RKStageNumber[2].SmallTimeStep[1]
    sp_2 = serializer.savepoint['FastWavesSCUnittest.UV-in'].RKStageNumber[2].SmallTimeStep[1].LargeTimeStep[1]

    assert(sp_1.as_savepoint() == sp_2.as_savepoint())

if __name__ == '__main__':
    main()

