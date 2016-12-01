import numpy as np
import serialbox as ser

class MyStencil(object):

    def __init__(self):
        self.invocation_count = 0
        self.u = np.random.rand(32, 34, 80)
        self.v = np.random.rand(32, 34)
        
    def run(self, serializer):
        self.my_fance_stage(serializer)
        self.invocation_count += 1

    def my_fance_stage(self, serializer):
        #
        # At each stage we create the savepoints and serialize the input fields, ...
        #
        savepoint_in = ser.Savepoint("MyStencil__in")
        savepoint_in.metainfo.insert("stage_name", "my_fance_stage")
        savepoint_in.metainfo.insert("invocation_count", self.invocation_count)
        savepoint_in.metainfo.insert("stage_id", 0)
        
        serializer.write("u", savepoint_in, self.u)
        serializer.write("v", savepoint_in, self.v)

        #
        # .. run the stage ...
        #    
        self.u += 1
        self.v += 2
        
        #
        # .. and serialize the output fields at the output savepoint.
        # 
        savepoint_out = ser.Savepoint("MyStencil__out")
        savepoint_out.metainfo.insert("stage_name", "my_fance_stage")
        savepoint_out.metainfo.insert("invocation_count", self.invocation_count)
        savepoint_out.metainfo.insert("stage_id", 0)
        
        serializer.write("u", savepoint_out, self.u)
        serializer.write("v", savepoint_out, self.v)


if __name__ == '__main__':
    #
    # Initialize the Serializer and register the stencil in the global meta-information
    #
    serializer = ser.Serializer(ser.OpenModeKind.Write, "./test/", "stencil")
    serializer.global_metainfo.insert("stencils", ["MyStencil"])

    stenncil = MyStencil()
    stenncil.run(serializer)
