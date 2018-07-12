## import the required files
# example: import maddm_interface as maddm_interface # local file
#          import madgraph.various.cluster as cluster #MG5 distribution file
# Three types of functionality are allowed in a plugin
#   1. new output mode
#   2. new cluster support
#   3. new interface

import os
import madgraph.iolibs.export_v4 as export_v4
import madgraph.various.banner as banner_mod
import madgraph.iolibs.group_subprocs as group_subprocs
from madgraph.iolibs.files import cp
import madgraph.iolibs.file_writers as writers

from madgraph import MG5DIR
        

pjoin = os.path.join

rootdir = os.path.dirname(__file__)
class ISR_Exporter(export_v4.ProcessExporterFortranMEGroup):
    
    def copy_template(self, model):
        out = super(ISR_Exporter, self).copy_template(model)

        if self.cmd._generate_info:
            # Write the procdef_mg5.dat file with process info
            card_path = pjoin(self.dir_path ,'SubProcesses', \
                                     'procdef_mg5.dat')
            self.write_procdef_mg5(card_path,
                                self.cmd._curr_model['name'],
                                self.cmd._generate_info)

        # We will set lpp to 9 (special mode)
        # and therefore we need to define the function for x1/x2 in dummy
        self.modify_dummy()


    def modify_dummy(self):
        """ """
        
        plugin_dir = os.path.dirname(os.path.realpath( __file__ ))

        # Add dedicated functions in dummy_fct.f
        files = ["dummy_fct.f"]#, "pdg2pdf.f", "reweight.f"]
        remove_list = [['get_dummy_x1_x2']]#,["store_events","write_leshouche"],["setclscales"]]
        for name, to_rm in zip(files, remove_list):
            template = open(pjoin(self.dir_path, "SubProcesses", name),"r").read()
            plugin = open(pjoin(plugin_dir, "Templates",name),"r").read()
            ff = writers.FortranWriter(pjoin(self.dir_path, "SubProcesses", name))
            ff.remove_routine(template, to_rm, formatting=False)
            ff.writelines(plugin, formatting=False)
            ff.close()

        # No need of this one automatic with lpp=9 
        #cp(pjoin(rootdir, 'pdg2pdf.f'), pjoin(self.dir_path, 'Source','PDF'))
        #cp(pjoin(rootdir, 'reweight.f'), pjoin(self.dir_path, 'SubProcesses')) 
        
        filename = pjoin(self.dir_path, 'Cards', 'me5_configuration.txt')
        self.cmd.do_save('options %s' % filename.replace(' ', '\ '), check=False,
                         to_keep={'mg5_path':MG5DIR})
        
        #filename = pjoin(self.dir_path,'Cards', 'proc_card_mg5.dat')
        #self.cmd.do_history(filename)

        return

    #===========================================================================
    #  create the run_card 
    #===========================================================================
    def create_run_card(self, matrix_elements, history):
        """ """

        default = True
        if isinstance(matrix_elements, group_subprocs.SubProcessGroupList):            
            processes = [me.get('processes')  for megroup in matrix_elements 
                                        for me in megroup['matrix_elements']]
        elif matrix_elements:
            processes = [me.get('processes') 
                                 for me in matrix_elements['matrix_elements']]
        else:
            default = False


        run_card = banner_mod.RunCard()

        if default:
            run_card.create_default_for_process(self.proc_characteristic, 
                                                history,
                                                processes)

        run_card['lpp1'] = 9
        run_card['lpp2'] = 9
        run_card['use_syst'] = 'F'
        run_card['dynamical_scale_choice'] = 4

        run_card.write(pjoin(self.dir_path, 'Cards', 'run_card_default.dat'))
        run_card.write(pjoin(self.dir_path, 'Cards', 'run_card.dat'))

        return

    def pass_information_from_cmd(self, cmd):
        """pass information from the command interface to the exporter.
           Please do not modify any object of the interface from the exporter.
        """
        # just point a pointer
        self.cmd = cmd
        return super(ISR_Exporter, self).pass_information_from_cmd(cmd)
        



# 1. Define new output mode
#    example: new_output = {'myformat': MYCLASS}
#    madgraph will then allow the command "output myformat PATH"
#    MYCLASS should inherated of the class madgraph.iolibs.export_v4.VirtualExporter 
new_output = {'EE_ISR': ISR_Exporter}

# 2. Define new way to handle the cluster.
#    example new_cluster = {'mycluster': MYCLUSTERCLASS}
#    allow "set cluster_type mycluster" in madgraph
#    MYCLUSTERCLASS should inherated from madgraph.various.cluster.Cluster
new_cluster = {}


# 3. Define a new interface (allow to add/modify MG5 command)
#    This can be activated via ./bin/mg5_aMC --mode=PLUGINNAME
## Put None if no dedicated command are required
new_interface = None
 
 
########################## CONTROL VARIABLE ####################################
__author__ = 'Cheng Chen, Zhengwei Cui, Gang Li, Qiang Li, Xin Mo, Manqi Ruan, Lei Wang, Qi-Shu Yan'
__email__ = 'qliphy@gmail.com, olivier.mattelaer@uclouvain.be'
__version__ = (1,1,0)
minimal_mg5amcnlo_version = (2,6,0) 
maximal_mg5amcnlo_version = (1000,1000,1000)
latest_validated_version = (2,6,4)
