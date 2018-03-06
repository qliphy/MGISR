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
from madgraph import MG5DIR
        

pjoin = os.path.join

rootdir = os.path.dirname(__file__)
class ISR_Exporter(export_v4.ProcessExporterFortranMEGroup):
    
    def copy_template(self, model):
        out = super(ISR_Exporter, self).copy_template(model)
        cp(pjoin(rootdir, 'genps.f'), pjoin(self.dir_path, 'SubProcesses'))
        cp(pjoin(rootdir, 'pdg2pdf.f'), pjoin(self.dir_path, 'Source','PDF'))
        cp(pjoin(rootdir, 'reweight.f'), pjoin(self.dir_path, 'SubProcesses')) 
        
        filename = pjoin(self.dir_path, 'Cards', 'me5_configuration.txt')
        self.cmd.do_save('options %s' % filename.replace(' ', '\ '), check=False,
                         to_keep={'mg5_path':MG5DIR})
        
        #filename = pjoin(self.dir_path,'Cards', 'proc_card_mg5.dat')
        #self.cmd.do_history(filename)
        if self.cmd._generate_info:
            # Write the procdef_mg5.dat file with process info
            card_path = pjoin(self.dir_path ,'SubProcesses', \
                                     'procdef_mg5.dat')
            self.write_procdef_mg5(card_path,
                                self.cmd._curr_model['name'],
                                self.cmd._generate_info)
        return
        
        
        return out

    #===========================================================================
    #  create the run_card 
    #===========================================================================
    def create_run_card(self, matrix_elements, history):
        """ """

        run_card = banner_mod.RunCard()
        
        
        default=True
        if isinstance(matrix_elements, group_subprocs.SubProcessGroupList):            
            processes = [me.get('processes')  for megroup in matrix_elements 
                                        for me in megroup['matrix_elements']]
        elif matrix_elements:
            processes = [me.get('processes') 
                                 for me in matrix_elements['matrix_elements']]
        else:
            default =False
    
        if default:
            run_card.create_default_for_process(self.proc_characteristic, 
                                                history,
                                                processes)
            # check for beam_id
            beam_id = set()
            for proc in processes:
                for oneproc in proc:
                    for leg in oneproc['legs']:
                        if not leg['state'] and leg['number']==1:
                            beam_id.add(leg['id'])
                            
            if '-11' in beam_id:
                run_card['lpp1'] = 3
                run_card['lpp2'] =-3
            else:
                run_card['lpp1'] = -3
                run_card['lpp2'] =  3
        else:
            run_card['lpp1'] = 3
            run_card['lpp2'] = -3
            
        run_card['use_syst'] = 'F'
        run_card.write(pjoin(self.dir_path, 'Cards', 'run_card_default.dat'))
        run_card.write(pjoin(self.dir_path, 'Cards', 'run_card.dat'))
        
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
__email__ = 'qliphy@gmail.com'
__version__ = (1,0,0)
minimal_mg5amcnlo_version = (2,5,0) 
maximal_mg5amcnlo_version = (1000,1000,1000)
latest_validated_version = (2,5,6)
